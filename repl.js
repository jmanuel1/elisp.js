const readline = require('readline');
const process = require('process');
const path = require('path');
const fs = require('fs');
const zlib = require('zlib');

const ty = require('./elisp/types');
const parser = require('./elisp/parser');
const translate = require('./elisp/translate');
const elisp = require('./elisp/elisp');

var env = new elisp.Environment();

async function main() {
  /* put special forms in the env */
  for (let form of translate.special_forms) {
    let dummy = ty.subr(form, [], function() {
      throw new Error("Congratulations! You've just called a dummy, which should never happen");
    });
    await env.fset(form, dummy);
  }

  await env.fset('load', ty.subr('load', [], function(args) {
    if (!ty.is_string(args[0]))
      throw new ty.LispError('Wrong type argument: stringp, ' + args[0].to_string(), 'error', this);

    let tryFileName = (fname) => {
      let tryfile = (f) => {
        try { fs.accessSync(f, fs.constants.R_OK); return true; }
        catch (e) { return false };
      };
      if (tryfile(fname)) return fname;
      if (tryfile(fname + '.el')) return fname + '.el';
      if (tryfile(fname + '.gz')) return fname + '.gz';
      if (tryfile(fname + '.el.gz')) return fname + '.el.gz';
    };

    let filename = args[0].to_js();
    let fullpath;
    if (path.isAbsolute(filename)) {
      fullpath = tryFileName(filename);
    } else if (this.is_bound('load-path')) {
      let loadpath = this.get('load-path');
      for (let p = loadpath; !p.is_false; p = p.tl) {
        let base = path.resolve(p.hd.is_false ? process.cwd() : p.hd.to_js());
        fullpath = tryFileName(path.join(base, filename));
        if (fullpath)
          break;
      }
    } else
      throw new ty.LispError('`load-path` is not set', 'error', this);
    if (!fullpath)
      throw new ty.LispError('Cannot open load file: ' + filename, 'error', this);

    let file = fs.readFileSync(fullpath);
    if (fullpath.endsWith('.gz'))
      file = zlib.gunzipSync(file);
    let text = file.toString('utf-8');

    let forms = elisp.readtop(text, fullpath);
    forms.forEach((form) => {
      elisp.eval_lisp(form, this)
    });

    return ty.t;
  }));

  /*
   *  prelude
   */
  let prelude = [
  `(fset 'defun
    '(macro lambda (name args body)
        (list 'fset (list 'quote name) (list 'lambda args body))))`,
  `(setq *jsdebug* nil)`,
  ];
  prelude.forEach((stmt) => elisp.eval_text(stmt, env));


  /*
   *  Arguments
   */

  let argv = process.argv;
  switch (argv[argv.length - 1]) {
    case '--help':
      console.error('Elisp to JS translator');
      process.exit();
  };

  /*
   *  completer
   */
  function completer(line) {
    let par = line.lastIndexOf('(');
    let gap = line.lastIndexOf(' ');
    let start = ((par > gap) ? par : gap ) + 1;

    let key = line.slice(start);
    if (key.length == 0)
      return [[], line];

    let hits = [];
    for (let name in ((par > gap ? env.fs : env.vs))) {
      if (name.startsWith(key))
        hits.push(name);
    }

    return [hits, key];
  }

  /*
   *  repl loop
   */
  let rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: 'elisp> ',
    completer: completer
  });

  rl.prompt();

  let evaled_prelude = false;

  rl.on('line', async (line) => {
    line = line.trim();
    if (!line) return;

    try {
      if (!evaled_prelude) await elisp.eval_prelude(env);
      evaled_prelude = true;
      let result = await elisp.eval_text(line, env);
      console.log(result);
    } catch (e) {
      let jsdebug = env.has_jsdebug();
      if (e instanceof ty.LispError && !jsdebug)
        console.error(e.name + ': ' + e.message);
      else
        console.error(e.stack);
    };

    rl.prompt();
  });
  rl.on('close', () => {
    console.log('');
  });
}

main()
