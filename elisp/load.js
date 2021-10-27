const {promises: fs} = require('fs');
const path = require('path');

// TODO: Borrow implementation from repl.js
module.exports = async function load(filename, {isAbsolutePath} = {isAbsolutePath: false}, env) {
  const {readtop, eval_lisp} = require('./elisp');
  const loadPaths = ['../../emacs/lisp/emacs-lisp/', '../../emacs/lisp/'];
  for (let loadPath of loadPaths) {
    let pathToFile = path.join(__dirname, loadPath, filename);
    if (isAbsolutePath) {
      pathToFile = filename;
    }
    let contents;
    try {
      contents = await fs.readFile(pathToFile, 'utf-8');
    } catch (err) {
      continue;
    }
    for (let expr of readtop(contents)) {
      await eval_lisp(expr, env);
    }
    return;
  }
  throw new types.LispError(`failed to load ${filename}`, 'error', env);
}
