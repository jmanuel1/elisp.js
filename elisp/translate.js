'use strict';

const ty = require('./types');
const Environment = require('./environment').Environment;

/*
 *  Static contexts:
 *  lexical variable scopes
 */
function Context(prev, bound) {
  this.prev = prev;

  this.vars = {};

  this.freefuns = {};
  this.freevars = {};
  this.bound = {};
  bound.forEach((v) => { this.bound[v] = true; });

  this.counter = prev ? prev.counter : 0;
  this.jsvars = {};

  this.is_fun = {};
}

Context.prototype.jsvar = function(name) {
  /* walks all contexts, returns the js variable for name or null */
  for (let ctx = this; ctx; ctx = ctx.prev) {
    if (name in ctx.vars)
      return ctx.vars[name];
  }
  return null;
}

Context.prototype.addvar = function(name, is_fun) {
  /* gets an existing js variable or creates new */
  let jsname;
  if (jsname = this.jsvar(name))
    return jsname;

  ++this.counter;
  jsname = (is_fun ? 'f' : 'v') + this.counter;

  /* if it's a free variable, save it */
  this.checkFree(name, is_fun);

  this.vars[name] = jsname;
  this.jsvars[jsname] = name;

  if (is_fun)
    this.is_fun[name] = true;
  return jsname;
}
Context.prototype.addfun = function(name) {
  return this.addvar(name, true);
}

Context.prototype.checkFree = function(name, is_fun) {
  /*
   * check if the name is bound;
   * otherwise save it as free in the outermost context
   */
  for (let ctx = this; ctx; ctx = ctx.prev) {
    if (name in ctx.bound)
      return false;
    if (!ctx.prev) {
      if (is_fun)
        ctx.freefuns[name] = true;
      else
        ctx.freevars[name] = true;
      return true;
    }
  }
}

Context.prototype.to_jsstring = function(env) {
  let jscode = [];
  for (let v in this.vars) {
    let jsvar = this.vars[v];
    let getter = this.is_fun[v] ? 'fun' : 'var_';
    jscode.push(`let ${jsvar} = ${env.to_jsstring()}.${getter}('${v}')`);
  }
  if (jscode.length)
    jscode = jscode.join(";\n") + ";\n";
  return jscode;
}


/*
 *    Translation
 */

function translate_get(name, env, ctx) {
  if (ctx) {
    let jsname = ctx.addvar(name);
    return `Promise.resolve(${jsname}.get())`;
  }
  return `Promise.resolve(${env.to_jsstring()}.get('${name}'))`;
}

function translate_fget(name, env, ctx) {
  if (ctx) {
    let jsname = ctx.addfun(name);
    return `Promise.resolve(${jsname}.get())`;
  }
  return `Promise.resolve(${env.to_jsstring()}.fget('${name}'))`;
}

async function translate_let(args, env, ctx) {
  /* sanity checks */
  if (args.is_false)
    throw new ty.LispError('Wrong number of arguments: let', 0);
  if (!ty.is_cons(args))
    throw new ty.LispError('Wrong type argument: listp, ' + args.to_jsstring());

  let varlist = args.hd;
  let body = args.tl;
  if (!ty.is_list(body))
    throw new ty.LispError('Wrong type argument: listp, ' + body.to_jsstring());

  /* body preprocessing */
  if (body.is_false)
    body = ty.nil
  else if (body.tl.is_false)
    body = body.hd;
  else
    body = ty.cons(ty.interned_symbol('progn'), body);

  if (!ty.is_sequence(varlist))
    throw new ty.LispError('Wrong type of argument: sequencep, 2');
  if (varlist.is_false)
    /* (let () <body>) */
    return translate_expr(body, env, ctx);

  let names = [];
  let values = [];
  let errors = [];
  console.debug('varlist', varlist);
  for (let binding of varlist.to_array()) {
    if (ty.is_symbol(binding)) {
      names.push(binding.to_string());
      values.push(ty.nil.to_jsstring());
    } else if (ty.is_cons(binding)) {
      let name = binding.hd;
      binding = binding.tl;
      let jsval = await translate_expr(binding.hd || ty.nil, env, ctx);
      binding = binding.tl;
      if (binding && !binding.is_false) {
        let msg = "'let' bindings can have only one value-form: " + name.to_string();
        errors.push(msg);
      } else if (!ty.is_symbol(name)) {
        errors.push("Wrong type argument: symbolp, " + name.to_string());
      } else if (name.is_selfevaluating()) {
        let msg = "Attempt to set a constant symbol: " + name.to_string();
        errors.push(msg);
      } else {
        names.push(name.to_string());
        values.push(jsval);
      }
    } else
      errors.push('Wrong type argument: listp, ' + binding.to_string());
  }

  if (errors.length) {
    return `(async () => {
      throw new ty.LispError("${errors[0]}");
    })()`;
  }


  let ctx1 = new Context(ctx, names);
  let jscode = body ? await translate_expr(body, env, ctx1) : ty.nil.to_jsstring();
  let jsctx = ctx1.to_jsstring(env);

  let jsenv = env.to_jsstring();
  let jsnames = names.map((n) => '`'+n+'`').join(', ');
  let jsvalues = values.join(', ');
  return `(async () => {
    ${jsenv}.push([${jsnames}], await Promise.all([${jsvalues}]));
    ${jsctx} let result = await Promise.resolve(${jscode});
    ${jsenv}.pop([${jsnames}]);
    return result;
  })()`;
}

async function translate_let_star(args, env, ctx) {
  /* sanity checks */
  if (args.is_false)
    throw new ty.LispError('Wrong number of arguments: let', 0);
  if (!ty.is_cons(args))
    throw new ty.LispError('Wrong type argument: listp, ' + args.to_jsstring());

  let varlist = args.hd;
  let body = args.tl;
  if (!ty.is_list(body))
    throw new ty.LispError('Wrong type argument: listp, ' + body.to_jsstring());

  /* body preprocessing */
  if (body.is_false)
    body = ty.nil
  else if (body.tl.is_false)
    body = body.hd;
  else
    body = ty.cons(ty.interned_symbol('progn'), body);

  if (!ty.is_sequence(varlist))
    throw new ty.LispError('Wrong type of argument: sequencep, 2');
  if (varlist.is_false)
    /* (let () <body>) */
    return translate_expr(body, env, ctx);

  let names = [];
  let values = [];
  let errors = [];
  console.debug('varlist', varlist);
  for (let binding of varlist.to_array()) {
    if (ty.is_symbol(binding)) {
      names.push(binding.to_string());
      values.push(ty.nil.to_jsstring());
    } else if (ty.is_cons(binding)) {
      let name = binding.hd;
      binding = binding.tl;
      let jsval = await translate_expr(binding.hd || ty.nil, env, ctx);
      binding = binding.tl;
      if (binding && !binding.is_false) {
        let msg = "'let' bindings can have only one value-form: " + name.to_string();
        errors.push(msg);
      } else if (!ty.is_symbol(name)) {
        errors.push("Wrong type argument: symbolp, " + name.to_string());
      } else if (name.is_selfevaluating()) {
        let msg = "Attempt to set a constant symbol: " + name.to_string();
        errors.push(msg);
      } else {
        names.push(name.to_string());
        values.push(jsval);
      }
    } else
      errors.push('Wrong type argument: listp, ' + binding.to_string());
  }

  if (errors.length) {
    return `(async () => {
      throw new ty.LispError("${errors[0]}");
    })()`;
  }


  let ctx1 = new Context(ctx, names);
  let jscode = body ? await translate_expr(body, env, ctx1) : ty.nil.to_jsstring();
  let jsctx = ctx1.to_jsstring(env);

  let jsenv = env.to_jsstring();
  let jsnames = names.map((n) => '`'+n+'`').join(', ');
  let jsvalues = values.join(', ');
  let bindings = [];
  for (let i = 0; i < names.length; i++) {
    bindings.push(`${jsenv}.push([\`${names[i]}\`], await Promise.all([${values[i]}]));`);
  }
  return `(async () => {
    ${bindings.join('\n')}
    ${jsctx} let result = await Promise.resolve(${jscode});
    ${jsenv}.pop([${jsnames}]);
    return result;
  })()`;
}

async function translate_lambda(args, env) {
  let error = (msg, tag) => {
    return `Promise.resolve(ty.lambda([], ty.list([ty.interned_symbol('error'), ty.string('${msg}')]))) `
  };
  if (args.is_false)
    return error("Invalid function: (lambda)");
  if (!ty.is_list(args))
    return error('Wrong type argument: listp, 1');

  let repr = ty.cons(ty.interned_symbol('lambda'), args);
  let body = args.tl || ty.nil;
  let argv = args.hd || ty.nil;
  if (!ty.is_list(argv))
    return error(`Invalid function: ${repr.to_string()}`);

  let argspec = ty.parse_argspec(argv);
  if (!argspec)
    return error(`Invalid function: ${repr.to_string()}`);
  argspec = argspec.map((arg) => '`' + arg + '`');
  argspec = '[' + argspec.join(', ') + ']';

  /* existing macros? */
  const newBody = [];
  if (ty.is_list(body)) {
    for (let form of body.to_array()) {
      const sym = form.hd;
      if (ty.is_list(form) && !form.is_false && ty.is_symbol(sym) && env.is_fbound(sym.to_string())) {
        let f = env.fget(sym.to_string(), true);
        if (ty.is_macro(f)) {
          let expanded = await f.macroexpand(form.tl, env);
          newBody.push(expanded);
          continue;
        }
      }
      newBody.push(form);
    }
    body = ty.list(newBody);
  } else {
    return error(`Invalid function: ${repr.to_string()}`);
  }

  return `Promise.resolve(ty.lambda(${argspec}, ${body.to_jsstring()}))`;
}

let specials = {
  'let': translate_let,
  'let*': translate_let_star,
  'lambda': translate_lambda,

  'quote': function(args) {
    if (args.is_false || !args.tl.is_false)
      throw new ty.LispError('Wrong number of arguments: quote, ' + args.seqlen());

    let what = args.hd;
    return `Promise.resolve(${what.to_jsstring()})`;
  },

  'setq': async function(args, env, ctx) {
    args = args.to_array();
    console.debug('setq', args);
    if (args.length % 2)
      throw new ty.LispError('Wrong number of arguments: setq, ' + args.length);
    if (args.length == 0) {
      return 'Promise.resolve(ty.nil)';
    }

    if (args.length == 2) {
      let [name, value] = args;
      if (!ty.is_symbol(name))
        throw new ty.LispError('Wrong type argument: symbolp, ' + name.to_string());
      if (name.is_selfevaluating())
        throw new ty.LispError('Attempt to set a constant symbol: ' + name.to_string());
      name = name.to_string();

      if (ctx) {
        let jsvar = ctx.addvar(name);
        let jsval = await translate_expr(value, env, ctx);
        return `${jsval}.then(val => ${jsvar}.set(val))`;
      }
      let jsval = await translate_expr(value, env, ctx);
      return `${jsval}.then(val => ${env.to_jsstring()}.set('${name}', val))`;
    }

    let pairs = [];
    for (let i = 0; i < args.length; i += 2) {
      let name = args[i];
      let value = args[i+1];

      if (!ty.is_symbol(name))
        throw new ty.LispError('Wrong type argument: symbolp, ' + name.to_string());
      if (name.is_selfevaluating())
        throw new ty.LispError('Attempt to set a constant symbol: ' + name.to_string());
      name = name.to_string();

      if (ctx) ctx.checkFree(name);

      let jsval = await translate_expr(value, env, ctx);

      pairs.push("'" + name + "'");
      pairs.push(`await ${jsval}`);
    }
    return '(async () => ' + env.to_jsstring() + ".set(" + pairs.join(", ") + "))()";
  },

  'if': async function(args, env, ctx) {
    args = args.to_array();
    if (args.length != 3)
      throw new ty.LispError('Wrong number of arguments: if, ' + args.length);

    let cond = await translate_expr(args[0], env);
    let thenb = await translate_expr(args[1], env);
    let elseb = await translate_expr(args[2], env);

    return `Promise.all([${cond}, ${thenb}, ${elseb}]).then(([cond, thenb, elseb]) => (!(cond).is_false ? (thenb) : (elseb))) `;
  },

  'progn': async function(args, env, ctx) {
    if (args.is_false)
      return ty.nil.to_jsstring();
    args = args.to_array();
    let last = args.pop();

    let stmts = [];
    for (let arg of args) {
      stmts.push('await ' + await translate_expr(arg, env, ctx));
    }
    stmts.push('return ' + await translate_expr(last, env, ctx) + ';\n');

    return '(async () => { ' + stmts.join(';\n') + '})()';
  },

  'while': async function(args, env, ctx) {
    if (args.is_false)
      throw new ty.LispError("Wrong number of arguments: while, 0");
    let condition = args.hd;
    let body = args.tl;

    condition = await translate_expr(condition, env, ctx);
    if (body.is_false) {
      body = ''
    } else if (body.tl.is_false) {
      body = await translate_expr(body.hd, env, ctx);
    } else {
      body = ty.cons(ty.interned_symbol('progn'), body);
      body = await translate_expr(body, env, ctx);
    }
    return `(async () => {
      while (!(await ${condition}).is_false) {
        await ${body}
      };
      return ty.nil;
    })()`;
  },

  'condition-case': async function(args, env, ctx) {
    args = args.to_array();
    let variable;
    if (args[0].is_false) {
      variable = 'nil';
    } else {
      variable = args[0].to_string();
    }
    // console.debug('protected_form', args[1], ty.list(args[1]));
    const protected_form = await translate_expr(args[1], env, ctx);
    const handlers = await Promise.all(args.slice(2).map(async handler => {
      let conditions;
      if (ty.is_symbol(handler.hd)) {
        conditions = [handler.hd.to_string()];
      } else {
        conditions = handler.hd.to_array().map(cond => cond.to_string());
      }
      const body = await specials.progn(handler.tl, env, ctx);
      return {conditions, body};
    }));
    return `(async () => {
      try {
        return await ${protected_form};
      } catch (error) {
        if (!(error instanceof ty.LispError)) {
          throw error;
        }
        const handlers = ${JSON.stringify(handlers)};
        for (let {conditions, body} of handlers) {
          if (conditions.includes(error.tag)) {
            const error_description = ty.cons(ty.interned_symbol(error.tag), ty.list([ty.string(error.message)]));
            ${env.to_jsstring()}.push([${JSON.stringify(variable)}], [error_description]);
            const result = await eval(body);
            ${env.to_jsstring()}.pop([${JSON.stringify(variable)}]);
            return result;
          }
        }
        throw error;
      }
    })()`;
  }
};


async function translate_expr(input, env, ctx) {
  if (input.is_false) {
    /* nil */
    return `Promise.resolve(${ty.nil.to_jsstring()})`;
  }
  if (ty.is_cons(input)) {
    /* a form */
    let hd = input.hd;
    let args = input.tl;

    let callable;
    if (ty.is_symbol(hd)) {
      let args = input.tl;
      let sym = hd.to_string();

      /* (<special form> ...) */
      if (sym in specials)
        return (specials[sym])(args, env, ctx);

      /* macro? */
      if (env.is_fbound(sym)) {
        let f = env.fget(sym, true);
        if (ty.is_macro(f)) {
          let expanded = await f.macroexpand(args, env);
          return translate_expr(expanded, env, ctx);
        }
      }

      /* (<func name> ...) */
      callable = translate_fget(sym, env, ctx);
    } else if (ty.is_list(hd)) {
      /* ((some form) ...) */
      callable = await translate_expr(hd, env, ctx);
    } else {
      /* (:wrongtype ...) */
      callable = `Promise.resolve(ty.subr('#<error>', [], (() => { throw new ty.LispError('Invalid function: ${hd.to_string()}'); })))`;
    }

    let jsenv = env.to_jsstring();
    let jsargs = [];
    for (let item of args.to_array()) {
      let val = await translate_expr(item, env, ctx);
      jsargs.push(`Promise.resolve(${val})`);
    }
    jsargs = 'Promise.all([' + jsargs.join(', ') + '])';

    return `Promise.resolve(${callable}).then(async f => f.fcall(await ${jsargs}, ${jsenv})) `;
  }

  if (ty.is_symbol(input)) {
    if (input.is_selfevaluating())
      return `Promise.resolve(${input.to_jsstring()})`;
    return translate_get(input.to_string(), env, ctx);
  }
  if (ty.is_atom(input)) {
    console.debug('input', input);
    return `Promise.resolve(${input.to_jsstring()})`;
  }
  throw new Error('Failed to translate: ' + input.to_string());
};

/*
 *    Exports
 */
exports.expr = async (input, env) => {
  env = env || new Environment();
  return translate_expr(input, env);
};

exports.lambda = async (args, body, env) => {
  let ctx = new Context(null, args);

  let jsbody = await translate_expr(body, env, ctx);
  let jsctx = ctx.to_jsstring(env);

  /*
  let freevars = Object.keys(ctx.freevars);
  let freefuns = Object.keys(ctx.freefuns);
  console.error('### lambda: freefuns = ' + freefuns.join(','));
  console.error('### lambda: freevars = ' + freevars.join(','));
  */

  return `(async () => { ${jsctx} return ${jsbody}; })`;
};

exports.special_forms = Object.keys(specials);
