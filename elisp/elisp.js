'use strict';

const ty = require('./types.js');
const parser = require('./parser');
const translate = require('./translate');
const path = require('path');

const Environment = require('./environment').Environment;

async function fcall(args, env) {
  /* `this` is LispFun, do not call otherwise */
  // TODO: Combine argument logic with subr argument validation
  let expected_args_length = this.args.length;
  const optional_start_index = this.args.indexOf('&optional');
  if (optional_start_index > -1) {
    expected_args_length = optional_start_index;
    if (args.length < expected_args_length) {
      throw new ty.LispError('Wrong number of arguments: ' + this.to_string() + ', ' + args.length);
    }
    let optional_arguments = args.slice(optional_start_index);
    const nil_arguments = new Array(this.args.length - 1 - optional_arguments.length);
    nil_arguments.fill(ty.nil);
    optional_arguments = optional_arguments.concat(nil_arguments);
    args = args.concat(optional_arguments);
  }
  if (args.length != this.args.length)
    throw new ty.LispError('Wrong number of arguments: ' + this.to_string() + ', ' + args.length);

  let body = this.body;
  if (body.is_false) {
    // do nothing, it must evaluate to nil
  } else if (body.tl.is_false) {
    // single form, extract
    body = body.hd;
  } else {
    // mutliple forms, prepend `progn`
    body = ty.cons(ty.symbol('progn'), body);
  }

  this.jscode = await translate.lambda(this.args, body, env);
  this.func = eval(this.jscode);

  try {
    env.push.call(env, this.args, args);
    var result = await Promise.resolve(this.func());
  } finally {
    env.pop.call(env, this.args);
  }
  return result;
}

async function eval_lisp(expr, env) {
  env = env || new Environment('env');

  let result;
  let saved_env = global[env.name];
  try {
    global[env.name] = env;
    let jscode = await translate.expr(expr, env);
    console.debug('jscode', jscode);
    result = await Promise.resolve(eval(jscode));
  } finally {
    global[env.name] = saved_env;
  }

  return result;
}

async function eval_text(input, env) {
  let expr = parser.read(input);
  let result = await eval_lisp(expr, env);
  return result.to_string();
}

async function eval_prelude(env) {
  const load = require('./load');
  await load.call(env, path.join(__dirname, 'prelude.el'), {isAbsolutePath: true});
}

/*
 *  Exports
 */
exports.eval_text = eval_text;
exports.eval_lisp = eval_lisp;
exports.eval_prelude = eval_prelude;

exports.fcall = fcall;
exports.read = parser.read;
exports.readtop = parser.readtop;
exports.Environment = Environment;
