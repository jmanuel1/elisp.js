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
  const has_rest = this.args.length >= 2 && this.args[this.args.length - 2] === '&rest';
  if (optional_start_index > -1) {
    expected_args_length = optional_start_index;
    if (args.length < expected_args_length) {
      throw new ty.LispError('Wrong number of arguments: ' + this.to_string() + ', ' + args.length);
    }
    let optional_argument_count;
    if (has_rest) {
      optional_argument_count = this.args.length - 2 - (optional_start_index + 1);
    } else {
      optional_argument_count = this.args.length - (optional_start_index + 1);
    }
    let optional_arguments = args.slice(optional_start_index, optional_argument_count + optional_start_index);
    const nil_arguments = new Array(this.args.length - 1 - optional_arguments.length);
    nil_arguments.fill(ty.nil);
    args = args.concat(nil_arguments);
  }
  let new_args;
  let previous_args_end_index = this.args.length - 2;
  if (optional_start_index > -1) {
    previous_args_end_index--;
  }
  if (has_rest) {
    new_args = args.slice(0, previous_args_end_index);
    new_args.push(ty.list(args.slice(previous_args_end_index)));
  } else {
    new_args = args;
  }
  if (optional_start_index < 0 && !has_rest && args.length != this.args.length)
    throw new ty.LispError('Wrong number of arguments: ' + this.to_string() + ', ' + args.length);

  const new_argspec = this.args.filter(arg => !['&optional', '&rest'].includes(arg));

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

  this.jscode = await translate.lambda(new_argspec, body, env);
  this.func = eval(this.jscode);

  try {
    env.push.call(env, new_argspec, new_args);
    var result = await Promise.resolve(this.func());
  } finally {
    env.pop.call(env, new_argspec);
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
