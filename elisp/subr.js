'use strict';

const ty = require('./types');
const translate = require('./translate');
const util = require('util');
var elisp;

let subroutines_registry = {};

/*
 * `args` : argument validators:
 *    [] - any number of any arguments;
 *    [<mandatory] - list of mandatory validators,
 *      e.g `[[ty.symbol, ty.any]]` - two variables, the first is a symbol;
 *    [<mandatory>, <optional>] - list of mandatory and optional validators;
 *    [<mandatory>, <optional>, <rest>]
 *      e.g. `[[ty.string], [], ty.any]`: one mandatory and many rest variables;
 *      e.g. `[[], [], ty.is_number]`: 0 or more numbers;
 *    Validators can be custom: [[], [], (arg, num) => ...];
 */
/* `func` : has the environment as `this` */
function define_subr(name, args, func, attrs, doc) {
  /* TODO: make documentation database external */
  let subr = ty.subr(name, args, func);
  subroutines_registry[name] = subr;
};

/*
 *  types
 */
define_subr('subrp', [[ty.any]], function(args) {
  return ty.bool(ty.is_subr(args[0]));
});

define_subr('functionp', [[ty.any]], function(args) {
  return ty.bool(ty.is_function(args[0]));
});
define_subr('macrop', [[ty.any]], function(args) {
  return ty.bool(ty.is_macro(args[0]));
});

define_subr('listp', [[ty.any]], function(args) {
  return ty.bool(ty.is_list(args[0]));
});
define_subr('nlistp', [[ty.any]], function(args) {
  return ty.bool(!ty.is_list(args[0]));
});

define_subr('numberp', [[ty.any]], function(args) {
  return ty.bool(ty.is_number(args[0]));
});

define_subr('symbolp', [[ty.any]], function(args) {
  return ty.bool(ty.is_symbol(args[0]));
});

// subset of symbolp
define_subr('booleanp', [[ty.any]], function(args) {
  let expr = args[0];
  let val = ty.is_symbol(expr) && expr.to_string();
  return ty.bool(val == 't' || val == 'nil');
});

function natnump(args) {
  return ty.bool(ty.is_integer(args[0]) && args[0].to_js() >= 0);
}
define_subr('natnump', [[ty.any]], natnump);
define_subr('wholenump', [[ty.any]], natnump);

define_subr('vectorp', [[ty.any]], function(args) {
  return ty.bool(ty.is_vector(args[0]));
});

define_subr('threadp', [[ty.any]], function(args) {
  throw Error('todo');
});

define_subr('string-or-null-p', [[ty.any]], function(args) {
  return ty.bool(ty.is_string(args[0]) || args[0] === ty.nil);
});

define_subr('stringp', [[ty.any]], function(args) {
  return ty.bool(ty.is_string(args[0]));
});

define_subr('recordp', [[ty.any]], function(args) {
  throw Error('todo');
});

define_subr('sequencep', [[ty.any]], function(args) {
  return ty.bool(ty.is_sequence(args[0]));
});

define_subr('processp', [[ty.any]], function(args) {
  throw Error('todo');
});

define_subr('mutexp', [[ty.any]], function(args) {
  throw Error('todo');
});

define_subr('keywordp', [[ty.any]], function(args) {
  return ty.bool(ty.is_symbol(args[0]) && args[0].to_string()[0] === ':' && args[0].to_string() in ty.interned_symbols);
});

define_subr('integerp', [[ty.any]], function(args) {
  return ty.bool(ty.is_integer(args[0]));
});

define_subr('hash-table-p', [[ty.any]], function(args) {
  return ty.bool(ty.is_hash_table(args[0]));
});

define_subr('floatp', [[ty.any]], function(args) {
  throw Error('todo');
});

define_subr('consp', [[ty.any]], function(args) {
  return ty.bool(ty.is_cons(args[0]));
});

define_subr('condition-variable-p', [[ty.any]], function(args) {
  throw Error('todo');
});

define_subr('char-or-string-p', [[ty.any]], function(args) {
  return ty.bool(ty.is_integer(args[0]) || ty.is_string(args[0]));
});

define_subr('char-table-p', [[ty.any]], function(args) {
  return ty.bool(ty.is_char_table(args[0]));
});

define_subr('byte-code-function-p', [[ty.any]], function(args) {
  console.warn('byte code compilation not supported');
  return ty.nil;
});

define_subr('case-table-p', [[ty.any]], function(args) {
  throw Error('todo');
});

define_subr('bool-vector-p', [[ty.any]], function(args) {
  return ty.bool(ty.is_bool_vector(args[0]));
});

define_subr('atom', [[ty.any]], function(args) {
  return ty.bool(ty.is_atom(args[0]));
});

define_subr('arrayp', [[ty.any]], function(args) {
  return ty.bool(ty.is_array(args[0]));
});

define_subr('zerop', [[ty.number]], function(args) {
  return num_equals.call(this, [args[0], ty.integer(0)]);
});

// NOTE: windowp, window-live-p, syntax-table-p, overlayp, number-or-marker-p,
// markerp, keymapp, integer-or-marker-p, framep, 'frame-configuration-p',
// 'frame-live-p', fontp, custom-variable-p, commandp, bufferp and
// window-configuration-p are Emacs concerns

define_subr('type-of', [[ty.any]], function(args) {
  return ty.symbol(args[0].type);
});

/*
 *  introspection
 */

define_subr('jscode', [[ty.any]],
function(args) {
  let arg = args[0];
  if (ty.is_function(arg)) {
    return ty.string(arg.to_jsstring());
  }
  let jscode = translate.expr(args[0], this);
  return ty.string(jscode);
},
{ need_env: true });

define_subr('jseval', [[ty.any]],
function(args) {
  return ty.from_js(eval(args[0].to_js()), this);
});

define_subr('read', [[ty.string]], function(args) {
  elisp = elisp || require('./elisp');
  let input = args[0].to_js();
  return ty.list(elisp.readtop(input));
});

define_subr('eval', [[ty.any]], function(args) {
  elisp = elisp || require('./elisp');
  return elisp.eval_lisp(args[0]);
});

define_subr('macroexpand-1', [[ty.any]], function (args) {
  return translate.macroexpand_1(args[0], this);
});

/*
 *  integer operations
 */
define_subr('+', [[], [], ty.is_number], function(args) {
  if (args.length == 2)
    return ty.integer(args[0].to_js() + args[1].to_js());
  let sum = 0;
  for (let i = 0; i < args.length; ++i)
    sum += args[i].to_js();
  return ty.integer(sum);
});
define_subr('-', [[ty.is_number], [], ty.is_number], function(args) {
  let x = args[0].num;
  return ty.integer(args[1] ? x - args[1].num : -x);
});

define_subr('*', [[], [], ty.is_number], function(args) {
  if (args.length == 2)
    return ty.integer(args[0].to_js() * args[1].to_js());
  let prod = 1;
  for (let i = 0; i < args.length; ++i)
    prod *= args[i].to_js();
  return ty.integer(prod);
});

define_subr('<=', [[], [], ty.is_number], function(args) {
  if (args.length == 2)
    return ty.bool(args[0].to_js() <= args[1].to_js());
  for (let i = 1; i < args.length; ++i)
    if (args[i-1].to_js() > args[i].to_js())
      return ty.nil;
  return ty.t;
});

/*
 *  Lists
 */
define_subr('car', [[ty.is_list]], function(args) {
  return args[0].hd || ty.nil;
});
define_subr('cdr', [[ty.is_list]], function(args) {
  return args[0].tl || ty.nil;
});
define_subr('cons', [[ty.any, ty.any]], function(args) {
  return ty.cons(args[0], args[1]);
});
define_subr('list', [[], [], ty.any], function(args) {
  return ty.list(args);
});

/*
 *  environment
 */
define_subr('fset', [[ty.is_symbol, ty.any]],
function(args) {
  let [sym, val] = args;
  return this.fset(sym.to_string(), val);
},
{ need_env: true });

define_subr('symbol-function', [[ty.is_symbol]],
function(args) {
  let sym = args[0];
  return this.fget(sym.to_string(), true);
},
{ need_env: true });

/*
 *  Errors
 */
define_subr('error', [[ty.is_symbol], [], ty.is_string],
function(args) {
  let [message, ...formatArgs] = args;
  throw new ty.LispError(format_message(message.to_js(), formatArgs), 'error', this);
});

function format_message(message, args) {
  let index = 0;
  args = args.map((x) => x.to_string());
  const formatArgs = [];
  message = message.replace(/%\w/g, match => {
    if (match === '%S') {
      return args[index++];
    }
    formatArgs.push(args[index++]);
    return match;
  });
  return util.format.apply(null, [message, ...formatArgs]);
}

/*
 *  Utils
 */
define_subr('print', [[ty.any]], function(args) {
  let expr = args[0];
  console.log(expr.to_string());
  return expr;
});
define_subr('float-time', [[]], function() {
  return ty.integer(Date.now() / 1000);
});

define_subr('eq', [[ty.any, ty.any]], function(args) {
  return ty.bool(args[0] === args[1]);
});

define_subr('equal', [[ty.any, ty.any]], function(args) {
  if (args[0] === args[1]) {
    return ty.t;
  }
  if (args[0].constructor !== args[1].constructor) {
    return ty.nil;
  }
  return ty.bool(args[0].equals(args[1]));
});

function num_equals(args) {
  if (args.length == 0) {
    throw new ty.LispError('Wrong number of arguments: =, 0', 'error', this);
  }
  args = args.map(arg => {
    if (!ty.is_number(arg)) {
      throw Error('todo');
    }
    return arg.to_js();
  });
  const number = args[0];
  for (let arg of args) {
    if (arg !== number) {
      return ty.nil;
    }
  }
  return ty.t;
}

define_subr('=', [[], [], ty.any], num_equals);

const features = [];

define_subr('require', [[ty.symbol], [ty.string, ty.any]], async function(args) {
  const load = require('./load');

  const feature = args[0].to_string();
  if (args[1] !== ty.nil || args[2] !== ty.nil) {
    throw Error('todo');
  }
  if (features.includes(feature)) {
    return;
  }
  await load(feature + '.el', {}, this);
  if (!features.includes(feature)) {
    throw ty.LispError(`feature ${feature} not provided by ${feature}.el`, 'error', this);
  };
  return args[0];
});

define_subr('provide', [[ty.symbol], [ty.list]], function(args) {
  if (args[1] !== ty.nil) {
    throw Error('todo');
  }
  features.push(args[0].to_string());
  return ty.nil;
});

/*
 *  Exports
 */
exports.all = subroutines_registry;
