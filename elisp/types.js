'use strict';

const assert = require('assert');
let translate;
var elisp;  /* lazy-loaded './elisp' */

function from_js(val, env) {
  if (val instanceof LispObject)
    return val;
  switch (typeof val) {
    case 'number': return new LispInteger(val);
    case 'string': return new LispString(val);
    case 'symbol': return new LispSymbol(val.toString().slice(7, -1));
    case 'boolean': return val ? exports.t : LispNil;
    case 'undefined': return LispNil;
    default: throw new LispError('Failed to lispify: ' + val.toString(), 'error', env);
  }
}


/*
 *  LispObject
 */

function LispObject() {};

LispObject.prototype = {
  to_string: function() { return '#<object>'; },
  extend: function(clss) { clss.prototype = Object.create(this.prototype); },
  equals: function(that) { return this === that; },
  fcall: function(args, env) {
    throw new LispError(`${this.to_string()} is not a function`, 'error', env);
  }
};

/*
 *  LispInteger
 */

function LispInteger(num) {
  assert(typeof num === 'number');
  this.num = num;
};
LispInteger.prototype = Object.create(LispObject.prototype);
Object.defineProperty(LispInteger.prototype,
  'is_number', { value: true, writable: false }
);
Object.defineProperty(LispInteger.prototype,
  'type', { value: 'integer', writable: false }
);

LispInteger.prototype.to_string = function() { return this.num; };
LispInteger.prototype.to_js = function() { return this.num; };
LispInteger.prototype.to_jsstring = function() { return 'ty.integer(' + this.num + ')'; };

LispInteger.prototype.equals = function(that) {
  return that && that instanceof LispInteger
    && this.num === that.num;
};

/*
 *  LispFloat
 */

function LispFloat(num) {
  assert(typeof num === 'number');
  this.num = num;
};
LispFloat.prototype = Object.create(LispObject.prototype);
Object.defineProperty(LispFloat.prototype,
  'is_number', { value: true, writable: false }
);
Object.defineProperty(LispFloat.prototype,
  'type', { value: 'float', writable: false }
);

LispFloat.prototype.to_string = function() { return this.num; };
LispFloat.prototype.to_js = function() { return this.num; };
LispFloat.prototype.to_jsstring = function() { return 'ty.float(' + this.num + ')'; };

LispFloat.prototype.equals = function(that) {
  return that && that instanceof LispFloat
    && this.num === that.num;
};

/*
 *  LispSymbol
 */
function LispSymbol(sym) {
  this.sym = sym;
};

LispSymbol.make = function(sym) {
  if (sym in this.interned)
    return this.interned[sym];
  return new LispSymbol(sym);
}

LispSymbol.prototype = Object.create(LispObject.prototype);

LispSymbol.prototype.to_string = function() { return this.sym; };
LispSymbol.prototype.to_js = function() { return Symbol(this.sym); };
LispSymbol.prototype.to_jsstring = function() { return "ty.interned_symbol('" + this.sym + "')" };

Object.defineProperty(LispSymbol.prototype,
  'is_symbol', { value: true, writable: false }
);
Object.defineProperty(LispSymbol.prototype,
  'type', { value: 'symbol', writable: false }
);

LispSymbol.prototype.equals = function(that) {
  return that && that instanceof LispSymbol
    && that.sym === this.sym;
};

LispSymbol.prototype.is_selfevaluating = function() {
  return this.sym === 't' || this.sym.startsWith(':');
};

LispSymbol.interned = {
  'lambda': new LispSymbol('lambda'),
  'macro': new LispSymbol('macro'),
  'quote': new LispSymbol('quote'),
};

/*
 *  LispCons, LispNil
 */

function NilClass() {};
NilClass.prototype = Object.create(LispObject.prototype);

NilClass.prototype.toString = function() { return "[Object LispNil]"; };

NilClass.prototype.to_string = () => "nil";
NilClass.prototype.to_js = () => LispNil;
NilClass.prototype.to_jsstring = () => "ty.nil";
NilClass.prototype.to_array = () => [];
NilClass.prototype.is_selfevaluating = () => true;

// you can be everything you want to be:
Object.defineProperty(NilClass.prototype, 'is_symbol', { value: true, writable: false });
Object.defineProperty(NilClass.prototype, 'type', { value: 'symbol', writable: false });

// ...even a sequence!
NilClass.prototype.seqlen = () => 0;
NilClass.prototype.forEach = () => null;
Object.defineProperty(NilClass.prototype, 'is_seq', { value: true, writable: false });
Object.defineProperty(NilClass.prototype, 'is_list', { value: true, writable: false });

// ...even a nobody!
Object.defineProperty(NilClass.prototype, 'is_false', { value: true, writable: false });

const LispNil = new NilClass();
NilClass.prototype.equals = (that) => that == LispNil;



function LispCons(hd, tl) {
  if (hd instanceof Promise) {
    throw Error('no promises here!');
  }
  this.hd = hd;
  this.tl = tl;
};

LispCons.prototype = Object.create(LispObject.prototype);
Object.defineProperty(LispCons.prototype,
  'is_list', { value: true, writable: false }
);
Object.defineProperty(LispCons.prototype,
  'is_seq', { value: true, writable: false }
);
Object.defineProperty(LispCons.prototype,
  'type', { value: 'cons', writable: false }
);

LispCons.prototype.forEach = function(callback, env) {
  for (let cur = this; cur != LispNil; cur = cur.tl) {
    callback(cur.hd);
    if (!cur.tl.is_list)
      throw new LispError('LispCons.forEach: not a regular list', 'error', env);
  }
};

LispCons.prototype.show_elems = function() {
  let elems = [];
  let cur = this;
  while (cur != LispNil) {
    elems.push(cur.hd.to_string());
    if (!cur.tl.is_list) {
      elems.push('.');
      elems.push(cur.tl.to_string());
      break;
    }
    cur = cur.tl;
  };
  return elems;
};

LispCons.prototype.to_string = function() {
  return '(' + this.show_elems().join(' ') + ')';
};
LispCons.prototype.to_js = function() {
  return '[' + this.show_elems().join(', ') + ']';
};
LispCons.prototype.to_jsstring = function() {
  let elems = [];
  this.forEach((item) => {
    let repr = item.to_jsstring();
    elems.push(repr);
  });
  return "ty.list([" + elems.join(', ') + '])';
};

LispCons.prototype.seqlen = function() {
  let cur = this;
  let len = 0;
  this.forEach(() => ++len);
  return len;
};

LispCons.prototype.equals = function(that) {
  return that && that instanceof LispCons
    && that.hd.equals(this.hd) && that.tl.equals(this.tl);
};

let consify = (elems, last = LispNil) => {
  let cur = last;
  while (elems.length) {
    let elem = elems.pop();
    cur = new LispCons(elem, cur);
  }
  return cur;
};

LispCons.prototype.to_array = function() {
  let val = [];
  this.forEach((item) => val.push(item));
  return val;
};

/*
 *  string
 */
function LispString(str) {
  this.str = str;
};
LispString.prototype = Object.create(LispObject.prototype);
Object.defineProperty(LispString.prototype,
  'is_array', { value: true, writable: false }
);
Object.defineProperty(LispString.prototype,
  'type', { value: 'string', writable: false }
);

LispString.prototype.to_string = function() {
  let str = this.str;
  str = str.replace(/\\/g, "\\\\");
  str = str.replace(/"/g, "\\\"");
  return '"' + str + '"';
};

LispString.prototype.to_js = function() { return this.str; };
LispString.prototype.to_jsstring = function() { return 'ty.string(' + this.to_string() + ')'; };

LispString.prototype.seqlen = function() {
  return this.str.length;
};

LispString.prototype.equals = function(that) {
  return that && that instanceof LispString
    && that.str === this.str;
};


/*
 *  hash-table
 */

function LispHashtable(hash) {
  this.hash = hash;
};
LispHashtable.prototype = Object.create(LispObject.prototype);

LispHashtable.prototype.to_string = function() {
  return "#s(<TODO>)";
};

LispHashtable.prototype.to_js = () => this.hash;

LispHashtable.prototype.equals = function (that) {
  if (!(that instanceof LispHashtable)) {
    return false;
  }
  const thisEntries = [...Object.entries(this.hash)].sort((a, b) => a[0] - b[0]);
  const thatEntries = [...Object.entries(that.hash)].sort((a, b) => a[0] - b[0]);
  if (thisEntries.length !== thatEntries.length) {
    return false;
  }
  for (let i = 0; i < thisEntries.length; i++) {
    if (thisEntries[i][0] !== thatEntries[i][0] || !(thisEntries[i][1].equals(thatEntries[i][1]))) {
      return false;
    }
  }
  return true;
}

Object.defineProperty(LispHashtable.prototype,
  'type', { value: 'hash-table', writable: false }
);

/*
 *  vectors
 */
function LispVector(arr) {
  arr = arr || [];
  this.arr = arr;
};
LispVector.prototype = Object.create(LispObject.prototype);
Object.defineProperty(LispVector.prototype,
  'is_array', { value: true, writable: false }
);
Object.defineProperty(LispVector.prototype,
  'is_seq', { value: true, writable: false }
);
Object.defineProperty(LispVector.prototype,
  'type', { value: 'vector', writable: false }
);

LispVector.prototype.to_string = function() {
  return '[' + this.arr.map(obj => obj.to_string()).join(' ') + ']';
};
LispVector.prototype.to_js = LispVector.prototype.to_array = function() { return this.arr; };
LispVector.prototype.to_jsstring = function() {
  return 'ty.vector([' + this.arr.map((it) => it.to_jsstring()).join(', ') + '])';
};
LispVector.prototype.seqlen = function() {
  return this.arr.length;
};
LispVector.prototype.equals = function(that) {
  return that && that instanceof LispVector
    && this.arr.length == that.arr.length
    && this.arr.every((v, i) => that.arr[i].equals(v));
};

LispVector.prototype.forEach = function(callback) {
  this.arr.forEach((item) => callback(item));
};

/*
 *  char-table
 */
function LispChartable() { };
LispChartable.prototype = Object.create(LispObject.prototype);

Object.defineProperty(LispChartable.prototype,
  'type', { value: 'char-table', writable: false }
);

LispChartable.prototype.equals = function () {
  throw Error('todo');
}

/*
 *  bool-vector
 */
function LispBoolvector() { };
LispBoolvector.prototype = Object.create(LispObject.prototype);

Object.defineProperty(LispBoolvector.prototype,
  'type', { value: 'bool-vector', writable: false }
);

LispBoolvector.prototype.equals = function () {
  throw Error('todo');
}

/*
 *  lambda
 */
function LispFun(args, body, interact, doc) {
  elisp = elisp || require('./elisp');

  this.args = args;
  this.body = body;
  this.interact = interact;
  this.doc = doc;

  /* LispCons compatibility */
  this.hd = exports.symbol('lambda');
  let argl = consify(this.args.map((arg) => new LispSymbol(arg)));

  this.tl = new LispCons(argl, body);
};

LispFun.parse_argspec = function(args, env) {
  let argspec = [];
  args.forEach((arg) => {
    if (!exports.is_symbol(arg))
      throw new LispError('Wrong type argument: symbolp, ' + arg.to_string(), 'error', env);
    argspec.push(arg.to_string());
  });
  return argspec;
};

LispFun.from = async function(lst, env) {
  translate = require('./translate');

  if (lst.hd.sym !== 'lambda')
    return;
  lst = lst.tl;
  if (!exports.is_cons(lst))
    return;

  let args = lst.hd;
  let argspec = LispFun.parse_argspec(args, env);
  if (!argspec)
    return;

  let body = await translate.macroexpand_all(lst.tl, env);
  return exports.lambda(argspec, body);
};

LispFun.prototype = Object.create(LispCons.prototype);

Object.defineProperty(LispFun.prototype,
  'is_function', { value: true, writable: false }
);
Object.defineProperty(LispFun.prototype,
  'type', { value: 'cons', writable: false }
);

LispFun.prototype.to_js = function() { return this.func; };
LispFun.prototype.to_jsstring = function() { return this.jscode || '#<thunk>'; };

LispFun.prototype.to_string = function() {

  return LispCons.prototype.to_string.call(this);
};

LispFun.prototype.fcall = async function(args, env) {
  console.debug('lispfun fcall');
  try {
    return await Promise.resolve(elisp.fcall.call(this, args, env));
  } catch (e) {
    if (e.message !== 'Macro accessed as a function')
      throw e;
    /* trigger recompilation */
    this.func = undefined;
    return await Promise.resolve(elisp.fcall.call(this, args, env));
  }
};

LispFun.prototype.equals = function (that) {
  return that instanceof LispFun &&
    this.args.every((arg, i) => arg === that.args[i]) &&
    this.body.equals(that.body) &&
    (this.interact && that.interact ? this.interact.equals(that.interact) : true) &&
    this.doc === that.doc;
}

/*
 *  native subroutine
 */
function LispSubr(name, args, func, attrs, doc) {
  this.name = name;
  this.args = args;
  this.func = func;
  this.attrs = attrs;
  this.doc = doc;
};

LispSubr.prototype = Object.create(LispObject.prototype);

Object.defineProperty(LispSubr.prototype,
  'is_function', { value: true, writable: false }
);
Object.defineProperty(LispSubr.prototype,
  'type', { value: 'subr', writable: false }
);

LispSubr.prototype.to_string = function() { return "#<subr " + this.name + ">"; }
LispSubr.prototype.to_js = function() { return this.func; }
LispSubr.prototype.to_jsstring = function () { return "subr.all['" + this.name + "']"; };

LispSubr.prototype.fcall = async function(args, env) {
  return await Promise.resolve(this.func.call(env, args));
};

LispSubr.prototype.equals = function(that) {
  // FIXME: comparing attrs, args
  return this.name === that.name &&
    this.args === that.args &&
    this.func === that.func &&
    this.attrs === that.attrs &&
    this.doc === that.doc;
}

/*
 *  macros
 */
function LispMacro(transform) {
  assert(transform instanceof LispFun);
  this.transform = transform;

  /* LispCons */
  this.hd = exports.symbol('macro');
  this.tl = transform;
};

LispMacro.from = async function(lst, env) {
  if (lst.hd.sym !== 'macro')
    return;
  let transform = await LispFun.from(lst.tl, env);
  if (!transform)
    return;
  return new LispMacro(transform);
};

LispMacro.prototype = Object.create(LispCons.prototype);

LispMacro.prototype.macroexpand = async function(args, env) {
  args = args.to_array();
  return this.transform.fcall(args, env);
};

Object.defineProperty(LispMacro.prototype,
  'type', { value: 'cons', writable: false }
);

LispMacro.prototype.equals = function(that) {
  return this.transform.equals(that.transform);
}

/*
 *  Errors
 */
class LispError extends Error {
  constructor(message, tag, env) {
    super(message);
    this.name = 'LispError';
    this.tag = tag || 'error';

    let stack = this.stack.split('\n');
    this.stack = stack.slice(0, 1).concat(stack.slice(2)).join('\n');
    this.env = env && env.copy();
  }
}

/*
 *  exports
 */

/* type predicates */
exports.any =       (obj) => obj instanceof LispObject;
exports.is_string = (obj) => obj instanceof LispString;
exports.is_vector = (obj) => obj instanceof LispVector;
exports.is_subr =   (obj) => obj instanceof LispSubr;
exports.is_macro =  (obj) => obj instanceof LispMacro;
exports.is_cons =   (obj) => obj instanceof LispCons;
exports.is_integer = (obj) => obj instanceof LispInteger;
exports.is_hash_table = (obj) => obj instanceof LispHashtable;
exports.is_char_table = (obj) => obj instanceof LispChartable;
exports.is_bool_vector = (obj) => obj instanceof LispBoolvector;

exports.is_function = (obj) => obj.is_function;
exports.is_symbol =   (obj) => obj.is_symbol;
exports.is_number =   (obj) => obj.is_number;
exports.is_atom =     (obj) => (obj == LispNil) || !obj.is_list;
exports.is_list =     (obj) => obj.is_list;
exports.is_sequence = (obj) => obj.is_seq;
exports.is_array =    (obj) => obj.is_array;

/* constructors */
exports.integer = (n) => (typeof n === 'undefined') ? LispNil : new LispInteger(n);
exports.float = (n) => (typeof n === 'undefined') ? LispNil : new LispFloat(n);
exports.symbol  = (s) => new LispSymbol(s);
exports.interned_symbol = (s) => {
  const symbol = LispSymbol.make(s);
  if (!(s in LispSymbol.interned)) {
    LispSymbol.interned[s] = symbol;
  }
  return symbol;
};
exports.list    = (arr, last) => consify(arr, last);
exports.vector  = (arr) => new LispVector(arr);
exports.string  = (s) => new LispString(s);
exports.lambda  = (argspec, body, interact, doc) => new LispFun(argspec, body, interact, doc);
exports.subr    = (name, argspec, jscode) => new LispSubr(name, argspec, eval(jscode));
exports.cons    = (h, t) => new LispCons(h, t);
exports.bool    = (b) => (b ? exports.t : exports.nil);

exports.from_js = from_js;

/* errors */
exports.LispError = LispError;

/* functions,macros */
exports.parse_argspec = LispFun.parse_argspec;
exports.from_list = async (lst, env) => {
  if (exports.is_cons(lst)) {
    let callable = await LispFun.from(lst, env) || await LispMacro.from(lst, env);
    return callable || lst;
  }
  return lst;
};

/* constants */
exports.nil     = LispNil;
exports.t       = exports.interned_symbol('t');

/* obarray I guess */
exports.interned_symbols = LispSymbol.interned;
