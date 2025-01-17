'use strict';

const ty = require('./types');
const subr = require('./subr');
const vars = require('./vars');


/*
 *  Variables are handles to a value stack inside Environment
 *    (to have less keyed lookups in hot code)
 */
function Variable(name, stack, is_fun, env) {
  this.name = name;
  this.stack = stack;
  this.is_fun = is_fun;
  this.env = env;
}
/* PERF TODO: compare to properties */
Variable.prototype.get = function() {
  if (this.stack.length)
    return this.stack[0];
  let ns = this.is_fun ? "function" : "variable";
  throw new ty.LispError(`Symbol's value as ${ns} is void: ${this.name}`, 'error', this.env);
};
Variable.prototype.set = async function(val) {
  if (val instanceof Promise) {
    throw Error('no promises here!');
  }
  if (this.is_fun)
    val = await ty.from_list(val)
  this.stack[0] = val;
  return val;
};

/*
 *  Lisp Environment:
 */
function Environment(name, custom_subrs) {
  this.name = name || 'env';

  /* values: name -> stack of values */
  this.vs = {};
  for (let v in vars.all) {
    this.vs[v] = [ vars.all[v] ];
  }

  /* functions: name->LispFun */
  this.fs = {};
  for (let sub in subr.all) {
    this.fs[sub] = [ subr.all[sub] ];
  }
  if (typeof custom_subrs === 'object') {
    for (let sub in custom_subrs)
      this.fs[sub] = [ custom_subrs[sub] ];
  }
}

Environment.prototype.copy = function() {
  const copy = new Environment(this.name);
  for (let variable in this.vs) {
    copy.vs[variable] = [...this.vs[variable]];
  }
  for (let fun in this.fs) {
    copy.fs[fun] = [...this.fs[fun]];
  }
  return copy;
}

Environment.prototype.to_jsstring = function() {
  return 'global.' + this.name;
};

/*
 *  functions namespace
 */
Environment.prototype.fun = function(name) {
  let stack = this.fs[name];
  if (stack === undefined) {
    stack = [];
    this.fs[name] = stack;
  }
  if (stack.length && ty.is_macro(stack[0]))
    throw new Error('Macro accessed as a function');
  return new Variable(name, stack, true);
}

Environment.prototype.fset = async function(name, value) {
  /* try to make it LispFun/LispMacro */
  value = await ty.from_list(value, this);
  if (!value) {
    throw new ty.LispError('fset value must be function-like', 'error', this);
  }

  let stack = this.fs[name];
  if (stack === undefined) {
    this.fs[name] = [value];
  } else {
    stack[0] = value;
  }
  return value;
}

Environment.prototype.fget = function(name, is_macro) {
  let stack = this.fs[name];
  if (stack && stack.length) {
    if (ty.is_macro(stack[0]) && !is_macro)
      throw new Error('Macro accessed as a function');
    return stack[0];
  }
  throw new ty.LispError("Symbol's function definition is void: " + name, 'error', this);
}

/*
 *  values namespace
 */
Environment.prototype.var_ = function(name) {
  let stack = this.vs[name];
  if (stack === undefined) {
    stack = [];
    this.vs[name] = stack;
  }
  return new Variable(name, stack, false, this);
}

Environment.prototype.set = function() {
  let value;
  for (let i = 0; i < arguments.length; i += 2) {
    let name = arguments[i];
    value = arguments[i+1];

    let stack = this.vs[name];
    if (stack === undefined) {
      /* should this happen? */
      this.vs[name] = [value];
    } else if (stack.length) {
      stack[0] = value;
    } else {
      stack.push(value);
    }
  }
  return value;
};

Environment.prototype.get = function(name) {
  let stack = this.vs[name];
  if (stack && stack.length)
    return stack[0];
  throw new ty.LispError("Symbol's value as variable is void: " + name, 'error', this);
};

Environment.prototype.push = function(names, values) {
  for (let i = 0; i < names.length; ++i) {
    let name = names[i];
    let value = values[i];

    let stack = this.vs[name];
    if (stack === undefined) {
      this.vs[name] = [value];
    } else {
      stack.unshift(value);
    }
  }
};

Environment.prototype.pop = function(names) {
  for (let i = 0; i < names.length; ++i) {
    let name = names[i];
    this.vs[name].shift();
  }
};

Environment.prototype.is_bound = function(name) {
  let stack = this.vs[name];
  return stack && stack.length;
};
Environment.prototype.is_fbound = function(name) {
  let stack = this.fs[name];
  return (stack && stack.length);
};

Environment.prototype.has_jsdebug = function() {
  return this.is_bound('*jsdebug*') && !this.get('*jsdebug*').is_false;
};

exports.Environment = Environment;
