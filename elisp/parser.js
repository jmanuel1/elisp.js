'use strict';

let ty = require('./types.js');
let P = require('parsimmon');
const assert = require('assert').strict;

/*
 * Utils
 */
let mustEscape = "#;()[] \t\n\r\\\"'`,?";

let hexdigit = P.regex(/[0-9A-Fa-f]/);
let octdigit = P.regex(/[0-7]/);

let uniCharP = P.string('\\u').then(hexdigit.times(4))
  .or(P.string('\\U00').then(hexdigit.times(6)))
  .or(P.string('\\x').then(hexdigit.many()))
  .map((ds) => parseInt(ds.join(''), 16));
let octCharP = P.string('\\').then(octdigit.times(3))
  .map((os) => parseInt(os.join(''), 8));
const escs = {
  a: 7, b: 8, t: 9, n: 10, v: 11, f: 12,
  r: 13, e: 27, s: 32, '\\': 92, d: 127
};
let escCharP = P.string('\\').then(P.any)
  .map((c) => c in escs ? escs[c] : c.charCodeAt(0));
let justCharP = P.any
   .map((c) => c.charCodeAt(0));
const charP = P.lazy(() => P.alt(ctrlCharP, altCharP, hyperCharP, superCharP, shiftCharP, metaCharP, uniCharP, octCharP, escCharP, justCharP));
let ctrlCharP = P.string('\\^').or(P.string('\\C-')).then(charP)
  .map((charCode) => {
    const metaMask = charCode&(1<<27);
    charCode = charCode&~(1<<27);
    let result;
    if (charCode <= 127 && /[a-z]/i.test(String.fromCharCode(charCode))) {
      result = String.fromCharCode(charCode).toUpperCase().charCodeAt(0)-64;
    } else {
      result = (1<<26)|charCode;
    }
    result |= metaMask;
    assert(isCtrlChar(result));
    return result;
  });

function isCtrlChar(charCode) {
  charCode = charCode&~(1<<27);
  const asciiInverse = charCode+64;
  if (asciiInverse <= 127 && /[A-Z]/.test(String.fromCharCode(asciiInverse))) {
    return true;
  }
  return charCode&(1<<26);
}

function unCtrlChar(charCode) {
  if (!isCtrlChar(charCode)) {
    return charCode;
  }
  const metaMask = charCode&(1<<27);
  charCode = charCode&~(1<<27);
  const asciiInverse = charCode+64;
  if (asciiInverse <= 127 && /[A-Z]/.test(String.fromCharCode(asciiInverse))) {
    return asciiInverse;
  }
  const result = charCode&~(1<<26)|metaMask;
  assert(!isCtrlChar(result));
  return result;
}

const META_MODIFER_MASK = 1<<27;

const metaCharP = P.string('\\M-').then(charP)
  .map(charCode =>
    charCode | (1<<27)
  );
const altCharP = P.string('\\A-').then(charP)
  .map(charCode =>
    charCode | (1<<22)
  );
const hyperCharP = P.string('\\H-').then(charP)
  .map(charCode =>
    charCode | (1<<24)
  );
const superCharP = P.string('\\s-').then(charP)
  .map(charCode =>
    charCode | (1<<23)
  );
const shiftCharP = P.string('\\S-').then(charP)
  .map(charCode =>
    charCode | (1<<25)
  );

let wordstop = P.oneOf(mustEscape).or(P.eof);

let comment = P.string(';')
  .then(P.noneOf('\n').many())
  .then(P.end);
let gap = P.oneOf(" \t\r\n\f").or(comment);
let optWhitespace = gap.many();
let whitespace = gap.atLeast(1);

/*
 *  Lisp
 */
let Lisp = P.createLanguage({
  Integer: () => {
    let int = (s, b) => {
      let sign = '';
      if (s.length == 0)
        return; // ty.nil
      if (s[0].match(/[+-]/)) {
        sign = s[0];
        s = s.slice(1);
      }
      if (s.indexOf('.') == 0)
        return; // ty.nil
      return parseInt(sign + s, b);
    };
    let decP = P.regexp(/[+-]?[0-9]+\.?/)
      .map((s) => int(s, 10));
    let binP = P.regexp(/#b[+-]?[01]*(\.[01]*)?/)
      .map((s) => int(s.slice(2), 2));
    let octP = P.regexp(/#o[+-]?[0-7]*(\.[0-7]*)?/)
      .map((s) => int(s.slice(2), 8));
    let hexP = P.regexp(/#x[+-]?[0-9a-fA-F]*(\.[0-9a-fA-F]*)?/)
      .map((s) => int(s.slice(2), 16));
    let baseP = P.seqMap(
        P.regexp(/#[23]?[0-9]/),
        P.regexp(/r[+-]?[0-9a-zA-Z]*(\.[0-9a-zA-Z]*)?/),
        (b, s) => {
          let base = parseInt(b.slice(1), 10);
          if (!(2 <= base && base <= 36))
            throw new ty.LispError(
              "Invalid read syntax: integer, radix " + base
            );
          return int(s.slice(1), base);
        });

    return P.alt(binP, octP, hexP, baseP, decP)
      .lookahead(wordstop)     // otherwise it's a symbol
      .map((n) => ty.integer(n))
      .desc("number");
  },

  Float: () => {
    return P.regexp(/[+-]?(?:[0-9]*\.[0-9]+(?:e[+-]?[0-9]+)?|[0-9]+e[+-]?[0-9]+)/)
      .lookahead(wordstop)
      .map(n => ty.float(Number(n)))
      .desc('float');
  },

  Character: () => {
    return P.string('?')
      .then(P.alt(charP))
      .map(ty.integer)
      .desc('character');
  },

  String: () => {
    let dquote = P.string('"');
    let ignored = P.string('\\').then(P.oneOf('\n ')).result('');
    let escapes = P.string('\\').then(P.oneOf('"\\'));
    let unichar = uniCharP.map((code) => String.fromCharCode(code));
    let octchar = octCharP.map((code) => String.fromCharCode(code));
    let ctrlmetachar = ctrlCharP.or(metaCharP).map((code) => {
      let prefix = '';
      if (isCtrlChar(code)) {
        prefix += '^';
        code = unCtrlChar(code);
      }
      if (code&META_MODIFER_MASK) {
        code = (code&~META_MODIFER_MASK)|(1<<7);
      }
      return prefix + String.fromCharCode(code);
    });

    return P.alt(ctrlmetachar, unichar, octchar, ignored, escapes, P.noneOf('"'))
      .many().wrap(dquote, dquote)
      .map((cs) => ty.string(cs.join('')))
      .desc("string");
  },

  Symbol: (r) => {
    let nilp = P.string('nil').lookahead(wordstop).result(ty.nil);

    const symbolMustEscape = "#;()[] \t\n\r\\\"'`,";
    let charp = P.noneOf(symbolMustEscape).or(P.string('\\').then(P.any));
    let symp = charp.atLeast(1).map(atom => ty.interned_symbol(atom.join('')));
    return P.notFollowedBy(P.string('.').lookahead(wordstop))
      .then(nilp.or(symp).desc("symbol"));
  },

  Expression: (r) => {
    let quotemap = {"'": "quote", "#'": "function"};
    let quote = P.seqMap(
        P.alt(P.string(",@"), P.string("#'"), P.oneOf("'`,")), r.Expression,
        (q, e) => ty.list([ty.interned_symbol(quotemap[q] || q), e])
      ).desc("quoted expression");
    // fast backtracking first:
    return P.alt(
      r.Character,
      r.String,
      quote,
      r.List,
      r.Vector,
      r.Float,
      r.Integer,
      r.Symbol
    ).wrap(optWhitespace, optWhitespace)
    .desc("expression");
  },

  List: (r) => {
    let open = P.string('(').then(optWhitespace);
    let close = optWhitespace.then(P.string(')'));
    let listp = r.Expression.notFollowedBy(P.string('.')).many()
      .desc("list");
    const pairp = P.seqMap(r.Expression, P.string('.').then(r.Expression), ty.cons)
      .desc("pair");
    return P.seqMap(listp, pairp.fallback(ty.nil), ty.list)
      .wrap(open, close)
      .desc("list/pair");
  },

  Vector: (r) => {
    let open = P.string('[').then(optWhitespace);
    let close = optWhitespace.then(P.string(']'));
    return r.Expression.many()
      .wrap(open, close)
      .map(ty.vector)
      .desc("vector");
  }
});


let mkParser = (p) => (input) => p.tryParse(input);

exports.parseInteger = mkParser(Lisp.Integer);
exports.parseCharacter = mkParser(Lisp.Character);
exports.parseSymbol = mkParser(Lisp.Symbol);
exports.parseString = mkParser(Lisp.String);
exports.parseExpr = mkParser(Lisp.Expression);
exports.parseList = mkParser(Lisp.List);
exports.parseVector = mkParser(Lisp.Vector);

exports.read = (input) => Lisp.Expression.tryParse(input.trim());
exports.readtop = (input) => Lisp.Expression.many().tryParse(input);
