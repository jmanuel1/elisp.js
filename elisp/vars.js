const ty = require('./types');

exports.all = {
  'most-negative-fixnum': ty.integer(Number.MIN_SAFE_INTEGER),
  'most-positive-fixnum': ty.integer(Number.MAX_SAFE_INTEGER)
};
