const {promises: fs} = require('fs');
const path = require('path');

// TODO: Borrow implementation from repl.js
module.exports = async function load(filename, {isAbsolutePath} = {isAbsolutePath: false}) {
  const {readtop, eval_lisp} = require('./elisp');
  let pathToFile = path.join(__dirname, '../../emacs/lisp/emacs-lisp/', filename);
  if (isAbsolutePath) {
    pathToFile = filename;
  }
  const contents = await fs.readFile(pathToFile, 'utf-8');
  for (let expr of readtop(contents)) {
    await eval_lisp(expr, this);
  }
}
