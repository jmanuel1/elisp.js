'use strict';

const assert = require('assert');

const ty = require('../elisp/types');
const elisp = require('../elisp/elisp');


let env = new elisp.Environment();

let assertEval = async (input, output) => {
  let result = await elisp.eval_text(input, env);
  assert.equal(result, output);
};

let assertEvalTop = async (code, output) => {
  let result = Promise.resolve(null);
  let env = new elisp.Environment();
  for (let form of elisp.readtop(code)) {
    await result;
    result = elisp.eval_lisp(form, env);
    console.debug('result', await result);
  }
  assert.equal((await result).to_string(), output);
};

let assertThrows = (input) => {
  /* TODO : make it check error message */
  assert.rejects(() => elisp.eval_text(input, env), ty.LispError);
};


describe('environment', () => {
  describe('global variables', () => {
    it("should set it",     () => assertEval("(progn (setq x 42) x)", 42));
    it("should re-set it",  () => assertEval("(progn (setq x :ignore) (setq x 42) x)", 42));

    it("should throw if absent",        () => assertThrows("doesnotexist"));
    it("should throw if absent in let", () => assertThrows("(let () doesnotexist)"));
  });

 describe('local variables', () => {
    it("should shadow globals",
        () => assertEval("(progn (setq x 42) (let ((x :ignore)) x) x)", 42));

    //xit("functions can create locals", () => assertEval);
    //it("macros can create locals"), () => );
    //it("condition-case can create locals", () => );
  });

  describe('constant symbols', () => {
    it("cannot set nil",      () => assertThrows("(setq nil t)"));
    it("cannot set t",        () => assertThrows("(setq t nil)"));
    it("cannot set keywords", () => assertThrows("(setq :nope :yes)"));
  });

  describe('void variables', () => {
    it("should throw on a non-existent variable", () => assertThrows("no-such"));
  });

  describe('dynamic scope', () => {
    it("should use dynamic scope", () => {
      let code = `(progn  (fset 'incx (lambda () (setq x (+ 1 x)))) (let ((x 3)) (incx) x))
      `;
      return assertEval(code, 4);
    });
  });
});

describe('types', () => {
  describe('booleanp', () => {
    it("should recognize t", () => assertEval("(booleanp t)", "t"));
    it("should recognize nil", () => assertEval("(booleanp nil)", "t"));
    it("should not recognize anything else", () => assertEval("(booleanp 0)", "nil"));
  });

  describe('subrp', () => {
    it("should accept subrs", () => assertEval("(subrp (symbol-function '+))", "t"));
    it("should reject functions", () => assertEval("(subrp (lambda (x) x))", "nil"));
  });
  describe('functionp', () => {
    it("should accept functions", () => assertEval("(functionp (symbol-function '+))", "t"));
    it("should accept functions", () => assertEval("(functionp (lambda (x) x))", "t"));
    it("should reject others", () => {
      let code = `(progn
        (fset 'twice '(macro lambda (f x) \`(,f (,f ,x))))
        (functionp (symbol-function 'twice)))`;
      return assertEval(code, "nil");
    });
  });

  describe('listp', () => {
    it("nil is list", () => assertEval("(listp nil)", "t"));
    it("(1 . 2) is list", () => assertEval("(listp (cons 1 2))", "t"));
    it("(listp (lambda (x) x))",  () => assertEval("(listp (lambda (x) x))", "t")); // yep!
  });
});


describe('special forms', () => {
  describe('quote', () => {
    it("should quote!",         () => assertEval("'foo", "foo"));
    it("should quote quote!",   () => assertEval("''foo", "(quote foo)"));

    it("(quote) should fail",   () => assertThrows("(quote)"));
  });

  describe('environment', () => {
    describe('setq', () => {
      it("(setq x 12)",
          () => assertEval("(progn (setq x 12) x)", 12));
      it("(setq one 1 two 2)",
          () => assertEval("(progn (setq one 1 two 2) (+ one two))", 3));
    });

    describe('let', () => {
      it("should make a binding",
          () => assertEval("(let ((x 12)) x)", 12));
      it("should make a nil binding",
          () => assertEval("(let (false) false)", "nil"));
      it("should make two bindings",
          () => assertEval("(let ((x 12) (y 15)) (+ x y))", 27));
      it("should make a nested let",
          () => assertEval("(let ((x 12)) (let ((y 15)) (+ x y)))", 27));
      it("should make a nested let with shadowing",
          () => assertEval("(let ((x :ignore)) (let ((x 42)) x))", 42));
      it("nested let should keep outer values",
          () => assertEval("(let ((x 42)) (progn (let ((x :ignore)) x) x))", 42));

      it("(let () 'ok)",        () => assertEval("(let () 'ok)", "ok"));
      it("(let [] 'ok)",        () => assertEval("(let [] 'ok)", "ok"));
      it("should set (let ((it)) it) to nil",
          () => assertEval("(let ((it)) it)", "nil"));
      it("should return nil for empty body",
          () => assertEval("(let ((it 'pass)))", "nil"));

      it("should not see its own bindings",
          () => assertThrows("(let ((a 1) (b a)) b)"));
      it("let* should see its own bindings",
          () => assertEval("(let* ((x 42) (y x)) y)", 42));

      it("should fail if bindings are not a sequence",
          () => assertThrows("(let it 'fail)", "Wrong type argument: sequencep, 63"));
      it("should reject ((x 1 2))",
          () => assertThrows("(let ((x 1 2)) x)",
            "'let' bindings can have only one value-form: x, 1, 2"));
      it("should reject (:notsettable 42)",
          () => assertThrows("(let ((:constant 42)) :constant)",
            "Attempt to set a constant symbol: :constant"));
      it("should reject ('wrongtype 42)",
          () => assertThrows("(let (('wrong-type 42)) 'wrong-type)",
            "Wrong type argument: symbolp, (quote wrong-type)"));

      it("should take blocks",
          () => assertEval("(let () 'first 'second 'result)", "result"));
    });
  });

  describe('control flow', () => {
    describe('if', () => {
      it("should choose true",         () => assertEval("(if t 1 2)", 1));
      it("should choose false",        () => assertEval("(if nil 1 2)", 2));
      it("should choose true if any",  () => assertEval("(if :true 1 2)", 1));
      it("should take a form",         () => assertEval("(if (if t nil t) 1 2)", 2));
    });

    describe('progn', () => {
      it("should execute commands", () => {
        assertEval("(progn (setq x 1) (setq x (+ x 1)) (setq x (* x 2)) x)", 4);
      });
    });
  });
});


describe('functions', () => {
  describe('fset/symbol-function', () => {
    it("should set and get",
        () => assertEval("(progn (fset 'lol 'gotcha) (symbol-function 'lol))", "gotcha"));
  });

  describe('lambda', () => {
    it("should apply", () => assertEval("((lambda (x) (* x x)) 12)", 144));
    it("should create and apply function",
        () => assertEval("(progn (fset 'sqr (lambda (x) (* x x))) (sqr 12))", 144));
    it("((lambda ())) === nil",
        () => assertEval("((lambda ()))", "nil"));

    it("((lambda . 1)) should be invalid",
        () => assertThrows("((lambda . 1))", "Wrong type argument: listp, 1"));
    it("((lambda)) should be invalid",
        () => assertThrows("((lambda))", "Invalid function: (lambda)"));

    it("should check argument count (1 to 2)",
        () => assertThrows("((lambda (x y) x) 42)", "Wrong number of arguments: (lambda (x y) x), 1"));
    it("should check argument count (0 to 1)",
        () => assertThrows("((lambda (x) x))",
          "Wrong number of arguments: (lambda (x) x), 0"));
    it("should check argument count (1 to 0)",
        () => assertThrows("((lambda () 42) :dummy)",
          "Wrong number of arguments: (lambda nil 42), 1"));
  });

  describe('arguments', () => {
    it("should take no arguments",
        () => assertEval("((lambda ()))", "nil"));
    it("should take one argument",
        () => assertEval("((lambda (x) x) 42)", 42));
    it("should take two arguments",
        () => assertEval("((lambda (x y) (+ x y)) 2 2)", 4));

    it("should take symbols only",
        () => assertThrows("((lambda (1) 1) 1)", "Invalid function: (lambda (1) 1)"));
  });

  describe('funcall/apply', () => {
  });
  describe('apply-partially', () => {
  });
});


describe('macros', () => {
  it("fset's macro", async () => {
    let env = new elisp.Environment();
    await elisp.eval_text("(fset 'twice '(macro lambda (fn arg) (list fn (list fn arg))))", env);
    await elisp.eval_text("(fset 'sqr (lambda (x) (* x x)))", env);
    let result = await elisp.eval_text("(twice sqr 2)", env);
    assert.equal(result, 16);
  });

  it("macroexpand-1", async () => {
    let x = ty.symbol('x');
    let plus = ty.symbol('+');
    let setq = ty.symbol('setq');
    let lp = ty.list([setq, x, ty.list([plus, x, ty.integer(1)])]);

    let env = new elisp.Environment();
    await elisp.eval_text("(fset 'inc '(macro lambda (v) (list 'setq v (list '+ v 1))))", env);
    let result = await elisp.eval_text("(macroexpand-1 '(inc x))", env);

    assert.equal(result, lp.to_string());
  });

  it("does defuns", async () => {
    let env = new elisp.Environment();
    let defbody = "(list 'fset (list 'quote name) (list 'quote (list 'lambda args body)))";
    let defun = `(macro lambda (name args body) ${defbody})`;
    await elisp.eval_text(`(fset 'defun '${defun})`, env);
    await elisp.eval_text("(defun sqr (x) (* x x))", env);
    assert.equal(await elisp.eval_text("(sqr 23)", env), 529);
  });

  it("does defmacro", async () => {
    let env = new elisp.Environment();
    let defbody = "(list 'fset (list 'quote name) (list 'quote (list 'macro 'lambda args body)))";
    let defmacro = `(macro lambda (name args body) ${defbody})`;
    await elisp.eval_text(`(fset 'defmacro '${defmacro})`, env);
    await elisp.eval_text("(defmacro test () :yep)", env);
    assert.equal(await elisp.eval_text("(test)", env), ":yep");
  });

  it("fset's macro in progn", () => {
    let code = `
      (fset 'twice '(macro lambda (fn arg) (list fn (list fn arg))))
      (fset 'sqr (lambda (x) (* x x)))
      (twice sqr 2)
    `;
    return assertEvalTop(code, 16);
  });

  it("macroexpand: macro before defun", () => {
    let code = `
      (fset 'macro1 '(macro lambda () :before))
      (fset 'test (lambda () (macro1)))
      (test)
      (fset 'macro1 '(macro lambda () :after))
      (test)
    `;
    return assertEvalTop(code, ":before");
  });

  it("macroexpand: macro after defun", () => {
    let code = `
      (fset 'test (lambda () (macro1)))
      (fset 'macro1 '(macro lambda () :after1))
      (let ((ret1 (test)))
         (fset 'macro1 '(macro lambda () :after2))
         (cons ret1 (test))) `;

    return assertEvalTop(code, "(:after1 . :after2)");
  });
});
