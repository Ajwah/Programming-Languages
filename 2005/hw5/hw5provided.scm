;; A CSE341 Mini-Scheme Interpreter for the language "MiniScheme"
;; Skeleton Code
;; Be sure you understand this code before writing your solutions.  This will
;; save you a lot of time since most of the code is implemented for you!


;;---------------DO NOT MODIFY CODE BELOW HERE------------------------------------

;;helper function -- removes a variable from an environment,
;;returning a copy of the environment with that variable instance(s) removed.
;;Allows us to implement scheme-like define semantics of "mutate the existing
;;binding if there is one"
(define (env-copy-remove e v)
  (cond ((null? e)
         '())
        (else
         (if (eq? (caar e) v)
             (env-copy-remove (cdr e) v)
             (cons (car e)
                   (env-copy-remove (cdr e) v))))))

;;Updates the inputted environment with a new MiniScheme variable and its value.
;;Use this function for evaluating Define sentences
(define (add-define! var val env)
  (let ((tmp (env-copy-remove env var))) ;;tmp has curr env with var removed
    (set-car! env (list var val))
    (set-cdr! env tmp)
    'ok))

;;returns true if the argument is a minischeme constant
(define constant?
  (lambda (expr)
    (or (boolean? expr) (number? expr))))

;;returns true if the argument is a minischeme variable
(define variable?
  (lambda (expr)
    (symbol? expr)))

;;Makes a MiniScheme closure out of a lambda's arguments, body, and environment.
;;A closure is represented as a list where the first value is 'closure,
;;the second is the lambda's arguments, 3rd is the lambda's body, and 4th is
;;the environment the lambda was defined in.
(define make-closure
  (lambda (args body env)
    (list 'closure args body env)))

;;returns true if the argument is a MiniScheme closure
(define closure?
  (lambda (expr)
    (and (pair? expr)
         (eq? (car expr) 'closure))))

;;returns the lambda's arguments from minischeme closure
(define closure-args
  (lambda (func)
    (cadr func)))

;;returns the lambda's body from a minischeme closure
(define closure-body
  (lambda (func)
    (caddr func)))

;;returns the environment from a minischeme closure
(define closure-env
  (lambda (func)
    (cadddr func)))

;;returns true if the argument is a minischeme "if" expression
(define if?
  (lambda (expr)
    (eq? (car expr) 'if)))

;;returns the first minischeme expression in an "if" expression
(define if-condition
  (lambda (expr)
    (cadr expr)))

;;returns the second minischeme expression in an "if" expression
(define if-consequent
  (lambda (expr)
    (caddr expr)))

;;returns the 3rd minischeme expression in an "if" expression
(define if-alternative
  (lambda (expr)
    (cadddr expr)))

;;returns true if the argument is a minischeme "define" sentence
(define define?
  (lambda (expr)
    (eq? (car expr) 'define)))

;;returns the minischeme variable in a "define" sentence
(define define-var
  (lambda (expr)
    (cadr expr)))

;;returns the minischeme value in a "define" sentence
(define define-val
  (lambda (expr)
    (caddr expr)))

;;returns true if the argument is a minischeme "lambda" expression 
(define lambda?
  (lambda (expr)
    (eq? (car expr) 'lambda)))

;;returns the arguments of a minischeme "lambda" expression
(define lambda-args
  (lambda (expr)
    (cadr expr)))

;;returns the body of a minischeme "lambda" expression
(define lambda-body
  (lambda (expr)
    (caddr expr)))

;;returns the first minischeme expression in a function application expression.
;;This had better eval to a closure or a primitive op for evaluation to be correct!
;;example: returns <exp1> from the sentence (<exp_1> <exp_2> ... <exp_n>)
(define application-func
  (lambda (expr)
    (car expr)))

;;returns the rest of the minischeme expressions in a function application expression.
;;example: returns <exp2>...<exp_n> from the sentence (<exp_1> <exp_2> ... <exp_n>)
(define application-args
  (lambda (expr)
    (cdr expr)))

;;The initial global environment.  Maps a number of primitive Scheme functions
;;to MiniScheme variables so that MiniScheme sentences may use these primitive
;;operations.  Uses Scheme quasiquote (shorthand is ` and shorthand for 
;;unquote is ,) to bind the variable names to the Scheme primitive operations.
;;DO NOT MODIFY!
(define global-env 
  `((car ,car) (cdr ,cdr) (cons ,cons) (list, list) (empty_list ,())
               (+ ,+) (- ,-) (* ,*) (/ ,/) (> ,>) (= , =)))

;;---------------------END DO NOT MODIFY--------------------------------------

;;For problem 4, add similar accessor routines for a let expression here
;;TODO

;;Given a MiniScheme expression and an environment, evaluate the expression
;;and return the result.
(define (minischeme-eval expr env)
  (cond ( ;;TODO -- your cases go here, 1 case for each minischeme sentence type
         'NOT_IMPLEMENTED)))

;;Given a MiniScheme value (either a closure or a Scheme primitive), return
;;the result of evaluating this closure or primitive on the input list of vals
(define (minischeme-apply closure_or_prim vals)
  (if (procedure? closure_or_prim)  ;;case for primitve operations!!
      (apply closure_or_prim vals)  ;;just apply the primitive to the vals
      ;;TODO -- your code for applying minischeme closures goes here
      'NOT_IMPLEMENTED))

;;The main REP loop for the MiniScheme interpreter.  Execution of the interpreter 
;;starts by executing this function.  Feel free to play with this function,
;;but your final solution must NOT MODIFY it.
(define MiniScheme
  (lambda ()
    (letrec
      ((loop 
        (lambda ()
          (display "$ ")
          (let ((input (read)))  ;;read is your simple parser
            (cond ((equal? input 'exit) ;;exit if "exit" is typed
                   (void))
                  (else
                   ;;display result of evaluating this sentence
                   (display (minischeme-eval input global-env)) 
                   (newline)
                   (loop)))))))
      (display "Welcome to MiniScheme!")
      (newline)
      (loop))))