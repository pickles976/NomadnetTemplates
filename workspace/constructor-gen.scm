#!/usr/bin/env -S csi -s

;;; constructor-gen.scm - Learning about dynamic function generation
;;;
;;; We want to automatically create functions like:
;;;   (define (make-comment fields) ...)
;;;   (define (make-post fields) ...)
;;;
;;; from model definitions

(import scheme)
(import (chicken base))
(import (chicken string))

;; ========== EXAMPLE 1: Manual Eval ==========

;;; The 'eval' function takes Scheme code (as a list) and executes it
;;; Think of it like running "eval()" in JavaScript or Python
;;;
;;; For example:
;;;   (eval '(+ 1 2))  ; Returns 3
;;;   (eval '(define x 10))  ; Creates variable x

(print "=== Example 1: Basic Eval ===")
(eval '(define greeting "Hello from eval!"))
(print greeting)
(newline)

;; ========== EXAMPLE 2: Building Code with Quasiquote ==========

;;; Quasiquote (`) is like quote ('), but allows interpolation with comma (,)
;;;
;;; Compare:
;;;   '(+ a b)      ; Literal list: (+ a b)
;;;   `(+ ,a ,b)    ; If a=1, b=2, produces: (+ 1 2)
;;;
;;; This is similar to template strings in JavaScript: `Hello ${name}`

(define name "Alice")
(define code `(define message (conc "Hello, " ,name)))

(print "=== Example 2: Quasiquote ===")
(print "Code to eval: " code)
(eval code)
(print message)
(newline)

;; ========== EXAMPLE 3: Creating a Function Dynamically ==========

;;; Now let's create a function dynamically
;;; We'll build the (define (func-name ...) ...) form as a list

(define (create-greeter name)
  "Dynamically create a greeter function"
  (let ((func-name (string->symbol (conc "greet-" name))))
    (eval `(define (,func-name)
             (print "Hello from " ,name "!")))))

(print "=== Example 3: Dynamic Function Creation ===")
(create-greeter "Alice")
(create-greeter "Bob")

;; Now we have two functions: greet-Alice and greet-Bob
(greet-Alice)
(greet-Bob)
(newline)

;; ========== EXAMPLE 4: Constructor Pattern ==========

;;; This is the pattern we'll use for our ORM
;;; Create a make-* function for a given model name

(define (make-instance model-name fields)
  "Helper that creates an instance"
  (cons `(__model__ . ,model-name) fields))

(define (generate-constructor model-name)
  "Generate a make-<model> constructor function"
  ;;;
  ;;; For model-name 'comment, this creates:
  ;;;   (define (make-comment fields)
  ;;;     (make-instance 'comment fields))

  (let ((constructor-name (string->symbol (conc "make-" (symbol->string model-name)))))
    (print "Generating constructor: " constructor-name)
    (eval `(define (,constructor-name fields)
             (make-instance ',model-name fields)))))

(print "=== Example 4: Constructor Generation ===")
(generate-constructor 'comment)
(generate-constructor 'post)

;; Now test them
(define my-comment (make-comment '((text . "Test"))))
(define my-post (make-post '((title . "Hello"))))

(print "Comment: " my-comment)
(print "Post: " my-post)
