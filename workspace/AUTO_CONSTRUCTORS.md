# Automatic Constructor Generation - Explained

## The Problem We Solved

**Before:** Every time you added a model to `models.scm`, you had to manually write:

```scheme
;; In orm-lib.scm
(define (make-comment fields)
  (make-instance 'comment fields))

(define (make-post fields)
  (make-instance 'post fields))

(define (make-user fields)  ; Forgot to add this? Error!
  (make-instance 'user fields))
```

This was tedious and error-prone.

**After:** Just add the model definition - the constructor is auto-generated!

```scheme
;; Only need to edit models.scm
(define all-models
  (list post-model comment-model user-model))  ; Done!
```

## How It Works

### Step 1: Understanding Quasiquote

Quasiquote (`) is like a template that allows variable interpolation with comma (,):

```scheme
;; Regular quote - everything is literal
'(define x 10)
; Result: (define x 10)

;; Quasiquote - can interpolate variables
(define name 'greeting)
`(define ,name "hello")
; Result: (define greeting "hello")
```

This is similar to template literals in JavaScript:
```javascript
`Hello ${name}!`
```

### Step 2: Building Code as Data

Scheme treats code as data (lists). We can build code programmatically:

```scheme
(define func-name 'greet)
(define code `(define (,func-name) (print "Hello!")))
; code is now: (define (greet) (print "Hello!"))
```

### Step 3: Executing Code with Eval

`eval` takes Scheme code (as a list) and executes it:

```scheme
(eval '(define x 10))    ; Creates variable x
(eval '(+ 1 2))          ; Returns 3

;; Combined with quasiquote:
(define var-name 'greeting)
(eval `(define ,var-name "hi"))
; Now 'greeting' exists and equals "hi"
```

### Step 4: Our Constructor Generator

Putting it all together:

```scheme
(define (generate-constructor model-name)
  ;; 1. Build function name: 'comment -> 'make-comment
  (let ((constructor-name
          (string->symbol (conc "make-" (symbol->string model-name)))))

    ;; 2. Build the function definition as code
    ;;    `(define (make-comment fields) ...)
    (eval `(define (,constructor-name fields)
             (make-instance ',model-name fields)))))
```

**What this does:**
- For `model-name = 'comment`
- Builds function name: `'make-comment`
- Creates code: `(define (make-comment fields) (make-instance 'comment fields))`
- Evaluates it, making `make-comment` globally available

### Step 5: Generating All Constructors

Loop through all models and generate each constructor:

```scheme
(define (generate-all-constructors)
  (load "models.scm")
  (for-each
    (lambda (model)
      (let ((model-name (alist-ref 'name model)))
        (generate-constructor model-name)))
    all-models))

;; Run when library loads
(generate-all-constructors)
```

## Key Learning Points

### 1. Code as Data (Homoiconicity)

In Scheme, code is just lists. This means we can:
- Build code programmatically
- Transform code
- Generate code from data

```scheme
;; These are equivalent:
(+ 1 2)              ; Code
'(+ 1 2)             ; Data (a list)
(eval '(+ 1 2))      ; Data → Code
```

### 2. Quasiquote for Templates

```scheme
;; Build dynamic code
(define operation '+)
(define x 5)
(define y 10)

`(,operation ,x ,y)
; Result: (+ 5 10)

(eval `(,operation ,x ,y))
; Result: 15
```

### 3. Symbol Manipulation

Convert between symbols and strings to build names:

```scheme
(symbol->string 'hello)           ; "hello"
(string->symbol "world")          ; world
(string->symbol (conc "make-" "user"))  ; make-user
```

### 4. Metaprogramming

Writing code that writes code:

```scheme
;; Instead of:
(define (make-x ...) ...)
(define (make-y ...) ...)

;; We write a function that creates these functions
(for-each generate-constructor '(x y))
```

## Benefits

1. **Less Boilerplate** - No manual constructor functions
2. **Automatic** - Add model → get constructor for free
3. **Maintainable** - Single source of truth (models.scm)
4. **Extensible** - Easy to add new models

## Testing

See `test-auto-constructor.scm` for a working example:

```bash
cd workspace
csi -s test-auto-constructor.scm
```

This demonstrates that `make-user` exists without being manually coded!

## Comparison to Other Languages

**Python metaclasses:**
```python
class ModelMeta(type):
    def __new__(cls, name, bases, attrs):
        # Automatically create methods
        pass
```

**Ruby method_missing:**
```ruby
def method_missing(name, *args)
  # Dynamically handle make_* calls
end
```

**Scheme eval + quasiquote:**
```scheme
(eval `(define (,name fields) ...))
```

Scheme's approach is simpler and more direct!
