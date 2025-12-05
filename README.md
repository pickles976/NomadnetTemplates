# Macron

Tools for building Nomadnet apps in Chicken Scheme. Includes an ORM, a micron DSL, and a markdown converter.

## Project Structure

```
workspace/
├── src/                    # Source code
│   ├── orm.scm            # CLI tool for table generation
│   ├── orm-lib.scm        # Runtime ORM library
│   ├── models.scm         # Model definitions
│   └── micron-dsl.scm     # Micron markup DSL
│
├── pages/                  # Example Nomadnet page
│   ├── index.mu           # Main page (ORM-powered comments)
│   └── app/
│       ├── actions/       # Form handlers
│       │   └── handle_comment.scm
│       └── templates/     # Page templates
│           └── comments.scm
│
├── docs/                   # Documentation and examples
│   ├── README.md          # Complete learning guide
│   ├── QUICK_START.md     # Quick reference
│   ├── AUTO_CONSTRUCTORS.md
│   ├── DB_CONNECTION_EXPLAINED.md
│   ├── DB_LIST_EXPLAINED.md
│   ├── PARAMETER_SUMMARY.md
│   └── *.scm             # Runnable examples
│
├── app.db                 # SQLite database
└── README.md             # This file
```

## Quick Start

### 1. Generate Database Tables

```bash
cd workspace
csi -s src/orm.scm --generate
```

### 2. Run Example Page

```bash
csi -s pages/index.mu
```

### 3. Try Examples

```bash
# Run any example from docs/
csi -s docs/full-crud-demo.scm
csi -s docs/test-db-list.scm
```

## Core API

### Models (src/models.scm)

Define your data structures as alists:

```scheme
(define comment-model
  '((name . comment)
    (fields . (
      ((name . id)
       (type . integer)
       (primary-key . #t)
       (autoincrement . #t))
      ((name . text)
       (type . text))))))

(define all-models (list comment-model ...))
```

### Create (INSERT)

```scheme
(load "src/orm-lib.scm")

(db-open "app.db")

;; Constructors are auto-generated from models!
(define my-comment
  (make-comment
    '((name . "Alice")
      (page-name . "index")
      (text . "Hello!"))))

(db-save my-comment)

(db-close)
```

### Read (SELECT)

```scheme
;; Get all
(db-list 'comment)

;; Filter by field
(db-list 'comment '((page-name . "index")))

;; Multiple filters (AND)
(db-list 'comment '((name . "Alice") (page-name . "index")))
```

### Working with Results

Results are alists - easy to process:

```scheme
(define comments (db-list 'comment '((page-name . "index"))))

;; Access fields
(alist-ref 'name (car comments))
(alist-ref 'text (car comments))

;; Map/filter
(map (lambda (c) (alist-ref 'text c)) comments)
(filter (lambda (c) (> (alist-ref 'id c) 5)) comments)
```

## What's Included

- Automatic table generation from model definitions
- Auto-generated constructors - add a model, get `make-<model>`
- INSERT with `db-save`
- SELECT with `db-list` and filtering
- Parameterized queries
- Thread-safe database connection
- Micron DSL for generating micron markup
- Markdown to micron converter

## Example

`pages/` contains a working Nomadnet page:

- `index.mu` - Main page that displays comments from database
- `comments.scm` - Template that queries and renders comments
- `handle_comment.scm` - Form handler that saves new comments

## Documentation

`docs/` contains:

- `README.md` - Learning guide
- `QUICK_START.md` - API reference
- `AUTO_CONSTRUCTORS.md` - How code generation works
- `DB_CONNECTION_EXPLAINED.md` - Parameters
- `DB_LIST_EXPLAINED.md` - SELECT queries

## Examples

`docs/` has runnable examples:

- `full-crud-demo.scm` - Create + Read
- `test-db-list.scm` - Query tests
- `nomadnet-page-example.scm` - Page rendering
- `parameters-explained.scm` - Parameters
- `constructor-gen.scm` - Code generation

## Scheme Concepts Used

- Association lists (alists)
- Parameters
- Quasiquote & eval
- Variadic functions
- Higher-order functions (map, filter, for-each)
- SQL integration

## Installation

### Prerequisites

- Chicken Scheme 5.x
- SRFI eggs: srfi-1, srfi-13, srfi-19
- sql-de-lite egg

### Quick Install

1. **Clone the repository**
   ```bash
   git clone https://github.com/pickles976/Macro.git
   cd Macro
   ```

2. **Install Chicken Scheme dependencies**
   ```bash
   sudo chicken-install sql-de-lite srfi-1 srfi-13 srfi-19
   ```

3. **Build and install Macron modules**
   ```bash
   cd pages/framework

   # Build the modules
   csc -s micron.scm
   csc -s markdown.scm
   csc -s orm-lib.scm

   # Install system-wide (optional)
   sudo chicken-install -s micron.egg
   sudo chicken-install -s markdown.egg
   sudo chicken-install -s orm.egg

   cd ../..
   ```

   Then use them anywhere:
   ```scheme
   (import micron)
   (import markdown)
   (import orm)
   ```

## Modules

### Micron (`micron`)

DSL for generating micron markup.

```scheme
(import micron)

(print
  (center (bold "Welcome to Macron"))
  newline
  (link "https://github.com/pickles976/Macro" "View on GitHub"))
```

### Markdown (`markdown`)

Converts markdown to micron.

```scheme
(import markdown)

(define content (md-file->micron "content.md"))
(print content)
```

### ORM (`orm`)

ORM for SQLite with auto-generated constructors.

```scheme
(import orm)

(orm-init "app/models.scm")
(db-open "app.db")

(db-save (make-comment '((name . "Alice") (text . "Hello!"))))
(define comments (db-list 'comment))

(db-close)
```

## License

MIT

## Usage

1. Read `docs/README.md`
2. Run examples in `docs/`
3. Modify `src/models.scm` to add models
4. Build your app
