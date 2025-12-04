#!/usr/bin/env -S csi -s

;;; demo.scm - Complete ORM demonstration
;;;
;;; This shows the full workflow:
;;; 1. Models defined in models.scm (post, comment, user)
;;; 2. Constructors automatically generated
;;; 3. Instances created and saved

(load "orm-lib.scm")

(print "╔════════════════════════════════════════╗")
(print "║   Nomadnet ORM - Complete Demo        ║")
(print "╚════════════════════════════════════════╝")
(newline)

(print "Models defined in models.scm:")
(print "  - post")
(print "  - comment")
(print "  - user")
(newline)

(print "Constructors automatically generated:")
(print "  - make-post")
(print "  - make-comment")
(print "  - make-user")
(newline)

;; Open database
(db-open "app.db")

;; Create and save a post
(print "Creating a post...")
(define blog-post
  (make-post
    '((title . "Welcome to Nomadnet")
      (content . "This is my first post on the mesh network!")
      (author . "Alice")
      (created-at . "2025-12-04 18:00"))))
(db-save blog-post)

;; Create and save comments
(print "Adding comments...")
(define comment1
  (make-comment
    '((name . "Bob")
      (address . "")
      (page-name . "welcome")
      (timestamp . "2025-12-04 18:15")
      (text . "Great post, Alice!"))))
(db-save comment1)

(define comment2
  (make-comment
    '((name . "Charlie")
      (address . "a1b2c3d4e5f6")
      (page-name . "welcome")
      (timestamp . "2025-12-04 18:30")
      (text . "Thanks for sharing!"))))
(db-save comment2)

;; Create and save a user
(print "Registering a user...")
(define new-user
  (make-user
    '((username . "alice")
      (email . "alice@mesh.local")
      (created-at . "2025-12-04 17:00"))))
(db-save new-user)

;; Close database
(db-close)

(newline)
(print "✓ Demo complete!")
(newline)
(print "View data:")
(print "  sqlite3 app.db 'SELECT * FROM post;'")
(print "  sqlite3 app.db 'SELECT * FROM comment;'")
(print "  sqlite3 app.db 'SELECT * FROM user;'")
