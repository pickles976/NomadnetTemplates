#!/usr/bin/env -S csi -s

;;; test-auto-constructor.scm
;;; Demonstrates that constructors are automatically generated
;;;
;;; We added 'user-model' to models.scm but did NOT manually create make-user
;;; The ORM should create it automatically!

(load "orm-lib.scm")

(print "=== Testing Automatic Constructor Generation ===")
(print "")
(print "We added 'user' model to models.scm")
(print "But we did NOT manually write make-user in orm-lib.scm")
(print "The ORM generates it automatically!")
(print "")

;; This should work without any manual code changes
(define new-user
  (make-user
    '((username . "alice")
      (email . "alice@example.com")
      (created-at . "2025-12-04 17:00"))))

(print "✓ make-user exists and works!")
(print "Created: " new-user)
(newline)

;; Let's save it to the database
(db-open "app.db")
(db-save new-user)
(db-close)

(print "✓ Saved to database!")
(print "")
(print "Verify with: sqlite3 app.db 'SELECT * FROM user;'")
