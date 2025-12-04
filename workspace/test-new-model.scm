#!/usr/bin/env -S csi -s

;;; test-new-model.scm - Test that new models get constructors automatically
;;;
;;; We'll add a 'user' model to models.scm and verify make-user works

(import scheme)
(import (chicken base))

;; First, let's manually add a user model to test
;; (In practice, you'd edit models.scm directly)

(define user-model
  '((name . user)
    (fields . (
      ((name . id)
       (type . integer)
       (primary-key . #t)
       (autoincrement . #t))
      ((name . username)
       (type . text)
       (size . 32))
      ((name . email)
       (type . text))
      ((name . created-at)
       (type . text)
       (size . 20))))))

;; Load existing models and add user
(load "models.scm")
(set! all-models (cons user-model all-models))

;; Now load orm-lib (which will generate constructors)
(load "orm-lib.scm")

;; Test that make-user was automatically created
(print "=== Testing Automatic Constructor Generation ===")
(print "Available constructors: make-comment, make-post, make-user")
(newline)

(define new-user
  (make-user
    '((username . "alice")
      (email . "alice@example.com")
      (created-at . "2025-12-04 17:00"))))

(print "Created user instance:")
(print new-user)
(newline)

(print "Success! make-user was automatically generated!")
