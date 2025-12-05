#!/usr/bin/csi -script

;;; handle_comment.scm - Save comment to database using ORM

(import srfi-19)
(import (chicken process-context))
(import (chicken string))

;; Path relative to workspace root
(load "framework/orm-lib.scm")

;; Get form data from environment variables
(define user-name (or (get-environment-variable "field_user_name") "Anonymous"))
(define user-lxmf (or (get-environment-variable "field_user_lxmf") ""))
(define comment-text (or (get-environment-variable "field_comment_text") ""))
(define post-id (or (get-environment-variable "var_post_id") "default"))

;; Sanitize input (basic example - replace backticks)
(define sanitized-name (string-translate user-name #\` #\'))
(define sanitized-text (string-translate comment-text #\` #\'))
(define sanitized-lxmf (string-translate user-lxmf #\` #\'))

;; Generate timestamp
(define timestamp (date->string (current-date) "~Y-~m-~d ~H:~M"))

;; Create comment instance
(define new-comment
  (make-comment
    `((name . ,sanitized-name)
      (address . ,sanitized-lxmf)
      (page-name . ,post-id)
      (timestamp . ,timestamp)
      (text . ,sanitized-text))))

;; Database path - relative to workspace root
(define db-path "app/app.db")

;; Save to database
(db-open db-path)
(db-save new-comment)
(db-close)

;; Display success message and redirect
(display "`c`!Thank you for your comment!`!`a\n")
(display "-\n")
(display "Your comment has been submitted successfully.\n")
(display "`F00a`_`[Return to blog`:/page/index.mu]`_`f\n")
