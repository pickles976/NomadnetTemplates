#!/usr/bin/env -S csi -s

;;; index.mu - Main page using ORM-based comments

;; Paths relative to workspace root (where this is run from)
(load "framework/micron.scm")
(load "app/templates/comments.scm")

;; Configuration
(define db-path "app/app.db")
(define page-name "index")

(define (my-input-field label fieldname size)
  (conc (style '(bg "333" fg "aaa")) (input-field-fixed fieldname size) (reset-style) label newline))

;; Generate the page
(print

  squiggle newline
  (center
    (bold "Hello! ")
    "This is output from "
    (italics "micron")
    " with "
    (bold "ORM-powered comments"))
  newline squiggle newline

  (section "Main")
    (style '(align left))
"This is a demo page showing the new ORM-based comment system.
Comments are now stored in SQLite and queried efficiently!"
    newline
    (style '(fg "33f"))
    (link "lxmf@exampleaddress" "Link Example")
    (style '(fg "aaa"))
    newline

    (style '(align center)) newline

    (subsection "Comments")
      (display-comments db-path page-name)
      newline

    (subsection "Leave a Comment")
      (style '(fg "aaa" align left))
      (my-input-field  " Name " "user_name" 16) newline
      (my-input-field  " LXMF Address (optional)" "user_lxmf" 32) newline
      (my-input-field  " Comment " "comment_text" 64) newline

      (style '(bg "373"))
      ;; label, link, page name, fields
      (submit-field "Submit" "/app/actions/handle_comment.scm" page-name "user_name" "user_lxmf" "comment_text")
      (reset-style))
