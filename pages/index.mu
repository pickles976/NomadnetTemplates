#!/usr/bin/env -S csi -s

;;; index.mu - Macron Framework Homepage

;; Paths relative to workspace root (where this is run from)
(load "framework/micron.scm")
(load "framework/markdown.scm")
(load "app/templates/header.scm")
(load "app/templates/comments.scm")

;; Configuration
(define db-path "app/app.db")
(define page-name "index")

(define (my-input-field label fieldname size)
  (conc (style '(bg "333" fg "aaa")) (input-field-fixed fieldname size) (reset-style) label newline))

;; Generate the page
(print

  (macron-header)

  ;; Welcome section
  (style '(align left))
  newline

  newline
  (section "Welcome to Macron")
  newline

    (style '(fg "ddd"))
    "This is a framework written (mostly by claude) to allow for the easy"
    newline
    "construction of " (bold "interactive pages") " on nomadnet."
    newline newline

    (style '(fg "5af"))
    (link "https://github.com/pickles976/Macro" "View on GitHub")
    (style '(fg "ddd"))
    newline newline

    "I found that there was not enough easy to use tooling around nomadnet"
    newline
    "and micron, and I hope that macron helps bridge that gap."
    newline newline

    (style '(fg "5af"))
    (link "/file/macron.tar.gz" "Download macron.tar.gz")
    (style '(fg "ddd"))
    newline

  ;; Built for Learning section
  newline
  (subsection "Built for Learning")
  newline

    (style '(fg "ddd" align left))
    newline
    "The " (code "docs") " folder has a bunch of interactive scheme files that claude"
    newline
    "generated. They do a pretty good job at explaining how everything works."
    newline

  ;; Installation instructions from markdown
  (style '(align left))
  newline
  (md-file->micron "app/markdown/index.md")
  newline

  ;; Comments section
  newline
  (section "Community Discussion")
  newline

    (style '(align left fg "ddd"))
    newline
    (display-comments db-path page-name)
    newline

  newline
  (subsection "Leave a Comment")
  newline

    (style '(fg "aaa" align left))
    newline
    (my-input-field  " Name " "user_name" 16) newline
    (my-input-field  " LXMF Address (optional)" "user_lxmf" 32) newline
    (my-input-field  " Comment " "comment_text" 64) newline

    (style '(bg "373"))
    ;; label, link, page name, fields
    (submit-field "Submit" "/app/actions/handle_comment.scm" page-name "user_name" "user_lxmf" "comment_text")
    (reset-style))
