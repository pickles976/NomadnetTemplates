#!/usr/bin/env -S csi -s

;;; chicken-scheme-basics.mu - Introduction to Chicken Scheme

(import micron)
(import markdown)
(load "/home/pi/.nomadnetwork/storage/pages/app/settings.scm")
(load "/home/pi/.nomadnetwork/storage/pages/app/templates/comments.scm")
(load "/home/pi/.nomadnetwork/storage/pages/app/templates/subpage-header.scm")
(define page-name "chicken-scheme-basics")

(print
  (subpage-header "Chicken Scheme Basics")

  ;; Load content from markdown
  (markdown-file->micron "/home/pi/.nomadnetwork/storage/pages/markdown/chicken-scheme-basics.md")
  nl

  (comment-section page-name))
