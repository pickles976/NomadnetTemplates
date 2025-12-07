#!/usr/bin/env -S csi -s

;;; micron-dsl.mu - Micron DSL Reference

(import micron)
(import markdown)
(load "/home/pi/.nomadnetwork/storage/pages/app/settings.scm")
(load "/home/pi/.nomadnetwork/storage/pages/app/templates/comments.scm")
(load "/home/pi/.nomadnetwork/storage/pages/app/templates/subpage-header.scm")
(define page-name "micron-dsl")

(print
  (subpage-header "Micron DSL")

  ;; Load content from markdown
  (markdown-file->micron "/home/pi/.nomadnetwork/storage/pages/markdown/micron-dsl.md")
  nl

  (comment-section page-name))
