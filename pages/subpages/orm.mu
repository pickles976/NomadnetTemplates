#!/usr/bin/env -S csi -s

;;; orm.mu - ORM Guide

(import micron)
(import markdown)
(load "/home/pi/.nomadnetwork/storage/pages/app/settings.scm")
(load "/home/pi/.nomadnetwork/storage/pages/app/templates/comments.scm")
(load "/home/pi/.nomadnetwork/storage/pages/app/templates/subpage-header.scm")
(define page-name "orm")

(print
  (subpage-header "ORM Guide")

  ;; Load content from markdown
  (markdown-file->micron "/home/pi/.nomadnetwork/storage/pages/markdown/orm.md")
  nl

  (comment-section page-name))
