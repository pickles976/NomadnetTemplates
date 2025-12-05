#!/usr/bin/env -S csi -s

;;; header.scm - Reusable header template for Macron pages

(import (chicken string))

;; Load micron DSL
(load "framework/micron.scm")

;; ========== HEADER COMPONENT ==========

(define (page-header #!optional (title "MACRON") (subtitle "Interactive Micron Apps Made Easy"))
  "Generate a styled header with title and subtitle

   Parameters:
     title    - Main title text (default: 'MACRON')
     subtitle - Subtitle text (default: 'Interactive Micron Apps Made Easy')

   Returns:
     Micron markup string for the header"
  (conc
    squiggle newline
    (center
      (bold (style '(fg "3af")) title (reset-style))
      newline
      (italics subtitle))
    newline squiggle newline))

(define (macron-header)
  "Generate the default Macron framework header"
  (page-header "MACRON" "Interactive Micron Apps Made Easy"))
