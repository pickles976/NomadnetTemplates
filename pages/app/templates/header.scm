#!/usr/bin/env -S csi -s

;;; header.scm - Reusable header template for Macron pages

(import (chicken string))
(import micron)

;; ========== HEADER COMPONENT ==========

(define (page-header #!optional (title "MACRON") (subtitle "Tools for building Nomadnet apps"))
  "Generate a styled header with title and subtitle

   Parameters:
     title    - Main title text (default: 'MACRON')
     subtitle - Subtitle text (default: 'Tools for building Nomadnet apps')

   Returns:
     Micron markup string for the header"
  (conc
    squiggle nl
    "`c`!`F3af" title "`!" nl
    "`*" subtitle "`*" nl
    "`Fddd" nl
    squiggle nl))

(define (macron-header)
  "Generate the default Macron framework header"
  (page-header "MACRON" "Tools for building Nomadnet apps"))
