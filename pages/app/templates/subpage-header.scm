#!/usr/bin/env -S csi -s

;;; subpage-header.scm - Reusable header for subpages with back-to-home link

(import micron)

;; ========== SUBPAGE HEADER COMPONENT ==========

(define (subpage-header title)
  "Generate a styled header for subpages with title and back-to-home link

   Parameters:
     title - The page title to display

   Returns:
     Micron markup string for the subpage header"
  (conc
    (style '(align left fg "5af"))
    (file-link "/page/index.mu" "← Back to Home")
    (reset-style)
    nl nl
    (section title)
    (style '(align left))
    nl))
