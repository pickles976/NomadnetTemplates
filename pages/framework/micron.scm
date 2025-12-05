#!/usr/bin/env -S csi -s

;;; micron-dsl.scm - DSL for generating Nomad Network Micron markup
;;;
;;; Complete reference: https://rfnexus.github.io/micron-parser-js/

(import scheme)
(import chicken.base)      ; For export, make-parameter, parameterize
(import (chicken string))  ; For string-intersperse, conc
(import srfi-1)           ; For alist-ref, assq
(import srfi-13)          ; For string-null?

;; ========== CONSTANTS ==========

(define squiggle "-∿")
(define newline "  \n")

;; ========== TEXT FORMATTING ==========

(define (bold . content)
  "Bold text: `!text`!"
  (conc "`!" (apply conc content) "`!"))

(define (italics . content)
  "Italic text: `*text`*"
  (conc "`*" (apply conc content) "`*"))

(define (underline . content)
  "Underlined text: `_text`_"
  (conc "`_" (apply conc content) "`_"))

;; Combined formatting helpers
(define (bold-italic . content)
  "Bold and italic: `!`*text`*`!"
  (conc "`!`*" (apply conc content) "`*`!"))

(define (bold-underline . content)
  "Bold and underlined: `!`_text`_`!"
  (conc "`!`_" (apply conc content) "`_`!"))

;; ========== COLORS ==========

(define (fg color . content)
  "Set foreground color: `Fxxx where xxx is 3 hex chars"
  (conc "`F" color (apply conc content)))

(define (bg color . content)
  "Set background color: `Bxxx where xxx is 3 hex chars"
  (conc "`B" color (apply conc content)))

(define (colored fg-color bg-color . content)
  "Set both foreground and background colors"
  (conc "`F" fg-color "`B" bg-color (apply conc content)))

(define (reset-colors)
  "Reset to default colors: `f`b"
  "`f`b")

;; ========== ALIGNMENT ==========

(define (left . content)
  "Left-align text: `a"
  (conc "`a" (apply conc content)))

(define (center . content)
  "Center-align text: `c"
  (conc "`c" (apply conc content)))

(define (right . content)
  "Right-align text: `r"
  (conc "`r" (apply conc content)))

(define (align-default)
  "Reset to default alignment: `a"
  "`a")

;; ========== SECTIONS & STRUCTURE ==========

(define (section . content)
  "First-level section: > Title"
  (conc "> " (apply conc content) "\n"))

(define (subsection . content)
  "Second-level section: >> Title"
  (conc ">> " (apply conc content) "\n"))

(define (subsubsection . content)
  "Third-level section: >>> Title"
  (conc ">>> " (apply conc content) "\n"))

(define (section-depth depth . content)
  "Section at arbitrary depth"
  (let ((heading (make-string depth #\>)))
    (conc heading " " (apply conc content) "\n")))

(define (divider)
  "Horizontal divider line: -"
  "-\n")

;; ========== LINKS ==========

(define (link url . label)
  "Hyperlink: `[label`url]"
  (let ((link-text (if (null? label) url (apply conc label))))
    (conc "`[" link-text "`" url "]")))

;; ========== LITERAL/CODE BLOCKS ==========

(define (literal . content)
  "Literal block (no parsing): `=text`="
  (conc "`=" (apply conc content) "`="))

(define (code . content)
  "Alias for literal - displays code without parsing"
  (apply literal content))

;; ========== COMMENTS ==========

(define (comment text)
  "Comment line (not displayed): # comment"
  (conc "# " text "\n"))

;; ========== FORM INPUTS ==========

(define (input-field name)
  "Variable-width input field: `<name`>"
  (conc "`<" name "`>"))

(define (input-field-fixed name size)
  "Fixed-width input field: `<size|name`>"
  (conc "`<" size "|" name "`>"))

(define (input-password name size)
  "Password input (masked): `<size|name|*`>"
  (conc "`<" size "|" name "|*`>"))

(define (checkbox name . label)
  "Checkbox: `{name`}"
  (let ((display-label (if (null? label) name (apply conc label))))
    (conc "`{" name "`} " display-label)))

(define (radio name value . label)
  "Radio button: `(name=value`)"
  (let ((display-label (if (null? label) value (apply conc label))))
    (conc "`(" name "=" value "`) " display-label)))

(define (submit-field label destination page . field-names)
  "Submit button with fields: `[label`:destination`field1|field2|var=value]"
  (let ((fields-str (if (null? field-names)
                        ""
                        (string-intersperse field-names "|"))))
    (if (string-null? fields-str)
        (conc "`[" label "`:" destination "`post_id=" page "]")
        (conc "`[" label "`:" destination "`" fields-str "|post_id=" page "]"))))

;; ========== STYLE COMBINATIONS ==========

(define (style attrs . body-elements)
  "Apply multiple style attributes at once

  Usage: [style '[fg \"33f\" bg \"000\" align center] \"text\"]

  Valid attrs:
    fg - foreground color (3 hex chars)
    bg - background color (3 hex chars)
    align - left, center, right, or default"
  (let loop ((attrs attrs) (output ""))
    (if (null? attrs)
        (conc output (apply conc body-elements))
        (let ((key (car attrs))
              (value (cadr attrs))
              (rest (cddr attrs)))
          (case key
            ((fg) (loop rest (conc output "`F" value)))
            ((bg) (loop rest (conc output "`B" value)))
            ((align) (case value
                      ((left) (loop rest (conc output "`a")))
                      ((center) (loop rest (conc output "`c")))
                      ((right) (loop rest (conc output "`r")))
                      ((default) (loop rest (conc output "`a")))
                      (else (loop rest output))))
            (else (loop rest output)))))))

(define (reset-style)
  "Reset all formatting: ``"
  "``")

(define (reset-all)
  "Reset formatting, colors, and alignment"
  "```f`b`a")

;; ========== PRESETS ==========

;; Common color presets (3-char hex)
(define color-red "f00")
(define color-green "0f0")
(define color-blue "00f")
(define color-yellow "ff0")
(define color-cyan "0ff")
(define color-magenta "f0f")
(define color-white "fff")
(define color-black "000")
(define color-gray "888")
(define color-dark-gray "444")
(define color-light-gray "ccc")

;; Semantic colors
(define color-success "0a0")
(define color-warning "fa0")
(define color-error "f33")
(define color-info "0af")

;; Helper for common patterns
(define (error-text . content)
  "Red error text"
  (apply fg color-error content))

(define (success-text . content)
  "Green success text"
  (apply fg color-success content))

(define (warning-text . content)
  "Orange warning text"
  (apply fg color-warning content))

(define (info-text . content)
  "Blue info text"
  (apply fg color-info content))

(define (muted-text . content)
  "Gray muted text"
  (apply fg color-gray content))

;; ========== UTILITIES ==========

(define (spacer n)
  "Insert n blank lines"
  (apply conc (make-list n "\n")))

(define (indent level . content)
  "Indent content by spaces (2 spaces per level)"
  (let ((spaces (make-string (* 2 level) #\space)))
    (conc spaces (apply conc content))))

;; ========== COMMON PATTERNS ==========

(define (heading level . content)
  "Create a centered, bold heading"
  (center (apply bold content)))

(define (button label destination . fields)
  "Styled button (colored submit field)"
  (apply submit-field label destination fields))

(define (link-button url . label-content)
  "Link styled as a button"
  (let ((label (if (null? label-content) url (apply conc label-content))))
    (conc "`F00a`_" (link url label) "`_`f")))

;; ========== CONVENIENCE ==========

;; Re-export common constants for easy access
(define nl newline)
(define hr divider)
