#!/usr/bin/env -S csi -s

;;; markdown.scm - Simple Markdown to Micron converter
;;;
;;; Converts basic markdown syntax to micron markup
;;; Not comprehensive - handles common patterns

(import scheme)
(import (chicken base))
(import (chicken string))
(import (chicken io))
(import (chicken file))
(import (chicken irregex))  ; For regex
(import srfi-1)
(import srfi-13)

;; Load the micron DSL for cleaner output generation
(load "framework/micron.scm")

;; ========== UTILITIES ==========

(define (read-file-contents path)
  "Read entire file as string"
  (call-with-input-file path
    (lambda (port)
      (read-string #f port))))

;; ========== LINE-BY-LINE CONVERTERS ==========

(define (convert-heading line)
  "Convert # Heading to > Heading"
  (let ((match (irregex-search '(: bol (** 1 6 #\#) (+ space) (submatch (* any))) line)))
    (if match
        (let* ((hashes (irregex-match-substring match 0))
               (depth (string-count hashes #\#))
               (text (irregex-match-substring match 1)))
          (section-depth depth text))
        line)))

(define (convert-horizontal-rule line)
  "Convert --- or *** to -"
  (if (irregex-search '(: bol (or (>= 3 #\-) (>= 3 #\*) (>= 3 #\_)) (* space) eol) line)
      (divider)
      line))

(define (convert-inline-formatting text)
  "Convert inline markdown: **bold**, *italic*, `code`, [links](url)
   NOTE: Process code and links FIRST to avoid conflicts with formatting backticks"
  (let* (;; Code: `text` - MUST be first to avoid matching formatting backticks
         (text (irregex-replace/all '(: #\` (submatch (+ (~ #\`))) #\`)
                                    text
                                    (lambda (m) (code (irregex-match-substring m 1)))))

         ;; Links: [text](url) - before bold/italic to avoid conflicts
         (text (irregex-replace/all '(: #\[ (submatch (+ (~ #\]))) #\] #\( (submatch (+ (~ #\)))) #\))
                                    text
                                    (lambda (m)
                                      (link (irregex-match-substring m 2)
                                            (irregex-match-substring m 1)))))

         ;; Bold: **text** or __text__
         (text (irregex-replace/all '(: (or "**" "__") (submatch (+ (~ #\*))) (or "**" "__"))
                                    text
                                    (lambda (m) (bold (irregex-match-substring m 1)))))

         ;; Italic: *text* or _text_ (but not part of bold)
         (text (irregex-replace/all '(: (or #\* #\_) (submatch (+ (~ #\* #\_))) (or #\* #\_))
                                    text
                                    (lambda (m) (italics (irregex-match-substring m 1))))))
    text))

(define (convert-list-item line)
  "Convert list items to indented text"
  (let ((match (irregex-search '(: bol (* space) (or #\- #\* #\+) (+ space) (submatch (* any))) line)))
    (if match
        (let ((text (irregex-match-substring match 1)))
          (conc "  • " text "\n"))
        line)))

(define (convert-numbered-list line)
  "Convert numbered list items"
  (let ((match (irregex-search '(: bol (* space) (+ digit) #\. (+ space) (submatch (* any))) line)))
    (if match
        (let ((text (irregex-match-substring match 1)))
          (conc "  " text "\n"))
        line)))

(define (convert-blockquote line)
  "Convert > quote to indented italic text"
  (let ((match (irregex-search '(: bol #\> (? space) (submatch (* any))) line)))
    (if match
        (let ((text (irregex-match-substring match 1)))
          (conc "  " (italics text) "\n"))
        line)))

;; ========== MAIN CONVERTER ==========

(define (convert-line line)
  "Convert a single line of markdown to micron"
  (cond
    ;; Empty line
    ((string-null? (string-trim line)) "  \n")

    ;; Horizontal rule (check before inline conversion)
    ((irregex-search '(: bol (or (>= 3 #\-) (>= 3 #\*) (>= 3 #\_)) (* space) eol) line)
     (convert-horizontal-rule line))

    ;; Heading
    ((string-prefix? "#" (string-trim-both line))
     (convert-heading line))

    ;; Blockquote
    ((string-prefix? ">" (string-trim-both line))
     (convert-blockquote line))

    ;; List item
    ((irregex-search '(: bol (* space) (or #\- #\* #\+) space) line)
     (convert-list-item (convert-inline-formatting line)))

    ;; Numbered list
    ((irregex-search '(: bol (* space) (+ digit) #\. space) line)
     (convert-numbered-list (convert-inline-formatting line)))

    ;; Regular paragraph
    (else
     (conc (convert-inline-formatting line) "  \n"))))

(define (markdown->micron markdown-text)
  "Convert markdown string to micron markup"
  (let* ((lines (string-split markdown-text "\n"))
         (converted-lines (map convert-line lines)))
    (apply conc converted-lines)))

(define (markdown-file->micron path)
  "Read markdown file and convert to micron"
  (let ((content (read-file-contents path)))
    (markdown->micron content)))

;; ========== CONVENIENCE ==========

(define md->micron markdown->micron)
(define md-file->micron markdown-file->micron)
