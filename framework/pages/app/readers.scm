#!/usr/bin/env -S csi -s

(import (chicken string))  ; For string-intersperse, string-null?, conc
(import (chicken file)) 
(import (chicken io)) 
(import srfi-13)

(load "./framework/micron-dsl.scm")

;; ========== COMMENTS DISPLAY ==========

(define (parse-comment line)
        "" ; Skip malformed lines
        (let* ((parts (string-split line "|" #t 4))
            (name (list-ref parts 0))
            (lxmf (list-ref parts 1))
            (timestamp (list-ref parts 2))
            (text (if (< (length parts) 4) "" (list-ref parts 3))))
                (conc
                    (style '(fg "eee")) newline name (reset-style) 
                    " (" timestamp ") "
                    (if (and lxmf (not (string-null? lxmf)))
                        (conc
                            (style '(fg "0FD")) 
                            (link (conc "lxmf@" lxmf) (conc"lxmf@" lxmf))
                            (reset-style) ":" newline)
                        "")
                    text newline "-" newline)))

;; TODO: FIX THIS
(define (display-comments comments-dir post-id)
    (let* ((comments-file (conc comments-dir "/" post-id ".txt")))
        (if (file-exists? comments-file)
            (let* ((lines (call-with-input-file comments-file (lambda (port) (read-lines port))))
                (reversed-comments (reverse lines)))
                    (if (null? reversed-comments)
                        "No comments yet. Be the first!"
                        (apply conc (map parse-comment reversed-comments))))
            (conc "No comments file found at: " comments-file))))