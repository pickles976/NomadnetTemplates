#!/usr/bin/env -S csi -s

(import (chicken io))
(import (chicken string))


(define squiggle "-∿")
(define newline "\n")

(define (italics . inners) 
  (conc "`*" (apply conc inners) "`*"))

(define (bold . inners) 
  (conc "`!" (apply conc inners) "`!"))

(define (center . inners) 
  (conc "`c" (apply conc inners) "`a"))

(define (link src label)
  (conc "`[" label "`" src "]"))

(define (h . inners) (conc ">" (apply conc inners)))

(print
    squiggle newline
    (center (bold "Hello! ") "This is output from " (italics "micron")) newline
    squiggle newline
    ">Main" newline
    (link "lxmf@exampleaddress" "Link Example") newline
    ">>Comments" newline
    ">>Leave a Comment" newline)