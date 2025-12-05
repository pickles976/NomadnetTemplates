#!/usr/bin/env -S csi -s

;;; orm.mu - ORM Guide

(import micron)

(define (code-block . inners)
    (conc
      (style '(bg "000" align "left" fg "0f0")) nl
      (apply conc inners)
      (reset-style) nl))
