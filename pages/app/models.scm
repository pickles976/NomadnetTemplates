#!/usr/bin/env -S csi -s

;;; models.scm - Hand-written model definitions for learning
;;;
;;; In Scheme, we'll represent a model as an association list (alist).
;;; An alist is a list of pairs, like: '((key1 . value1) (key2 . value2))
;;;
;;; For our ORM, each model will be an alist with:
;;;   - 'name: the table name (symbol)
;;;   - 'fields: a list of field definitions
;;;
;;; Each field is also an alist with:
;;;   - 'name: field name (symbol)
;;;   - 'type: SQL type like 'text, 'integer
;;;   - 'size: optional max size for text fields

;; Let's also define the comment model to match what you already have
(define comment-model
  '((name . comment)
    (fields . (
      ((name . id)
       (type . integer)
       (primary-key . #t)
       (autoincrement . #t))

      ((name . name)
       (type . text)
       (size . 16))

      ((name . address)
       (type . text)
       (size . 32))

      ((name . page-name)
       (type . text)
       (size . 32))

      ((name . timestamp)
       (type . text)
       (size . 16))

      ((name . text)
       (type . text))))))

;; List of all models - add new models here
(define all-models
  (list comment-model))