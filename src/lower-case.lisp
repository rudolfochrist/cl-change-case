;;; lower-case.lisp

(in-package :cl-change-case)

(define-constant +empty-string+ "" :test #'string=)

(defun lower-case (string)
  (if string
      (string-downcase string)
      +empty-string+))

