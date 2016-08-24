;;; lower-case.lisp

(in-package :cl-change-case)

(define-constant +empty-string+ "" :test #'string=)

(defun lower-case (string)
  (if string
      (string-downcase string)
      +empty-string+))


(defun lower-case-first (string)
  (if (zerop (length string))
      +empty-string+
      (let ((copy (copy-seq string)))
        (setf (char copy 0)
              (char-downcase (char copy 0)))
        copy)))

(defun string-lower-case-p (string)
  (every #'identity (map 'list #'lower-case-p string)))

