;;; lower-case.lisp

(in-package :cl-change-case)

(define-constant +empty-string+ "" :test #'string=)

(defun lower-case (string)
  "Downcase each character in STRING."
  (if string
      (string-downcase string)
      +empty-string+))


(defun lower-case-first (string)
  "Downcase the first character in STRING."
  (if (zerop (length string))
      +empty-string+
      (let ((copy (copy-seq string)))
        (setf (char copy 0)
              (char-downcase (char copy 0)))
        copy)))

(defun string-lower-case-p (string)
  "Tests if each character in STRING has lower case."
  (every #'identity (map 'list #'lower-case-p string)))

