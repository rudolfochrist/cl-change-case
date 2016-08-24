;;; upper-case.lisp

(in-package :cl-change-case)

(defun upper-case (string)
  "Upcase each character in STRING."
  (if string
      (string-upcase string)
      +empty-string+))

