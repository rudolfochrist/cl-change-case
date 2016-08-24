;;; upper-case.lisp

(in-package :cl-change-case)

(defun upper-case (string)
  "Upcase each character in STRING."
  (if string
      (string-upcase string)
      +empty-string+))

(defun upper-case-first (string)
  "Upcase the first character of STRING."
  (if (null string)
      +empty-string+
      (let ((copy (copy-seq string)))
        (setf (char copy 0)
              (char-upcase (char copy 0)))
        copy)))
