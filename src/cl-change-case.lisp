;;; cl-change-case.lisp

(in-package :cl-user)
(defpackage #:cl-change-case
  (:nicknames :change-case)
  (:use :cl)
  (:import-from :alexandria
                #:define-constant)
  (:import-from :cl-ppcre
                #:regex-replace-all)
  (:export
   #:lower-case
   #:lower-case-first
   #:string-lower-case-p
   #:upper-case
   #:upper-case-first
   #:string-upper-case-p
   #:no-case
   #:camel-case
   #:dot-case
   #:header-case
   #:param-case
   #:pascal-case))

(in-package :cl-change-case)

(define-constant +empty-string+ "" :test #'string=)


;;; lower case

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


;;; upper case

(defun upper-case (string)
  "Upcase each character in STRING."
  (if string
      (string-upcase string)
      +empty-string+))

(defun upper-case-first (string)
  "Upcase the first character of STRING."
  (if (zerop (length string))
      +empty-string+
      (let ((copy (copy-seq string)))
        (setf (char copy 0)
              (char-upcase (char copy 0)))
        copy)))

(defun string-upper-case-p (string)
  "Test if each character in STRING has upper case."
  (every #'identity (map 'list #'upper-case-p string)))


;;; no case

(defun no-case (object &key (replacement " "))
  "Transform STRING to lower case space delimited.
Use REPLACEMENT as delimiter."
  (flet ((replace-camel-case (string)
           (regex-replace-all "([\\p{Ll}\\p{N}])(\\p{Lu})" string "\\1 \\2"))
         (replace-camel-case-upper (string)
           (regex-replace-all "(\\p{Lu}+)(\\p{Lu}\\p{Ll}+)" string "\\1 \\2"))
         (replace-non-word (string)
           (regex-replace-all
            "[^\\p{L}\\p{N}]+"
            string
            (lambda (target start end match-start match-end reg-starts reg-ends)
              (declare (ignore target start reg-starts reg-ends))
              ;; completely remove trailing and leading non-word chars
              (if (or (zerop match-start)
                      (= match-start (- end (- match-end match-start))))
                  +empty-string+
                  ;; use replacement kwarg for non-space chars inbetween
                  replacement))))
         (trim-whitespace (string)
           (string-trim '(#\Space #\Tab #\Newline) string)))
    (if (null object)
        +empty-string+
        (reduce (lambda (transformed fn)
                  (funcall fn transformed))
                (list #'replace-camel-case
                      #'replace-camel-case-upper
                      #'replace-non-word
                      #'trim-whitespace
                      #'lower-case)
                :initial-value (if (typep object 'fixnum)
                                   (princ-to-string object)
                                   object)))))


;;; camel case

(defun camel-case (string &key merge-numbers)
  "Transform STRING to camelCase.
Dot-separated numbers like 1.2.3 will be replaced by underscores 1_2_3
unless MERGE-NUMBERS is non-nil."
  (let ((nocase (if merge-numbers
                    (no-case string)
                    (regex-replace-all " (?=\\d)" (no-case string) "_"))))
    (regex-replace-all " (.)"
                       nocase
                       (lambda (target $1)
                         (declare (ignore target))
                         (upper-case $1))
                       :simple-calls t)))


;;; dot case

(defun dot-case (string)
  "Transform STRING to dot.case"
  (no-case string :replacement "."))


;;; header case

(defun header-case (string)
  "Transform STRING to Header-Case"
  (let ((no-case (no-case string :replacement "-")))
    (regex-replace-all "^.|\-."
                       no-case
                       (lambda (match &rest registers)
                         (declare (ignore registers))
                         (upper-case match))
                       :simple-calls t)))


;;; param case

(defun param-case (string)
  "Transform STRING to param-case"
  (no-case string :replacement "-"))


;;; pascal case

(defun pascal-case (string)
  "Transform STRING to PascalCase"
  (upper-case-first (camel-case string)))
