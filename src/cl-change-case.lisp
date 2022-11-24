;;; cl-change-case.lisp

(in-package :cl-user)
(defpackage #:cl-change-case
  (:nicknames :change-case)
  (:use :cl)
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
   #:pascal-case
   #:path-case
   #:sentence-case
   #:snake-case
   #:swap-case
   #:title-case
   #:constant-case))

(in-package :cl-change-case)

(defvar +empty-string+ "")


;;; lower case

(defun lower-case (string)
  "Downcase each character in STRING."
  (check-type string string)
  (string-downcase string))

(defun lower-case-first (string)
  "Downcase the first character in STRING."
  (check-type string string)
  (if (zerop (length string))
      +empty-string+
      (let ((copy (copy-seq string)))
        (setf (char copy 0)
              (char-downcase (char copy 0)))
        copy)))

(defun string-lower-case-p (string)
  "Tests if each character in STRING has lower case."
  (check-type string string)
  (every
   (lambda (char)
     (if (alpha-char-p char)
         (lower-case-p char)
         ;; non-alphanumeric chars considered lower case.
         t))
   string))


;;; upper case

(defun upper-case (string)
  "Upcase each character in STRING."
  (check-type string string)
  (string-upcase string))

(defun upper-case-first (string)
  "Upcase the first character of STRING."
  (check-type string string)
  (if (zerop (length string))
      +empty-string+
      (let ((copy (copy-seq string)))
        (setf (char copy 0)
              (char-upcase (char copy 0)))
        copy)))

(defun string-upper-case-p (string)
  "Test if each character in STRING has upper case."
  (check-type string string)
  (every
   (lambda (char)
     (if (alpha-char-p char)
         (upper-case-p char)
         ;; non-alphanumeric chars considered upper case.
         t))
   string))


;;; no case

(defun no-case (object &key (replacement " "))
  "Transform STRING to lower case space delimited.
Use REPLACEMENT as delimiter."
  (check-type object string)
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
    (reduce (lambda (transformed fn)
              (funcall fn transformed))
            (list #'replace-camel-case
                  #'replace-camel-case-upper
                  #'replace-non-word
                  #'trim-whitespace
                  #'lower-case)
            :initial-value object)))


;;; camel case

(defun camel-case (string &key merge-numbers)
  "Transform STRING to camelCase.
Dot-separated numbers like 1.2.3 will be replaced by underscores 1_2_3
unless MERGE-NUMBERS is non-nil."
  (check-type string string)
  (let ((nocase (if merge-numbers
                    (no-case string)
                    (regex-replace-all " (?=\\d)" (no-case string) "_"))))
    (values
     (regex-replace-all " (.)"
                        nocase
                        (lambda (target $1)
                          (declare (ignore target))
                          (upper-case $1))
                        :simple-calls t))))


;;; dot case

(defun dot-case (string)
  "Transform STRING to dot.case"
  (check-type string string)
  (no-case string :replacement "."))


;;; header case

(defun header-case (string)
  "Transform STRING to Header-Case"
  (check-type string string)
  (let ((no-case (no-case string :replacement "-")))
    (values
     (regex-replace-all "^.|\-."
                        no-case
                        (lambda (match &rest registers)
                          (declare (ignore registers))
                          (upper-case match))
                        :simple-calls t))))


;;; param case

(defun param-case (string)
  "Transform STRING to param-case"
  (check-type string string)
  (no-case string :replacement "-"))


;;; pascal case

(defun pascal-case (string)
  "Transform STRING to Pascal Case"
  (check-type string string)
  (upper-case-first (camel-case string)))


;;; path case

(defun path-case (string)
  "Transform STRING to path/case"
  (check-type string string)
  (no-case string :replacement "/"))


;;; sentence case

(defun sentence-case (string)
  "Transform STRING to Sentence case"
  (check-type string string)
  (upper-case-first (no-case string)))


;;; snake case 

(defun snake-case (string)
  "Transform STRING to snake_case"
  (check-type string string)
  (no-case string :replacement "_"))


;;; swap case

(defun swap-case (string)
  "Reverse case for each character in STRING."
  (check-type string string)
  (map 'string (lambda (char)
                 (if (upper-case-p char)
                     (char-downcase char)
                     (char-upcase char)))
       string))


;;; title case

(defun title-case (string)
  "Transform STRING to Title Case"
  (check-type string string)
  (let ((no-case (no-case string)))
    (values
     (regex-replace-all "^.| ."
                        no-case
                        (lambda (match &rest registers)
                          (declare (ignore registers))
                          (upper-case match))
                        :simple-calls t))))


;;; constant case

(defun constant-case (string)
  "Transform STRING to CONSTANT_CASE."
  (check-type string string)
  (upper-case (snake-case string)))
