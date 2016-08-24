;;; no-case.lisp

(in-package :cl-change-case)

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

