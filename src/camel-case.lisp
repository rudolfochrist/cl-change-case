;;; camel-case.lisp

(in-package :cl-change-case)

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
