;;; test-utils.lisp

(in-package :cl-change-case-test)

(defmacro is-string (actual expected)
  `(is (string= ,actual ,expected)))
