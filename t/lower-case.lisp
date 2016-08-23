;;; lower-case.lisp

(in-package :cl-change-case-test)
(in-suite :cl-change-case)

(test lower-case-strings
  (is (string= (lower-case nil) ""))
  (is (string= (lower-case "TEST") "test"))
  (is (string= (lower-case "test") "test")))
