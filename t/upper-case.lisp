;;; upper-case.lisp

(in-package :cl-change-case-test)
(in-suite :cl-change-case)

(test upper-case
  (is (string= (upper-case nil) ""))
  (is (string= (upper-case "test") "TEST"))
  (is (string= (upper-case "TEST") "TEST"))
  (is (string= (upper-case "string") "STRING")))
