;;; upper-case.lisp

(in-package :cl-change-case-test)
(in-suite :cl-change-case)

(test upper-case
  (is (string= (upper-case nil) ""))
  (is (string= (upper-case "test") "TEST"))
  (is (string= (upper-case "TEST") "TEST"))
  (is (string= (upper-case "string") "STRING")))

(test upper-case-first
  (is (string= (upper-case-first nil) ""))
  (is (string= (upper-case-first "test") "Test"))
  (is (string= (upper-case-first "TEST") "TEST")))
