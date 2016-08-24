;;; lower-case.lisp

(in-package :cl-change-case-test)
(in-suite :cl-change-case)

(test lower-case-strings
  (is (string= (lower-case nil) ""))
  (is (string= (lower-case "TEST") "test"))
  (is (string= (lower-case "test") "test")))

(test lower-case-first
  (is (string= (lower-case-first nil) ""))
  (is (string= (lower-case-first "Test") "test"))
  (is (string= (lower-case-first "TEST") "tEST")))

(test string-lower-case-p
  (is (not (null (string-lower-case-p "test"))))
  (is (null (string-lower-case-p "Test")))
  (is (null (string-lower-case-p "TEST"))))
