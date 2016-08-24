;;; camel-case.lisp

(in-package :cl-change-case-test)
(in-suite :cl-change-case)

(test camel-case
  ;; lowercase single words
  (is-string (camel-case "test") "test")
  (is-string (camel-case "TEST") "test")

  ;; camelCase regular no cased strings
  (is-string (camel-case "test string") "testString")
  (is-string (camel-case "Test String") "testString")

  ;; camel case non-alphanumeric chars
  (is-string (camel-case "dot.case") "dotCase")
  (is-string (camel-case "path/case") "pathCase")

  ;; underscore dots between numbers
  (is-string (camel-case "version 1.2.10") "version_1_2_10")
  (is-string (camel-case "version 1.21.0") "version_1_21_0")

  ;; camel case pascal cased stringd
  (is-string (camel-case "TestString") "testString")

  ;; smash numbers if wished
  (is-string (camel-case "test 1 2 3" :merge-numbers t) "test123"))
