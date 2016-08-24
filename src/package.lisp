;;; cl-change-case.lisp

(in-package :cl-user)
(defpackage #:cl-change-case
  (:nicknames :change-case)
  (:use :cl)
  (:import-from :alexandria
                #:define-constant)
  (:export
   #:lower-case
   #:lower-case-first
   #:string-lower-case-p
   #:upper-case
   #:upper-case-first))

