;;; cl-change-case.lisp

(in-package :cl-user)
(defpackage #:cl-change-case
  (:nicknames :change-case)
  (:use :cl)
  (:import-from :alexandria
                #:define-constant)
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
   #:camel-case))

