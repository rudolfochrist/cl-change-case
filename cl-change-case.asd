;;; cl-change-case.asd

(defsystem "cl-change-case"
  :version (:read-file-line "version")
  :author "Sebastian Christ"
  :mailto "rudolfo.christ@gmail.com"
  :license "MPL-2.0"
  :source-control (:git "git@github.com:rudolfochrist/cl-change-case.git")
  :bug-tracker "https://github.com/rudolfochrist/cl-change-case/issues"
  :depends-on ("cl-ppcre"
               "cl-ppcre-unicode")
  :components ((:module "src"
                :components ((:file "cl-change-case")))
               (:static-file "version"))
  :description "Convert strings between camelCase, param-case, PascalCase and more"
  :in-order-to ((test-op (load-op cl-change-case/test))))


(defsystem "cl-change-case/test"
  :author "Sebastian Christ"
  :mailto "rudolfo.christ@gmail.com"
  :description "Test system of cl-change-case"
  :license "MPL-2.0"
  :depends-on ("fiveam"
               "cl-change-case")
  :components ((:module "t"
                :components ((:file "cl-change-case"))))
  :perform (load-op :after (op c)
                    (uiop:symbol-call :5am :run! :cl-change-case)))
