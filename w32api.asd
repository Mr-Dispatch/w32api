(defpackage w32api-system
  (:use :cl :asdf))

(in-package :w32api-system)

(defsystem "w32api"
  :version "0.0.1"
  :author "Mr.Dispatch"
  :license ""
  :depends-on ("st-json" "cffi" "rutils")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Win32 API bindings generated from JSON translation of .WinMD metadata, taken from the github marlersoft/win32json project"
  :in-order-to ((test-op (test-op "w32api/tests"))))

(defsystem "w32api/tests"
  :author "Mr.Dispatch"
  :license ""
  :depends-on ("W32API"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for w32api"
  :perform (test-op (op c) (symbol-call :rove :run c)))
