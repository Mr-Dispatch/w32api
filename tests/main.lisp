(defpackage W32API/tests/main
  (:use :cl
        :W32API
        :rove))
(in-package :W32API/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :W32API)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
