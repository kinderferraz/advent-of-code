(defpackage twit-cleaner/tests/main
  (:use :cl
        :twit-cleaner
        :rove))
(in-package :twit-cleaner/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :twit-cleaner)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
