(defpackage #:policy-protocol/tests
  (:use #:cl #:rove))

(in-package #:policy-protocol/tests)

(deftest package-exists
  (ok (find-package :policy-protocol)))
