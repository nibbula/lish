;;;								-*- Lisp -*-
;;; lish-test.asd -- System definition for lish-test
;;;

(defpackage :lish-test-system
    (:use :common-lisp :asdf))

(in-package :lish-test-system)

(defsystem lish-test
    :name               "lish-test"
    :description        "Tests for Lish."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPLv3"
    :source-control	:git
    :long-description   "Tests for Lish."
    :depends-on (:test :lish)
    :components
    ((:file "lish-test")))
