;;;								-*- Lisp -*-
;;; lish-test.asd -- System definition for lish-test
;;;

(defsystem lish-test
    :name               "lish-test"
    :description        "Tests for Lish."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "Haphazard and surely incomplete tests for the Lish shell."
    :depends-on (:test :lish :dlib)
    :components
    ((:file "lish-test"))
    :in-order-to ((test-op load-op :list-test))
    :perform (asdf:test-op :after (op c)
	       (funcall (intern (symbol-name '#:run) :lish-test))))
