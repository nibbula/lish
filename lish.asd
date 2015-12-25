;;;								-*- Lisp -*-
;;; lish.asd -- System definition for LISH package
;;;

(defpackage :lish-system
    (:use :common-lisp :asdf))

(in-package :lish-system)

(defsystem lish
    :name               "lish"
    :description        "Lispy system command shell."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3 or something."
    :long-description   "I don't recommend using this yet."
    :entry-point	"lish:shell-toplevel"
    :depends-on (:dlib :opsys :dlib-misc :stretchy :glob :tiny-rl :cl-ppcre
		 :chipz)
    :components
    ((:file "package")
     (:file "vars"	:depends-on ("package"))
     (:file "args"      :depends-on ("package" "vars"))
     (:file "commands"  :depends-on ("package" "vars" "args"))
     (:file "shell"     :depends-on ("package" "vars" "args" "commands"))
     (:file "complete"  :depends-on ("package" "vars" "args" "commands"))
     (:file "stats"     :depends-on ("package" "vars" "args" "commands"
				     "shell"))
     (:file "lish"   	:depends-on ("package" "vars" "args" "commands"
				     "shell" "complete" "stats"))
     (:file "builtin"   :depends-on ("package" "vars" "args" "commands"
				     "shell" "complete" "stats" "lish"))
     (:file "piping"	:depends-on ("package" "vars" "args" "commands"
				     "shell" "lish"))
     (:file "mine"      :depends-on ("package" "vars" "args" "commands"
				     "shell" "lish" "piping")))
    :in-order-to ((asdf:test-op (asdf:test-op :lish-test))))
