;;;								-*- Lisp -*-
;;; lish.asd -- System definition for LISH package
;;;

;;; $Revision: 1.6 $

(defpackage :lish-system
    (:use :common-lisp :asdf))

(in-package :lish-system)

(defsystem lish
    :name               "lish"
    :description        "Lispy system command shell."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPL3 or something."
    :long-description   "I don't recommend using this yet."
    :entry-point	"lish:shell-toplevel"
    :depends-on (:tiny-rl :cl-ppcre :opsys :dlib :dlib-misc :stretchy :glob :chipz)
    :components
    ((:file "package")
     (:file "vars"	:depends-on ("package"))
     (:file "commands"  :depends-on ("package" "vars"))
     (:file "complete"  :depends-on ("package" "vars" "commands"))
     (:file "lish"   	:depends-on ("package" "vars" "commands" "complete"))
     (:file "builtin"   :depends-on ("package" "vars" "commands" "complete"
				     "lish"))
     (:file "piping"	:depends-on ("package" "vars" "commands" "lish"))
     (:file "mine"      :depends-on ("package" "vars" "commands" "lish"
				     "piping"))))
