;;;								-*- Lisp -*-
;;; lish.asd -- System definition for LISH package
;;;

(defsystem lish
    :name               "lish"
    :description        "Lispy system command shell."
    :version            "0.1.0"
    ;;:version		(:read-file-form "version.lisp")
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPLv3 or something."
    :source-control	:git
    :long-description   "Lish is a Lisp shell. Don't hold your breath."
    :entry-point	"lish:shell-toplevel"
    :depends-on (:dlib :opsys :dlib-misc :stretchy :char-util :glob :table-print
		 :reader-ext :dlib-interactive :completion :keymap :terminal
		 :terminal-ansi :rl
		 #+use-re :re
		 #-use-re :cl-ppcre
		 :chipz :fatchar :magic :theme :collections
		 :bordeaux-threads)
    :components
    ((:file "package")
     (:file "vars"	:depends-on ("package"))
     (:file "args"      :depends-on ("package" "vars"))
     (:file "reader"    :depends-on ("package" "vars"))
     (:file "commands"  :depends-on ("package" "vars" "args"))
     (:file "shell"     :depends-on ("package" "vars" "args" "commands"))
     (:file "complete"  :depends-on ("package" "vars" "args" "commands"))
     (:file "prompt"    :depends-on ("package" "vars" "args" "commands"
				     "shell"))
     (:file "lish"   	:depends-on ("package" "vars" "args" "commands"
				     "shell" "prompt" "complete"))
     (:file "builtin"   :depends-on ("package" "vars" "args" "commands"
				     "shell" "complete" "lish"))
     (:file "piping"	:depends-on ("package" "vars" "args" "commands"
				     "shell" "lish"))
     (:file "mine"      :depends-on ("package" "vars" "args" "commands"
				     "shell" "lish" "piping")))
    :in-order-to ((asdf:test-op (asdf:test-op :lish-test))))
