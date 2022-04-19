;;;								-*- Lisp -*-
;;; lish.asd -- System definition for LISH package
;;;

(defsystem lish
    :name               "lish"
    :description        "Lispy system command shell."
    ;;:version            "0.1.0"
    :version		(:read-file-form "version.lisp")
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :licence            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Lish is a Lisp shell. Don't hold your breath."
    :entry-point	"lish:shell-toplevel"
    :depends-on
    (;; Things in yew
     :dlib :opsys :dlib-misc :stretchy :char-util :glob
     :table :table-print :reader-ext :dlib-interactive :completion
     :keymap :terminal :terminal-ansi :rl
     :fatchar :fatchar-io :magic :theme :style :collections
     :ostring :ochar :grout :utf8b-stream :dtime :locale
     ;; Things in not in yew
      #+use-re :re
      #-use-re :cl-ppcre
     :chipz :bordeaux-threads #|:agnostic-lizard|#)
    :components
    ((:file "package")
     (:file "vars"	:depends-on ("package"))
     (:file "types"	:depends-on ("package"))
     (:file "args"      :depends-on ("package" "types" "vars"))
     (:file "reader"    :depends-on ("package" "vars"))
     (:file "jobs"      :depends-on ("package" "vars"))
     (:file "commands"  :depends-on ("package" "vars" "types" "args"))
     (:file "shell"     :depends-on ("package" "vars" "types" "args" "commands"))
     (:file "complete"  :depends-on ("package" "vars" "args" "commands"))
     (:file "prompt"    :depends-on ("package" "vars" "args" "commands"
				     "shell"))
     (:file "lish"   	:depends-on ("package" "vars" "types" "args" "jobs"
				     "commands" "shell" "prompt" "complete"))
     (:file "piping"	:depends-on ("package" "vars" "args" "commands"
				     "shell" "lish"))
     (:file "builtin"   :depends-on ("package" "vars" "args" "jobs" "commands"
				     "shell" "complete" "lish"))
     (:file "mine"      :depends-on ("package" "vars" "args" "commands"
				     "shell" "lish" "piping")))
    :in-order-to ((asdf:test-op (asdf:test-op :lish-test))))
