;;
;; package.lisp - Package definition for Lish
;;

(defpackage :lish
  (:documentation
   "Unix Shell & Lisp somehow smushed together.

Lish is a program designed to make typing both operating system commands, and
Common Lisp expressions, convienient. It combines the features of a
traditional operating system shell with a Lisp REPL. It's designed to
hopefully have little annoyance to people familair with a POSIX shell. But it
does not have exact compatibility with POSIX shells.

The motivation for writing Lish came from the annoyance of having to swtich
between a Lisp REPL and a Unix shell. But you might just want to use Lish for
it's shell features.
")
  (:use :cl :dlib :opsys :dlib-misc :stretchy :char-util :glob :table-print
	:completion :keymap :terminal :terminal-ansi :rl
	#+use-regex :regex #-use-regex :cl-ppcre
	:fatchar :collections)
  (:export
   ;; Main entry point(s)
   #:lish
   #:shell-toplevel
   ;; variables
   #:*lish-level*
   #:*lish-user-package*
   #:*shell*
   #:*old-pwd*
   #:*dir-list*
   #:*shell-path*
   #:*accepts*
   #:*output*
   #:*input*
   #:*lishrc*
   ;; hooks
   #:*pre-command-hook*
   #:*post-command-hook*
   #:*exit-shell-hook*
   ;; (installation)
   #:make-standalone
   ;; shell options
   #:lish-prompt
   #:lish-prompt-function
   #:lish-sub-prompt
   #:lish-ignore-eof
   #:lish-debug
   #:make-prompt
   ;; shell object
   #:shell
   #:lish-aliases
   #:lish-editor
   #:lish-keymap
   #:lish-old-pwd
   #:lish-dir-list
   #:lish-suspended-jobs
   #:lish-start-time
   #:lish-options
   ;; arguments
   #:argument
   #:arg-name #:arg-type #:arg-value #:arg-default #:arg-repeating
   #:arg-optional #:arg-hidden #:arg-prompt #:arg-help #:arg-short-arg
   #:arg-long-arg
   ;; argument types
   #:arg-boolean #:arg-number #:arg-integer #:arg-float #:arg-string
   #:arg-keyword #:arg-object #:arg-date #:arg-pathname
   #:arg-choice #:arg-choices #:arg-choice-labels
   #:arg-choice-test
   #:arg-choice-compare-ignore-case #:arg-choice-compare
   #:arg-lenient-choice 
   ;; argument generics
   #:convert-arg #:argument-choices
   #:defargtype
   ;; commands
   #:command #:command-name #:command-function #:command-arglist
   #:command-built-in-p #:command-loaded-from #:command-accepts
   #:internal-command #:shell-command #:builtin-command #:external-command
   #:command-list
   #:defcommand #:defexternal
   #:!cd #:!pwd #:!pushd #:!popd #:!dirs #:!suspend #:!history #:!echo
   #:!help #:!alias #:!unalias #:!type #:!exit #:!source #:!debug #:!bind
   #:!times #:!time #:!ulimit #:!wait #:!export #:!format
   #:!read #:!kill #:!umask #:!jobs #:!exec #:|!:| #:!hash #:!opt
   ;; convenience / scripting
   #:set-alias #:unset-alias #:get-alias
   #:command-paths
   #:pipe
   #:append-file #:append-files
   #:run-with-output-to
   #:run-with-input-from
   #:input-line-words
   #:input-line-list
   #:map-output-lines
   #:command-output-words
   #:command-output-list
   ;; magic punctuation
   #:!  #:!?  #:!!  #:!$  #:!$$  #:!@  #:!_  #:!-
   #:!= #:!?= #:!!= #:!$= #:!$$= #:!@= #:!_= #:!-=
   #:!and #:!or #:!bg
   #:!> #:!>> #:!>! #:!>>!
   #:!< #:!!<
   ;; internal-ish things that might want to be used
   #:get-command
   #:command-to-lisp-args
   #:posix-to-lisp-args
   #:shell-read
   #:shell-eval
   #:format-prompt
   #:symbolic-prompt-to-string
   #:load-file
   #:suspend-job
   #:accepts
   #:get-accepts
   ))

;; EOF
