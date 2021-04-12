;;;
;;; package.lisp - Package definition for Lish
;;;

(defpackage :lish
  (:documentation
   "Unix Shell & Lisp somehow smushed together.

Lish is a program designed to make typing both operating system commands, and
Common Lisp expressions, convienient. It combines the features of a
traditional operating system shell with a Lisp REPL. It's designed to
hopefully have little annoyance to people familair with a POSIX shell. But it
does not have exact compatibility with POSIX shells.

The motivation for writing Lish came from the annoyance of having to swtich
between a Lisp REPL and a Unix shell. Lish may be used as a command shell,
without any particular knowledge of it's Lisp programming features.
")
  (:use :cl :dlib :opsys :dlib-misc :stretchy :char-util :glob
	:table :table-print :reader-ext :completion :keymap
	:terminal :terminal-ansi :rl :fatchar :fatchar-io :collections
	:ostring :ochar :grout :dtime
	#+use-regex :regex #-use-regex :cl-ppcre)
  ;; #+sbcl (:import-from :sb-ext #:retry)
  (:export
   ;; Main entry point(s)
   #:lish
   #:shell-toplevel
   ;; variables
   #:*lish-level*
   #:*lish-user-package*
   #:*shell-name*
   #:*shell*
   #:*old-pwd*
   #:*dir-list*
   #:*shell-path*
   #:*accepts*
   #:*output* #:output
   #:*input* #:input
   #:*lishrc*
   #:*version*
   #:*major-version* #:*minor-version* #:*build-version*
   ;; hooks @@@ maybe should be made into options?
   #:*pre-command-hook*
   #:*post-command-hook*
   #:*unknown-command-hook*
   #:*enter-shell-hook*
   #:*exit-shell-hook*
   ;; installation
   #:make-standalone
   ;; shell options
   #:option
   #:lish-prompt                   #:set-lish-prompt
   #:lish-prompt-function          #:set-lish-prompt-function
   #:lish-right-prompt             #:set-lish-right-prompt
   #:lish-sub-prompt               #:set-lish-sub-prompt
   #:lish-ignore-eof               #:set-lish-ignore-eof
   #:lish-debug                    #:set-lish-debug
   #:lish-collect-stats            #:set-lish-collect-stats
   #:lish-autoload-from-asdf       #:set-lish-autoload-from-asdf
   #:lish-autoload-quietly         #:set-lish-autoload-quietly
   #:lish-history-expansion        #:set-lish-history-expansion
   #:lish-expand-braces            #:set-lish-expand-braces
   #:lish-colorize                 #:set-lish-colorize
   #:lish-auto-cd                  #:set-lish-auto-cd
   #:lish-history-style            #:set-lish-history-style
   #:lish-history-format           #:set-lish-history-format
   #:lish-history-save-values      #:set-lish-history-save-values
   #:lish-auto-suggest             #:set-lish-auto-suggest
   #:lish-partial-line-indicator   #:set-lish-partial-line-indicator
   #:lish-export-pipe-results      #:set-lish-export-pipe-results
   #:make-prompt
   ;; shell object
   #:shell
   #:lish-aliases
   #:lish-editor
   #:lish-keymap
   #:lish-old-pwd
   #:lish-dir-list
   #:lish-suspended-jobs
   #:lish-last-background-job
   #:lish-start-time
   #:lish-options
   ;; arguments
   #:argument
   #:arg-name #:arg-type #:arg-value #:arg-default #:arg-repeating
   #:arg-optional #:arg-hidden #:arg-prompt #:arg-help #:arg-short-arg
   #:arg-long-arg
   ;; argument types
   #:arg-boolean #:arg-number #:arg-integer #:arg-float #:arg-character
   #:arg-string #:arg-symbol #:arg-keyword #:arg-object
   #:arg-case-preserving-object #:arg-sequence #:arg-list #:arg-function
   #:arg-package #:arg-date #:arg-pathname #:arg-directory #:arg-choice
   #:arg-choices #:arg-choice-labels #:arg-choice-test
   #:arg-choice-compare-ignore-case #:arg-choice-compare #:arg-lenient-choice
   #:arg-option #:arg-input-stream-or-filename
   ;; argument types for builtins
   #:arg-job-descriptor #:arg-help-subject #:arg-boolean-toggle #:arg-signal
   #:arg-pid-or-job #:arg-function #:arg-key-sequence #:arg-command
   #:arg-system-designator #:arg-quicklisp-system-designator
   #:arg-autoload-place
   ;; argument generics
   #:convert-arg #:argument-choices
   #:defargtype
   ;; commands
   #:command #:command-name #:command-function #:command-arglist
   #:command-built-in-p #:command-loaded-from #:command-accepts
   #:internal-command #:shell-command #:builtin-command #:external-command
   #:command-list
   #:defcommand #:defexternal
   #:!cd #:!pwd #:!pushd #:!popd #:!dirs #:!suspend #:!fg #:!bg #:!jobs
   #:!history #:!echo #:!help #:!alias #:!unalias #:!exit #:!quit #:!source
   #:!debug #:!export #:!env #:!kill #:!time #:!times #:!umask #:!ulimit
   #:!wait #:!exec #:!bind #:!undefcommand #:!hash #:!type #:!opt #:!l #:!load
   #:!ldirs #:!ql #:!autoload #:!doc #:!var
   #:print-command-help
   ;; convenience / scripting
   #:alias #:set-alias #:unset-alias #:get-alias
   #:command-paths
   #:pipe
   #:in-bg
   #:in-pipe-p
   #:out-pipe-p
   #:append-file #:append-files
   #:run-with-output-to
   #:run-with-input-from
   #:input-line-words
   #:input-line-list
   #:map-output-lines
   #:command-output-words
   #:command-output-list
   #:with-shell
   ;; magic punctuation
   #:!  #:!?  #:!!  #:!$  #:!$$  #:!@  #:!_  #:!-
   #:!= #:!?= #:!!= #:!$= #:!$$= #:!@= #:!_= #:!-=
   #:!and #:!or #:!bg
   #:!> #:!>> #:!>! #:!>>!
   #:!< #:!!<
   #:!q #:!h #:!hh #:!v
   ;; internal-ish things that might want to be used
   #:get-command
   #:command-to-lisp-args
   #:posix-to-lisp-args
   #:enable-sharp-dollar-reader
   #:disable-sharp-dollar-reader
   #:file-enable-sharp-dollar-reader
   #:shell-read
   #:shell-eval
   #:shell-expand
   #:shell-expand-to-list
   #:format-prompt
   #:symbolic-prompt-to-string
   #:load-file
   #:suspend-job
   #:find-job
   #:job #:job-id #:job-name #:job-command-line #:job-status
   #:job-pid
   #:continue-job-in-foreground
   #:continue-job-in-background
   #:kill-job
   #:list-all-jobs
   #:accepts
   #:get-accepts
   #:twiddlify
   ))

;; EOF
