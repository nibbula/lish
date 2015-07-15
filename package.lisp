;;
;; package.lisp - Package definition for Lish
;;

;; $Revision$

(defpackage :lish
  (:documentation "Unix Shell & Lisp somehow smushed together.")
  (:use :cl :dlib :dlib-misc :opsys :stretchy :glob :completion :tiny-rl
	:cl-ppcre :ansiterm)
  (:export
   ;; Main entry point(s)
   #:lish
   #:shell-toplevel
   ;; variables
   #:*lish-level*
   #:*shell*
   #:*old-pwd*
   #:*dir-list*
   #:*shell-path*
   ;; (installation)
   #:make-standalone
   ;; shell options
   #:lish-prompt-char
   #:lish-prompt-string
   #:lish-prompt-function
   #:lish-sub-prompt
   #:lish-ignore-eof
   #:lish-debug
   #:make-prompt
   ;; shell object
   #:shell
   #:lish-aliases
   #:lish-editor
   #:lish-old-pwd
   #:lish-dir-list
   #:lish-suspended-jobs
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
   #:arg-lenient-choice
   ;; argument generics
   #:convert-arg
   ;; commands
   #:command #:command-name #:command-function #:command-arglist
   #:command-built-in-p #:command-loaded-from
   #:defcommand
   #:!cd #:!pwd #:!pushd #:!popd #:!dirs #:!suspend #:!history #:!echo
   #:!help #:!alias #:!unalias #:!type #:!exit #:!source #:!debug #:!bind
   #:!times #:!time #:!ulimit #:!wait #:!export #:!format
   #:!read #:!kill #:!umask #:!jobs #:!exec #:|!:| #:!hash
   ;; convenience / scripting
   #:set-alias #:unset-alias #:get-alias
   #:command-pathname
   #:command-paths
   #:input-line-words
   #:command-output-words
   #:command-output-list
   ;; magic punctuation
   #:! #:!? #:!! #:!$ #:!$$ #:!_ #:!-
   #:!and #:!or #:!bg
   #:!> #:!>> #:!>! #:!>>!
   #:!< #:!!<
   ;; internal-ish
   #:get-command
   #:command-to-lisp-args
   #:posix-to-lisp-args
   ))

;; EOF
