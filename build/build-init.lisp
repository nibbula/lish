;;;
;;; build-init.lisp - Initialization for built Lish.
;;;

(defpackage :build-init
  (:documentation "Initialization for built Lish.")
  (:use :cl)
  (:export #:*build-verbose*))
(in-package :build-init)

;; Do things that are too horrible to mention here.
(load "build/horrible.lisp" :verbose nil)

(msg "[Build initializing...]")

(defun fail (message &rest args)
  (format t "~&/~v,,,va~%~%BUILD FAILURE:~%~?~%~&\\~v,,,va~%"
	  40 #\- #\- message args 40 #\- #\-)
  (exit-lisp :code 1))

(defvar *build-verbose* (sf-getenv "LISH_BUILD_VERBOSE")
  "True to build with more compilation messages. Can be set with the
environment variable LISH_BUILD_VERBOSE.")

(load "build/load-asdf.lisp" :verbose *build-verbose*)

(defvar *home* (or (and (sf-getenv "LISP_HOME")
			(namestring (probe-file (sf-getenv "LISP_HOME"))))
		   (namestring (user-homedir-pathname)))
  "Namestring of the home directory.")

(load "build/load-quicklisp.lisp" :verbose nil)

(push (truename "../")          asdf:*central-registry*)
(push (truename "../lib/")      asdf:*central-registry*)
(push (truename "../opsys/")    asdf:*central-registry*)
(push (truename "../io/")       asdf:*central-registry*)
(push (truename "../iof/")      asdf:*central-registry*)
(push (truename "../terminal/") asdf:*central-registry*)
(push (truename "../inator/")   asdf:*central-registry*)
(push (truename "../syntax/")   asdf:*central-registry*)
(push (truename "../rl/")       asdf:*central-registry*)
(push (truename "../deblarg/")  asdf:*central-registry*)
(push (truename "../unicode/")  asdf:*central-registry*)
(push (truename "../tools/")    asdf:*central-registry*)
(push (truename "./")           asdf:*central-registry*)

;; Suppress the fucking ASDF warnings.
#+(or) ; we're in an alternate future
(let ((bitch (find-symbol "*SUPPRESS-DEFINITION-WARNINGS*" :asdf))
      (troublemakers
       '(:cl-ppcre-test :puri-tests :cl-base64-tests :flexi-streams-test
	 :openssl-1.1.0 :trivial-garbage-tests :cl-fad-test
	 :hunchentoot-test :hunchentoot-dev)))
  (if bitch
      (set bitch troublemakers) ;; yes, I mean set.
      (progn
	(setf bitch (find-symbol
		     "*KNOWN-SYSTEMS-WITH-BAD-SECONDARY-SYSTEM-NAMES*" :asdf))
	(if bitch
	    (set bitch (uiop:list-to-hash-set troublemakers))
	    (format t "~&I'm very sorry, but you're not running a recent ~
                        enough version of ASDF, so you will probably see ~
                        useless warnings.~%")))))

;; We really want to set debug up so that we can get function arguments.
;; This will porbably be turned back off in build-deinit.lisp.
(defparameter *saved-debug-quality* nil)
(let ((debug (cadr (assoc 'debug (uiop:get-optimization-settings)))))
  (when (< debug 2)
    (setf *saved-debug-quality* debug)
    (proclaim `(optimize (debug 2)))))

(in-package :cl-user)

;; EOF
