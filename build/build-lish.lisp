;;
;; build-lish.lisp - Build Lish
;;

;; A fake Makefile.

(defpackage :build-lish
  (:documentation "Build Lish.")
  (:use :cl)
  (:export
   #:build-lish
   ))
(in-package :build-lish)

;; Do things that are too horrible to mention here.
(load "build/horrible.lisp" :verbose nil)

(msg "[Start builder on ~a ~a]" (lisp-implementation-type)
     (lisp-implementation-version))

(defun fail (message &rest args)
  (format t "~&/~v,,,va~%~~%BUILD FAILURE:~%~?~%~&\\~v,,,va~%"
	  40 #\- #\- message args 40 #\- #\-)
  (exit-lisp :code 1))

(msg "[Load ASDF]")
(load "build/load-asdf.lisp" :verbose nil)

(defvar *home* (or (and (sf-getenv "LISP_HOME")
			(namestring (probe-file (sf-getenv "LISP_HOME"))))
		   (namestring (user-homedir-pathname)))
  "Namestring of the home directory.")

(defvar *exit-code* 1 "The code we should exit with.")

;; also horrible, but not in horrible becase we need uiop
(defun run-with-input-from (stream command)
  "Run commands with input from a file or stream. COMMANDS can be a SHELL-EXPR,
or a list to be converted by LISP-ARGS-TO-COMMAND."
  (multiple-value-bind (stupid stupider exit-code)
      (uiop:run-program command :input stream :output t :error-output t
			:ignore-error-status t)
    (declare (ignore stupid stupider))
    (format t "~%[Build ~a]~%"
	    (if (zerop exit-code)
		"Succeeded"
		"Failed"))
    (setf *exit-code* exit-code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build Lishes of various kinds.

(defparameter *default-target* 'lish)
(defparameter *targets* '("lish"))
(defparameter *install-dir* (merge-pathnames "bin" *home*))
(defparameter *lisp* (or (sf-getenv "LISP") "sbcl"))
(defun impl (i)
  (search i *lisp* :test #'equalp))

(defparameter *lisp-flags*
  (cond
    ((impl "sbcl") `("--noinform" "--noprint" "--disable-debugger"))
    (t '()))
  "Command line arguments to pass to the lisp.")

(defparameter *lisp-plain-flags*
  (cond
    ((impl "sbcl") "--no-userinit")
    ((impl "ccl")  "--no-init")
    ((impl "clisp") "-norc")
    (t ""))
  "Command line arguments to pass to the lisp for.")

(defgeneric build (target)
  (:documentation "Build a thing."))

(defmethod build (target)
  (format *error-output* "I don't know how to build ~a" target))

(defmethod build ((target (eql 'lish)))
  "Normal lish."
  (msg "[Build target ~s]" target)
  (with-input-from-string
      (stream
       (format nil
	       "(load \"build/build-init.lisp\" :verbose nil) ~
		(asdf:load-system :tiny-repl :verbose nil) ~
		(asdf:load-system :deblarg :verbose nil) ~
		(asdf:load-system :lish :verbose nil) ~
		(lish:make-standalone)"))
    (run-with-input-from stream `(,*lisp* ,@*lisp-flags* "--" "-norl"))))

(defmethod build ((target (eql 'lishfu)))
  (msg "[Build target ~s]" target)
  (with-input-from-string
      (stream
       (format nil
	       "(load \"build/build-init.lisp\" :verbose nil) ~
		(asdf:load-system :tiny-repl :verbose nil) ~
		(asdf:load-system :deblarg :verbose nil) ~
		(asdf:load-system :lish :verbose nil) ~
		(load \"build/fully-loaded.lisp\" :verbose nil) ~
		(lish:make-standalone)"))
    (run-with-input-from stream `(,*lisp* ,@*lisp-flags* "--" "-norl"))))

;; Plain, aka without my (or your) startup

(defmethod build ((target (eql 'lishp)))
  (msg "[Build target ~s]" target)
  (with-input-from-string
      (stream
       (format nil
	       "(load \"build/build-init.lisp\") ~
                (push \"../\" asdf:*central-registry*) ~
                (push \"../opsys/\" asdf:*central-registry*) ~
                (push \"../rl/\" asdf:*central-registry*) ~
                (push \"./\" asdf:*central-registry*) ~
                (asdf:load-system :lish :verbose nil) ~
                (setf asdf:*central-registry* ~
                 (delete \"./\" asdf:*central-registry* :test #'equal)) ~
                (setf asdf:*central-registry* ~
                 (delete \"../\" asdf:*central-registry* :test #'equal)) ~
                (lish:make-standalone)"))
    (run-with-input-from stream `(,*lisp* ,@*lisp-plain-flags* "--" "-norl"))))

#|
(defun install-one (file)
  (when (not (directory-p *install-dir*))
    (!mkdir *install-dir*))
  (copy-file file *install-dir*))

(defun install ()
  (build *targets*)
  (loop :for target :in *targets* :do
     (install-one target)))

(defun clean-one (file)
  (delete-file file))

(defun clean ()
  (loop :for target :in *targets* :do
     (clean-one target)))
|#

;; Main or something.

(if (> (length (lisp-args)) 1)
    (build (intern (string-upcase (second (lisp-args)))))
    (build *default-target*))

(exit-lisp :code *exit-code*)

;; EOF
