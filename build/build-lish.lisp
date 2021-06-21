;;;
;;; build-lish.lisp - Build Lish
;;;

;; A fake Makefile.

(defpackage :build-lish
  (:documentation "Build Lish.")
  (:use :cl)
  (:export
   #:build-lish
   ))
(defparameter build-lish::*default-target*
  (or (and (boundp '*target*) (symbol-value '*target*)) 'build-lish::lishfu))
(in-package :build-lish)

;; Do things that are too horrible to mention here.
(defvar *build-verbose* t)
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
  "Run commands with input from a file or stream. COMMAND should be a list of
strings."
  (multiple-value-bind (stupid stupider exit-code)
      (uiop:run-program command :input stream :output t :error-output t
			:ignore-error-status t)
    (declare (ignore stupid stupider))
    (format t "~%[Build ~a]~%"
	    (if (zerop exit-code)
		"Succeeded"
		"Failed"))
    (setf *exit-code* exit-code)))

(defun split (c string)
  "Simple low-budget split-sequence."
  (let ((start 0) end result)
    (loop :while (and (< start (length string))
		      (setf end (position c string :start start)))
       :do
       (push (subseq string start end) result)
       (setf start (1+ end)))
    (push (subseq string start) result)
    (nreverse result)))

;; Very low-budget, unfancy, and brittle.
(defun increment-build-version (file)
  "Increment the last component of the dotted version string in FILE."
  (format t "increment-build-version ~s~%" file)
  (let* ((vers (uiop:read-file-form file))
	 (vers-nums (mapcar (lambda (x)
			      (parse-integer x :junk-allowed t))
			    (split #\. vers)))
	 (new-vers (format nil "~a.~a.~a" (first vers-nums)
			   (second vers-nums)
			   (1+ (third vers-nums)))))
    (when (not (every #'integerp vers-nums))
      (cerror "Ok, whatever. Don't increment the version then."
	      "The version number incrementing seems to have failed.")
      (return-from increment-build-version nil))
    (with-open-file (stream file :direction :output :if-exists :overwrite)
      (write new-vers :stream stream)
      (terpri stream))))

(defun maybe-increment-build-version (file)
  "Only increment the version for maintainers."
  (when (sf-getenv "LISH_MAINTAINER")
    (increment-build-version file)))

;; So, ideally we would increment the build version number only if the build
;; succeeds, but that seems like a pain, since we would have to pass the new
;; version nubmer to both ASDF and the code being built, without setting it
;; in the version.lisp file. Perhaps in the future we can work that out.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build Lishes of various kinds.

;; (defparameter *default-target* 'lishfu)
(defparameter *targets* '('lish))
(defparameter *version-file* "version.lisp")
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
    ((impl "sbcl") `("--no-userinit"))
    ((impl "ccl")  `("--no-init"))
    ((impl "clisp") `("-norc"))
    (t ""))
  "Command line arguments to pass to the lisp for.")

(defgeneric build (target)
  (:documentation "Build a thing."))

(defmethod build (target)
  (format *error-output* "I don't know how to build ~a" target))

(defgeneric install (target)
  (:documentation "Install a thing."))

(defmethod install (target)
  (format *error-output* "I don't know how to install ~a" target))

(defun install-file (file &key (dir *install-dir*))
  (declare (ignore file dir))
  ;; (when (not (directory-p dir))
  ;;   (ensure-directories-exist dir))
  ;; (copy-file file dir)
  )

(defmethod build ((target (eql 'lish)))
  "Normal lish."
  (msg "[Build target ~s]" target)
  (with-input-from-string
      (stream
       (format nil
	       "(load \"build/build-init.lisp\" :verbose nil) ~
		(ql:quickload :dlib :verbose nil) ~
		(ql:quickload :tiny-repl :verbose nil) ~
		(ql:quickload :deblarg :verbose nil) ~
		(ql:quickload :lish :verbose nil) ~
		(lish:make-standalone :smaller t)"))
    (maybe-increment-build-version *version-file*)
    (run-with-input-from stream `(,*lisp* ,@*lisp-flags* "--" "-norl"))))

(defmethod build ((target (eql 'lishfu)))
  (msg "[Build target ~s]" target)
  (with-input-from-string
      (stream
       (format nil
	       "(load \"build/build-init.lisp\" :verbose nil) ~
		(ql:quickload :dlib :verbose nil) ~
		(ql:quickload :tiny-repl :verbose nil) ~
		(ql:quickload :deblarg :verbose nil) ~
		(ql:quickload :lish :verbose nil) ~
		(load \"build/fully-loaded.lisp\" :verbose nil) ~
		(lish:make-standalone :smaller t)"))
    (maybe-increment-build-version *version-file*)
    (run-with-input-from stream `(,*lisp* ,@*lisp-flags* "--" "-norl"))))

(defmethod build ((target (eql 'run)))
  (msg "[Build target ~s]" target)
  (load "build/build-init.lisp" :verbose nil)
  (let ((ql (intern "QUICKLOAD" (find-package :ql))))
    (funcall ql :dlib :verbose nil)
    (funcall ql :tiny-repl :verbose nil)
    (funcall ql :deblarg :verbose nil)
    (funcall ql :lish :verbose nil)
    (load "build/fully-loaded.lisp" :verbose nil)
    (funcall (intern "LISH" (find-package :lish)) :debug t)
    (setf *exit-code* 0)))

;; Plain, aka without my (or your) startup

(defmethod build ((target (eql 'lishp)))
  (msg "[Build target ~s]" target)
  (with-input-from-string
      (stream
       (format nil
	       "(load \"build/build-init.lisp\") ~
		(push (truename \"../\") asdf:*central-registry*) ~
		(push (truename \"../opsys/\") asdf:*central-registry*) ~
		(push (truename \"../rl/\") asdf:*central-registry*) ~
		(push (truename \"./\") asdf:*central-registry*) ~
		(ql:quickload :dlib :verbose nil) ~
		(ql:quickload :lish :verbose nil) ~
		(setf asdf:*central-registry* ~
		 (delete \"./\" asdf:*central-registry* :test #'equal)) ~
		(setf asdf:*central-registry* ~
		 (delete \"../\" asdf:*central-registry* :test #'equal)) ~
		(lish:make-standalone :smaller t)"))
    (maybe-increment-build-version *version-file*)
    (run-with-input-from stream `(,*lisp* ,@*lisp-flags*
					  ,@*lisp-plain-flags* "--" "-norl"))))

(defmethod build ((target (eql 'lishpfu)))
  (msg "[Build target ~s]" target)
  (with-input-from-string
      (stream
       (format nil
	       "(load \"build/build-init.lisp\") ~
		(push (truename \"../\") asdf:*central-registry*) ~
		(push (truename \"../opsys/\") asdf:*central-registry*) ~
		(push (truename \"../rl/\") asdf:*central-registry*) ~
		(push (truename \"./\") asdf:*central-registry*) ~
		(ql:quickload :dlib :verbose nil) ~
		(ql:quickload :lish :verbose nil) ~
		(setf asdf:*central-registry* ~
		 (delete \"./\" asdf:*central-registry* :test #'equal)) ~
		(setf asdf:*central-registry* ~
		 (delete \"../\" asdf:*central-registry* :test #'equal)) ~
		(load \"build/fully-loaded.lisp\" :verbose nil) ~
		(lish:make-standalone :smaller t)"))
    (maybe-increment-build-version *version-file*)
    (run-with-input-from stream `(,*lisp* ,@*lisp-flags*
					  ,@*lisp-plain-flags* "--" "-norl"))))

#|

(defun install ()
  (loop :for target :in *targets* :do
     (build targets)
     (install-one target)))

(defun clean-one (file)
  (delete-file file))

(defun clean ()
  (loop :for target :in *targets* :do
     (clean-one target)))
|#

;; Main or something.

(defun main ()
  (let ((env-target (sf-getenv "TARGET")))
    (when (and env-target (not (zerop (length env-target))))
      (setf *default-target* (intern (string-upcase env-target)))))

  (flet ((env-or (env default-value)
	   (let ((result (sf-getenv env)))
	     (or (and result (split #\space result))
		 default-value))))
    (setf *lisp-flags*       (env-or "LISH_FLAGS" *lisp-flags*)
	  *lisp-plain-flags* (env-or "LISH_PLAIN_FLAGS" *lisp-plain-flags*)))

  (let (pos)
    (if (and (> (length (lisp-args)) 1)
	     (setf pos (position "--" (lisp-args) :test #'equal)))
	(build (intern (string-upcase (nth (1+ pos) (lisp-args)))))
	(build *default-target*)))

  (when (not (sf-getenv "NO_EXIT"))
    (exit-lisp :code *exit-code*)))

(main)

;; EOF
