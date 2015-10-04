;;
;; vars.lisp - Variables for Lish
;;

(in-package :lish)

#|
(eval-when (:compile-toplevel)
  (format t "compile ~a~%"
	  (merge-pathnames (pathname "version.lisp") (or *load-pathname* ""))))

(eval-when (:load-toplevel)
  (format t "load ~a~%"
	  (merge-pathnames (pathname "version.lisp") (or *load-pathname* ""))))

(eval-when (:execute)
  (format t "execute ~a~%"
	  (merge-pathnames (pathname "version.lisp") (or *load-pathname* ""))))
|#

(defvar *version-file* "version.lisp")

(defun read-version ()
  (when (not (probe-file *version-file*))
    (setf *version-file* (tiny-rl:read-filename
			  :prompt "Where is version.lisp? ")))
  (with-open-file (str *version-file*)
    (safe-read-from-string (read-line str))))

(eval-when (:compile-toplevel)
  (declaim (sb-ext:muffle-conditions warning))
  (defconstant +version-flie+ *version-file*))

(defun write-version (version)
  (with-open-file (str *version-file*
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (prin1 version str) (terpri str)))

(defun update-version ()
  (write-version (1+ (read-version))))

;; We should almost always provide backwards compatibility as an option, but
;; releases with the same major version number should always be compatible
;; in the default configuration.
(defparameter *major-version* 0
  "Major version number. Releases with the same major version number should be
compatible in the default configuration.")

(defparameter *minor-version* 1
  "Minor version number. This should change at least for every release.")

(defparameter *build-version* (read-version)
  "Build version number. This should increase for every build, which probably
means every dumped executable.")

(defparameter *version*
  (format nil "~d.~d.~d" *major-version* *minor-version* *build-version*))

(defparameter *shell-name* "Lish"
  "The somewhat superfluous name of the shell.")

(defvar *shell* nil
  "The current shell instance.")

;; Like on windows this is #\; right? But not cygwin?
;; @@@ This should be in opsys 
(defvar *path-separator*
  #-windows #\:
  #+windows #\;
  "Separator in the PATH environement variable.")

;; @@@ Something else that should be in opsys
(defvar *buffer-size* (nos:getpagesize)
  "General buffer size for file or stream operations.")

(defparameter *options* nil
  "List of options defined.")

(defstruct shell-expr
  "The result of the shell lexer. A sequence of words and their start and ~
end points in the original string."
  words
  word-start
  word-end
  word-quoted
  line)

(defstruct shell-word
  "Used for temporarily splicing a shell-expr."
  word
  start
  end
  quoted)

(defstruct suspended-job
  id
  name
  command-line
  resume-function)

;; EOF
