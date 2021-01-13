;;;
;;; vars.lisp - Variables for Lish
;;;

(in-package :lish)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Versioning

#|
(eval-when (:compile-toplevel)
  (format t "compile ~a~%"
	  (merge-pathnames (pathname "version.lisp") (or *compile-file-truename* ""))))

(eval-when (:load-toplevel)
  (format t "load ~a~%"
	  (merge-pathnames (pathname "version.lisp") (or *load-truename* ""))))

(eval-when (:execute)
  (format t "execute ~a~%"
	  (merge-pathnames (pathname "version.lisp") (or *load-truename* ""))))
|#

;; This is such a lame hack. But what would be a better way?

;; Actually, on second thought, this is a very bogus "page counter"-like thing
;; which doesn't really do anything for us, and introduces needless complexity.
;; Let's just lose it. But maybe keep the code around for a while, in case we
;; really need some stupid thing like this one day. For now, if there really
;; is some subtle version incompatibility that we need to check for, just
;; increase the version number by hand.
#|
(define-constant +build-version-file-name+ "build-version")
(define-constant +release-version-file-name+ "release-version")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun version-file-name (base)
    (or (asdf:system-relative-pathname :lish base)
	(probe-file (s+ (dirname
			 (or *compile-file-truename* *load-truename*))
			*directory-separator* base))
	(probe-file (s+ (dirname (or *compile-file-truename* *load-truename*))
			*directory-separator* ".."
			*directory-separator* base))
	base)))

(defparameter *build-version-file*
  (version-file-name +build-version-file-name+))

(defparameter *release-version-file*
  (version-file-name +release-version-file-name+))

(defun read-version ()
  (when (not (probe-file *build-version-file*))
    (when (not (probe-file *release-version-file*))
      (format t "I thought it might be in ~s, but...~%" *release-version-file*)
      (setf *release-version-file* (rl:read-filename
				    :prompt ("Where is ? "))))
    (uiop:copy-file *release-version-file* *build-version-file*))
  (with-open-file (str *build-version-file*)
    (safe-read-from-string (read-line str))))

(defun write-version (version)
  (with-open-file (str *version-file*
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (prin1 version str) (terpri str)))

(defun update-version ()
  (write-version (1+ (read-version))))
|#

;; @@@ Deprecated for yet another way
#|
;; We should almost always provide backwards compatibility as an option, but
;; releases with the same major version number should always be compatible
;; in the default configuration.
(defparameter *major-version* 0
  "Major version number. Releases with the same major version number should be
compatible in the default configuration.")

(defparameter *minor-version* 1
  "Minor version number. This should change at least for every release.")

#|
(defparameter *build-version* (read-version)
  "Build version number. This should increase for every build, which probably
means every dumped executable.")
|#
(defvar *build-version* 222
  "Build version number. Just add to it whenever you want to.")

(defparameter *version*
  (format nil "~d.~d.~d" *major-version* *minor-version* *build-version*))
|#

;; Yet another version of versions where presumably the new "improved" build
;; script will increment it.

(defparameter *version* #.(uiop:read-file-form
			   (asdf:system-relative-pathname
			    :lish "version.lisp")))

(let ((split-version (mapcar (_ (parse-integer _ :junk-allowed t))
			     (split-sequence #\. *version*))))
  (defparameter *major-version* (first split-version)
    "Major version number. Releases with the same major version number should be
compatible in the default configuration.")

  (defparameter *minor-version* (second split-version)
    "Minor version number. This should change at least for every release.")

  (defparameter *build-version* (third split-version)
    "Build version number. This should increase for every build, which probably
means every dumped executable."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(defparameter *shell-name* "Lish"
  "The somewhat superfluous name of the shell.")

(defvar *shell* nil
  "The current shell instance.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-user-package ()
    (or (and (boundp '*lish-user-package*)
	     (symbol-value '*lish-user-package*))
	(find-package "LISH-USER")
	(make-package "LISH-USER" :use '(:cl :lish
					 #+use-regex :regex
					 #-use-regex :cl-ppcre
					 :glob)
		      :nicknames '("LU")))))

(defparameter *lish-user-package* (make-user-package)
  "Package for lish to hang out in. Auto-updates from :cl-user.")

(defvar *junk-package*
  (progn
    (when (find-package :lish-junk)
      (delete-package :lish-junk))
    (make-package :lish-junk))
  "Package to put partial or unknown reader junk.")

;; @@@ Something else that should be in opsys?
(defvar *buffer-size* (nos:memory-page-size)
  "General buffer size for file or stream operations.")

(defparameter *options* nil
  "List of options defined.")

(defvar *default-lishrc* "$HOME/.lishrc"
  "Default value for the start up file.")

(defvar *lishrc* nil
  "Pathname of the start up file.")

;; I really have some reservations about including this. It's somewhat
;; serendipitous that it works out this way, and it is highly efficient
;; keystroke-wise, but seems like an unorthogonal hackish trick.
(defvar ! nil
  "The previous command, so you can say e.g.: sudo !!")

;; These are my magic cookies. I would GENSYM them, but I would like them to
;; be constant over loads or systems.
(defparameter *real-eof-symbol* :Z-REAL-EOF-Z)
(defparameter *continue-symbol* :Z-CONTINUE-Z)
(defparameter *empty-symbol*    :Z-EMPTY-Z)
(defparameter *error-symbol*    :Z-ERROR-Z)
(defparameter *quit-symbol*     :Z-QUIT-Z)

;;; A place to put the list of visited directories.
;;; Has an hardcoded length of 32 directories.
(defvar *directory-ring* '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types?

(defstruct shell-expr
  "The result of the shell lexer. A sequence of words and their start and ~
end points in the original string."
  words					; sequence of objects
 #|
  word-start				; sequence of word start positions
  word-end				; sequence of word end positions
  word-quoted				; sequence of booleans
  word-eval				; sequence of booleans
|#
  line)					; the original string

(defstruct shell-word
  "A word in a shell-expr."
  word					; an object
  start					; integer start position
  end					; integer end position
  quoted				; boolean, true if word quoted
  eval)					; boolean, true to evaluate word

;; @@@ I'm not really sure if this is good idea yet.
(defmethod print-object ((object shell-word) stream)
  "Print a shell-word to STREAM."
  (with-slots (word quoted) object
    (cond
      ((or *print-readably* *print-escape*) (call-next-method))
      (quoted (format stream "\"~a\"" word))
      (t (format stream "~a" word)))))

(defstruct context
  "The context for a command. It's input and output streams and environment
variables."
  ;; These are a stream or NIL for the default.
  in-pipe
  out-pipe
  ;; This is NIL for unspecified, :empty, or an alist environement.
  environment
  ;; True if we flipped *input* to *output* for a pipeline.
  flipped-io
  ;; True if we should run things in the background.
  background
  ;; True if we should map parenless function calls.
  pipe-plus
  )

(defvar *context* nil
  "The dynamic command context. If it is NIL it is equivalent to all the
slots of the context being NIL, and so therefore equivalent to the default
input, output and environment.")

;; We do a simple fake out for signals on OS's that don't really support them.
(defparameter *siggy*
  #+windows
  '(("TERM" 15 terminate-process)
    ("STOP" 17 suspend-process)
    ("CONT" 19 resume-process))
  #+unix
  (loop :for i :from 1 :below os-unix:*signal-count*
     :collect (list (os-unix:signal-name i) i))
  "Fake windows signals.")

(defparameter *signal-names*
  #+unix (make-array
	  (list os-unix:*signal-count*)
	  :initial-contents
	  (cons ""
		(loop :for i :from 1 :below os-unix:*signal-count*
		   :collect (os-unix:signal-name i))))
  #+windows (mapcar #'car *siggy*)
  "Names of the signals.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks

(defvar *pre-command-hook* nil
  "Called before a command is run. Arguments are:
 COMMAND-NAME : string   - The name of the command.
 COMMAND-TYPE : keyword  - What kind of command it is.")

(defvar *post-command-hook* nil
  "Called after a command is run. Arguments are:
 COMMAND-NAME : string   - The name of the command.
 COMMAND-TYPE : keyword  - What kind of command it is.")

(defvar *unknown-command-hook* nil
  "Called when a command is not found. Arguments are the shell and the
condition.")

(defvar *enter-shell-hook* nil
  "Called when the shell starts, after loading the *lishrc*.")

(defvar *exit-shell-hook* nil
  "Called when the shell exits.")

;; EOF
