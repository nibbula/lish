;;
;; vars.lisp - Variables for Lish
;;

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

(define-constant +version-file-name+ "version.lisp")

(defparameter *version-file*
  (or (asdf:system-relative-pathname :lish +version-file-name+)
      (probe-file (s+ (dirname
		       (or *compile-file-truename* *load-truename*))
		      *directory-separator* +version-file-name+))
      (probe-file (s+ (dirname (or *compile-file-truename* *load-truename*))
		      *directory-separator* ".."
		      *directory-separator* +version-file-name+))
      +version-file-name+))

(defun read-version ()
  (when (not (probe-file *version-file*))
    (format t "I thought it might be in ~s, but...~%" *version-file*)
    (setf *version-file* (tiny-rl:read-filename
			  :prompt "Where is version.lisp? ")))
  (with-open-file (str *version-file*)
    (safe-read-from-string (read-line str))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(defparameter *shell-name* "Lish"
  "The somewhat superfluous name of the shell.")

(defvar *shell* nil
  "The current shell instance.")

(defvar *lish-user-package*
  (make-package "LISH-USER" :use '(:cl :lish :cl-ppcre :glob)
		:nicknames '("LU"))
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

;; I really have some reservations about incuding this. It's somewhat
;; serendipitous that it works out this way, and it is highly efficient
;; keystroke-wise, but seems like an unorthoganal hackish trick.
(defvar ! nil
  "The previous command, so you can say e.g.: sudo !!")

;; These are my magic cookies. I would GENSYM them, but I would like them to
;; be constant over loads or systems.
(defparameter *real-eof-symbol* :Z-REAL-EOF-Z)
(defparameter *continue-symbol* :Z-CONTINUE-Z)
(defparameter *empty-symbol*    :Z-EMPTY-Z)
(defparameter *error-symbol*    :Z-ERROR-Z)
(defparameter *quit-symbol*     :Z-QUIT-Z)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types?

(defstruct shell-expr
  "The result of the shell lexer. A sequence of words and their start and ~
end points in the original string."
  words					; sequence of objects
  word-start				; sequence of word start positions
  word-end				; sequence of word end positions
  word-quoted				; sequence of booleans
  word-eval				; sequence of booleans
  line)					; the original string

(defstruct shell-word
  "Used for temporarily splicing a shell-expr."
  word					; an object
  start					; integer start position
  end					; integer end position
  quoted				; boolean, true if word quoted
  eval)					; boolean, true to evaluate word

(defstruct job
  id					; unique integer
  name					; string
  command-line				; string
  resume-function			; closure to resume
  pid					; process ID
  process-group				; process group ID
  status)				; what's going on with the job?

(defstruct context
  "The context for a command. It's input and output streams and environment
variables."
  ;; These are a stream or NIL for the default.
  in-pipe
  out-pipe
  ;; This is NIL for unspecified, :empty, or an alist environement.
  environment
  ;; True if we flipped *input* to *output* for a pipeline.
  flipped-io)

(defvar *context* nil
  "The dynamic command context. If it is NIL it is equivalent to all the
slots of the context being NIL, and so therefore equivalent to the default
input, output and environment.")

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

(defvar *exit-shell-hook* nil
  "Called when the shell exits.")

;; EOF
