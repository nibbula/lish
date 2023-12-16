;;;
;;; vars.lisp - Variables for Lish
;;;

(in-package :lish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Versioning

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

;; Pipeline input and output values

(defvar %*input* nil
  "The output of the previous command in pipeline.")

(defvar %*output* nil
  "The output of the current command.")

(defun input ()
  "The pipeline input object."
  %*input*)

(defun output ()
  "The pipeline output object."
  %*output*)

(defun export-input ()
  "Let external processes access *input*."
  (when (and (lish-export-pipe-results *shell*) %*input*)
    (setf (nos:env "LISH_INPUT") (princ-to-string %*input*))))

(defun export-output ()
  "Let external processes access *output*."
  (when (and (lish-export-pipe-results *shell*) %*output*)
    (setf (nos:env "LISH_OUTPUT") (princ-to-string %*output*))))

(defun set-input (value)
  "Set the pipeline input object to VALUE."
  (setf %*input* value)
  (export-input)
  value)

(defun set-output (value)
  "Set the pipeline output object to VALUE."
  (setf %*output* value)
  (export-output)
  value)

;; Possibly exporting *input* and *output* in the environment to sub-processes
;; when they're set is the reason for the symbol macro and functions nonsense.
(defsetf input set-input)
(defsetf output set-output)

(define-symbol-macro *input* (input))
(define-symbol-macro *output* (output))
(define-symbol-macro *i* (input))
(define-symbol-macro *o* (output))

(defmacro with-input ((x) &body body)
  "Dynamicly rebind ‘*input*' to ‘x’."
  `(let ((%*input* ,x))
     (export-input)
     ,@body))

(defmacro with-output ((x) &body body)
  "Dynamicly rebind ‘*output*' to ‘x’."
  `(let ((%*output* ,x))
     (export-output)
     ,@body))

(defvar *accepts* nil
  "What the next command in the pipeline accepts.")

(defvar *pipe-plus* nil
  "True to omap objects to a parenless function call.")

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

(defun default-lishrc ()
  (nos:path-append (nos:user-home) ".lishrc"))

(defvar *default-lishrc* (default-lishrc)
  "Default value for the start up file.")

(defvar *lishrc* nil
  "Pathname of the start up file.")

(defvar *use-bracketed-paste* t
  "True to use safer pasting in terminals that support it.")

(defvar *dont-deblarg* nil
  "True to not activate our debugger even if it's loaded.")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types?

(defstruct shell-expr
  "The result of the shell lexer. A sequence of words and their start and ~
end points in the original string."
  words					; sequence of objects
  line)					; the original string

(defstruct shell-word
  "A word in a shell-expr."
  word			; an object
  start			; integer start position
  end			; integer end position
  quoted		; boolean, true if word quoted
  eval			; boolean, true to evaluate word
  join)			; nil :left :right or :both, to add to adjacent word

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
  ;; True if we should not pass the *output*.
  pipe-dot
  ;; True if we should pipe both output and error
  pipe-both
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

;; End
