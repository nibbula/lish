;;;
;;; shell.lisp - The shell object and it's options.
;;;

(in-package :lish)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
;(declaim (optimize (speed 3) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(defkeymap *lish-default-keymap* ()
  "Keymap for Lish."
  `((:f1		. shell-help-key)
    ;(,(ctrl #\v)	. shell-expand-line)
    (,(meta-char #\o)	. shell-expand-line)
    ))

;; @@@ I want to change all the lish-* accessors to shell-*
(defclass shell ()
  ((exit-flag
    :initarg :exit-flag
    :accessor lish-exit-flag
    :documentation "Set to true to exit the shell.")
   (exit-values
    :initarg :exit-values
    :accessor lish-exit-values
    :documentation "List of values to return to the caller.")
   (aliases
    :accessor lish-aliases
    :documentation "Hash table of aliases.")
   (global-aliases
    :accessor lish-global-aliases
    :documentation "Hash table of global aliases.")
   (editor
    :accessor lish-editor :initform nil
    :documentation "Line editor instance.")
   (history-store
    :initarg :history-store
    :accessor lish-history-store :initform nil
    :documentation "Where to save history.")
   (keymap
    :accessor lish-keymap
    :documentation "Keymap for the line editor.")
   (old-pwd
    :accessor lish-old-pwd
    :initform nil
    :documentation "The last working directory.")
   (dir-list
    :accessor lish-dir-list
    :initform nil
    :documentation "Directory list for pushd and popd.")
   (jobs
    :accessor lish-jobs :initarg :jobs :initform nil
    :documentation "List of jobs.")
   (last-background-job
    :accessor lish-last-background-job
    :initform nil
    :documentation
    "The last job run in background, or NIL if there wasn't one.")
   (start-time
    :initarg :start-time :accessor lish-start-time :type integer
    :documentation
    "Seconds elapsed since some time. Defaults to since shell was started.")
   (options
    :initarg :options :accessor lish-options :initform nil
    :documentation "Operator configurable options."))
  (:default-initargs
   :exit-flag nil
   :exit-values '()
   :start-time (get-universal-time)
   :keymap (copy-keymap *lish-default-keymap*))
  (:documentation "A lispy system command shell."))

(defmethod initialize-instance :after
    ((sh shell) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
;  (setf (slot-value sh 'commands) (make-hash-table :test #'equal))
  (setf (slot-value sh 'aliases) (make-hash-table :test #'equal))
  (setf (slot-value sh 'global-aliases) (make-hash-table :test #'equal))
  ;; Set default keymap
  (when (or (not (slot-boundp sh 'keymap)) (not (slot-value sh 'keymap)))
    (setf (slot-value sh 'keymap)
	  (copy-keymap *lish-default-keymap*))
    (define-key (slot-value sh 'keymap)
	#\escape (build-escape-map (slot-value sh 'keymap))))
  ;; Copy the objecs from the defined option list, and set the default values.
  (loop :with o :for opt :in *options* :do
     (setf o (shallow-copy-object opt)
	   (arg-value o) (arg-default o))
     (push o (lish-options sh)))
  (init-commands))

;; Most things that are designed to be settable by the user should likely
;; be made into an option. Options defined by DEFOPTION are accessible like a
;; typical class slot acessor method on the shell object, as well as being an
;; easily accesible using the 'opt' command.
;;
;; We think of options like they are arguments for the shell, and use
;; the argument class to store them. That way we can use the same completion
;; and conversion.

(defun find-option (sh name)
  "Find the option of the shell SH, named NAME. Error if there is none."
  (or (find (string name) (lish-options sh) :key #'arg-name :test #'equalp)
      (error 'shell-error :format "No such option ~w"
	     :arguments (list name))))

(defparameter *option-accessor-prefix* "LISH-")

(defun set-option (sh name value)
  "Set the option named NAME, for shell SH, to VALUE."
  (funcall (symbolify (s+ "SET-" *option-accessor-prefix* name)) value sh))

(defun get-option (sh name)
  "Get the option named NAME, for shell SH."
  (arg-value (find-option sh name)))

;; @@@ The whole option setting interface is stupid, and we should probably
;; convert to just using (option name) and (setf (option name))

(defun option (name)
  (arg-value (find-option *shell* name)))

(defun %set-option (name value)
  (set-option *shell* name value)
  value)

(defsetf option %set-option
  "Set a shell option.")

(defmacro defoption (name &rest arg)
  "Define a shell option named NAME, with the properties in arg. The syntax
is like Lish arguments, e.g.:
  (defoption \"foo\" type :help \"Make sure to foo.\" :short-arg #\\f)"
  (let* ((sym (symbolify (s+ "LISH-" name)))
	 (setter (symbolify (s+ "SET-" sym)))
	 (name-string (string-downcase name)))
    `(progn
       ;; Access options as if they were in the shell object.
       (defgeneric ,sym (shell)
	 (:documentation ,(s+ "Return the value of " name-string ".")))
       (defmethod ,sym ((sh shell)) (get-option sh ,name-string))
       (defgeneric ,setter (value shell)
	 (:documentation ,(s+ "Set the value of " name-string ".")))
       ;; Make a separate setter so it can be easily overridden.
       (defmethod ,setter (value (sh shell))
	 (setf (arg-value (find-option sh ',name)) value))
       (defgeneric (setf ,sym) (value shell)
	 (:documentation ,(s+ "Set the value of " name-string ".")))
       (defmethod (setf ,sym) (value (sh shell))
	 (,setter value sh))
       (push (make-argument (list ',name ',(first arg) ,@(rest arg)))
	     *options*))))

(setf *options* nil)

(defoption prompt object
  :help "Normal prompt. Output if there is no prompt function. Output
with SYMBOLIC-PROMPT-TO-STRING and FORMAT-PROMPT. See the documentation for
those functions for more detail about prompt formatting."
;;  :default nil
  :default
  '((:magenta "%u") (:green "@") (:cyan "%h") " " (:white "%w") (:red "%$") " "))

(defoption prompt-function function
  :help "Function which takes a SHELL and returns a string to output as the
prompt."
;;  :default make-prompt	       ; N.B.: #'make-prompt doesn't work here
  )

(defoption right-prompt object
  :help "Prompt for the right side of the input line. Output with
SYMBOLIC-PROMPT-TO-STRING and FORMAT-PROMPT. See the documentation for
those functions for more detail about prompt formatting."
  :default nil)

(defoption sub-prompt string
  :help "String to print when prompting for more input."
  :default "- ")	; @@@ maybe we need sub-prompt-char & sub-prompt-func?

(defoption ignore-eof integer
  :help "If true, prevent the EOF (^D) character from exiting the shell. If a 
number ignore it that many times before exiting."
  :default nil)

(defoption debug boolean
  :help "True to enter the debugger when there is an error."
  :default nil)

(defoption collect-stats boolean
  :help "True to collect statistics on commands."
  :default nil)

(defoption autoload-from-asdf boolean
  :help
  "True to try to load unknown commands from an ASDF system of the same name."
  :default t)

(defoption autoload-quietly boolean
  :help
  "True to suppress output from autoloading."
  :default t)

(defoption history-expansion boolean
  :help "True if !<integer> should expand to a history item."
  :default nil)

(defoption expand-braces boolean
  :help "True to expand braces in shell commands."
  :default t)

(defoption colorize boolean
  :help "True to colorize the command line."
  :default t)

(defoption auto-cd boolean
  :help "True to treat a directroy as a command to change to that directory."
  :default t)

(defoption history-style choice
  :help "Style of history to use. Simple stores just text lines. Fancy stores
more information, such as the date."
  :choices '("simple" "fancy")
  :default :fancy)

(defmethod history-style ((sh shell))
  (keywordify (get-option sh 'history-style)))

;; When changing the style, save and re-initialize the store.
(defmethod (setf history-style) (value (sh shell))
  (when (not (eq value (get-option sh 'history-style)))
    (finish-history sh)
    (set-option sh 'history-style value)
    (init-history sh)))

(defoption history-format choice
  :help "Style of history to use."
  :choices '("database" "text-file")
  ;; :default #.(if (getf rl-config::*config* :use-sqlite)
  ;; 		 :database :text-file))
  :default (if (getf rl-config::*config* :use-sqlite)
	       :database :text-file))

(defmethod history-format ((sh shell))
  (keywordify (get-option sh 'history-format)))

;; When changing the format, save and re-initialize the store.
(defmethod (setf history-format) (value (sh shell))
  (when (not (eq value (get-option sh 'history-format)))
    (finish-history sh)
    (set-option sh 'history-format value)
    (init-history sh)))

(defoption history-save-values boolean
  :help "True to save the result values of expressions in the history."
  :default nil)

(defoption command-glob boolean
  :help "Let Lish commands do their own globbing.")

(defoption auto-suggest boolean
  :help "True to make suggestions for the rest of the line."
  :default t)

(defmethod set-lish-auto-suggest (value (sh shell))
  (setf (arg-value (find-option sh 'auto-suggest)) value)
  (when (lish-editor sh)
    ;; There isn't always an editor.
    (setf (line-editor-auto-suggest-p (lish-editor sh)) value)))

(defoption partial-line-indicator object
  :help "A string to put at the end of partial lines before the prompt, or NIL
not to indicate partial lines."
  :default (span-to-fat-string '(:standout "%")))

(defmethod set-lish-partial-line-indicator (value (sh shell))
  (setf (arg-value (find-option sh 'partial-line-indicator)) value)
  (when (lish-editor sh)
    (setf (rl::partial-line-indicator (lish-editor sh)) value)))

(defoption export-pipe-results boolean
  :help "True to export LISH_INPUT and LISH_OUTPUT to sub-processes.")

(defoption auto-report-time integer
  :help "If a job takes longer than this number of seconds, report timing
statistics."
  :default -1)

;;; @@@ Shouldn't this be in the shell object?
;;; @@@ But it doesn't do anything right now anyway.
(defvar *shell-path* '()
  "List of directories to autoload commands from.")

;; EOF
