;;;
;;; builtin.lisp - Lish built-in commands.
;;;

;; Here we define the commands that are built in to Lish.

;; Most of these are really just for compatability with a POSIX shell, so
;; perhaps on another operating system you might not need them.
;; For example we might have a set of commands for an internet appliance
;; like a router.
;; @@@ Perhaps we should be able to load a built-in ‘personality’.

(in-package :lish)

;; (declaim (optimize (speed 0) (safety 3) (debug 3) (space 1)
;; 		   (compilation-speed 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command definitions

(defun home-directory ()
  "Return a namestring of the user's home directory in a particular way."
  (or (nos:environment-variable "HOME") (nos:user-home)
      (namestring (user-homedir-pathname))))

(defbuiltin cd ((directory directory :help "Directory to change to."))
  "Change the current directory to DIRECTORY. If DIRECTORY isn't specified,
use *input* if it's a directory or the shell's idea of the user's home
directory."
  (when (equal directory "-")
    (setf directory (lish-old-pwd *shell*)))
  (when (begins-with "=" directory)
    (setf directory (find-directory-in-ring (subseq directory 1))))
  (push-directory-ring (nos:current-directory))
  (setf (lish-old-pwd *shell*) (nos:current-directory))
  (nos:change-directory (or directory
			    (and *input* (nos:directory-p *input*) *input*)
			    (home-directory)))
  ;; Update $PWD like traditional Unix shells.
  ;; @@@ Maybe someday we can get rid of this.
  (setf (nos:environment-variable "PWD") (nos:current-directory)))

(defbuiltin pwd ()
  "Print the current working directory."
  (format t "~a~%" (nos:current-directory)))

(defbuiltin pushd
  ((directory directory :help "Directory to push on the stack."))
  "Change the current directory to DIRECTORY and push it on the the front of the
directory stack."
  (when (not directory)
    (setf directory (pop (lish-dir-list *shell*))))
  (push (nos:current-directory) (lish-dir-list *shell*))
  (!cd :directory directory))

(defbuiltin popd ((number number :help "Number of item to pop."))
  "Change the current directory to the top of the directory stack and remove it
from stack."
  (declare (ignore number))
  (let ((dir (pop (lish-dir-list *shell*))))
    (!cd :directory dir)
    dir))

(defbuiltin dirs ()
  "Show the directory stack."
  (format t "~{~a~%~}" (lish-dir-list *shell*))
  (setf *output* (lish-dir-list *shell*)))

(defbuiltin suspend ()
  "Suspend the shell."
  (opsys:suspend-process))

(defun job-id-list ()
  "Return a list of suspended job ids."
  (loop :for j :in (lish-jobs *shell*)
     :collect (job-id j)))

(define-builtin-arg-type job-descriptor (arg-lenient-choice)
  "A job descriptor."
  ()
  (:default-initargs
   :choice-func #'job-id-list))

;; (defclass arg-job-descriptor (arg-lenient-choice)
;;   ()
;;   (:default-initargs
;;    :choice-func #'job-id-list)
;;   (:documentation "A job descriptor."))

(defbuiltin fg
  ((job-descriptor job-descriptor :optional t :help "Job to resume."))
  "Resume a suspended job."
  (let (job)
    (cond
      ((or (null (lish-jobs *shell*))
	   (= (length (lish-jobs *shell*)) 0))
       (format t "No jobs to resume.~%")
       (return-from !fg (values)))
      ((= (length (lish-jobs *shell*)) 1)
       (setf job (first (lish-jobs *shell*))))
      (t
       (setf job (find-job job-descriptor))))
    (if (not job)
	(format t "Couldn't find a job matching ~a.~%" job-descriptor)
	(continue-job-in-foreground job))))

(defbuiltin bg
  ((job-descriptor job-descriptor :optional t :help "Job to backaground."))
  "Put a job in the background."
  (let ((job (find-job job-descriptor)))
    ;; (format t "job-descriptor = ~s ~a job = ~s~%"
    ;; 	    job-descriptor (type-of job-descriptor) job)
    (if job
	(continue-job-in-background job)
	(format t "Couldn't find a job matching ~a.~%" job-descriptor)))
  (values))

(defbuiltin jobs
  ((long boolean :short-arg #\l :help "Show the longer output.")
   (all boolean :short-arg #\a :help "Show all (or at least maybe more) jobs."))
  "Lists spawned processes that are active."
  (flet ((print-job (j)
	   (with-slots (id name command-line status) j
	     (if long
		 (typecase j
		   (lisp-job
		    (format t "~3d ~10a ~20a ~:[~;~a ~]~a~%"
			    id (job-type-name j) name long
			    (job-resume-function j) command-line))
		   (system-job
		    (format t "~3d ~10a ~20a ~:(~a~) ~:[~;~a ~]~a~%"
			    id (job-type-name j) name status long (job-pid j)
			    command-line))
		   (t
		    (format t "~3d ~10a ~20a ~:(~a~) ~a~%"
			    id (job-type-name j) name status command-line)))
		 (format t "~3d ~10a ~20a ~:(~a~) ~a~%"
			 id (job-type-name j) name status command-line)))))
    (loop :for j :in (lish-jobs *shell*)
       :do
	 (print-job j))
    (when all
      ;; (mapc (_ (mapc #'print-job (list-all-jobs _))) *job-types*))))
      (mapc (_ (mapc #'print-job (list-all-jobs _))) '(lisp-job thread-job)))))

(defbuiltin history
  ((clear boolean :short-arg #\c
    :help "Clear the history.")
   (write boolean :short-arg #\w
    :help "Write the history to the history file.")
   (read boolean :short-arg #\r
    :help "Read the history from the history file.")
   (append boolean :short-arg #\a
    :help "Append the history to the history file.")
   (read-not-read boolean :short-arg #\n
    :help "Read history items not already read from the history file.")
   (filename pathname :short-arg #\f
    :help "Use PATHNAME as the history file.")
   (show-times boolean :short-arg #\t
    :help "Show history times.")
   (delete integer :short-arg #\d
    :help "Delete the numbered history entry.")
   (table boolean :short-arg #\T
    :help "True to return a table of the history."))
  "Show a list of the previously entered commands."
  ;; Check argument conflicts
  (cond ;; @@@ Could this kind of thing be done automatically?
    ((and clear (or write read append read-not-read filename show-times delete))
     (error "CLEAR should not be given with any other arguments."))
    ((and delete (or write read append read-not-read filename show-times clear))
     (error "DELETE should not be given with any other arguments."))
    ((> (count t `(,write ,read ,append ,read-not-read)) 1)
     (error
      "Only one of WRITE, READ, APPEND, or READ-NOT-READ should be given."))
    ((and filename (not (or read write append read-not-read)))
     (error
      "FILENAME is only useful with READ, WRITE, APPEND, or READ-NOT-READ.")))
  (cond
    (clear
     (rl:history-clear :lish))
    ;; @@@ It might be nice if we could say what happend, like how many
    ;; history items were saved or loaded.
    (write         (save-history *shell*))
    (read          (load-history *shell*))
    (append        (save-history *shell* :update t))
    (read-not-read (load-history *shell* :update t))
    (filename
     (setf (history-store-file-name (lish-history-store *shell*))
	   filename))
    (delete (format t "Sorry, delete is not implemented yet.~%"))
    (table (setf *output* (rl:history-table :context :lish)))
    (t (rl:show-history :context :lish :show-time show-times
			:show-extra (get-option *shell* 'history-save-values)))))

;; This seems stupid and unnecessary. 
;; (defbuiltin #:|:| (("args" t :repeating t))
;;   "Arguments are evaluated for side effects."
;;   (declare (ignore args))
;;   (values))

(defbuiltin echo
  ((no-newline boolean :short-arg #\n :help "Don't output a newline.")
   (args t :rest t :help "Words to print."))
  "Output the arguments. If -n is given, then don't output a newline a the end."
  (format t "~{~a~#[~:; ~]~}" args)
  (when (not no-newline)
    (terpri)))

(defparameter *help-subjects*
  '("commands" "builtins" "external" "editor" "keys" "options" "syntax"
    "startup" "differences")
  "Subjects we have help about.")

(defun help-choices ()
  "Return a list of choices for a help subject."
  (concatenate
   'list *help-subjects*
   (mapcar #'(lambda (x)
	       (or (and (symbolp x) (string-downcase (symbol-name x)))
		   (and (stringp x) x)
		   x))
	   *command-list*)))

(define-builtin-arg-type help-subject (arg-choice)
  "Something which we can get help on."
  ()
  (:default-initargs
   :choice-func
      #+clisp 'help-choices		; I'm not sure why.
      #-clisp #'help-choices))

(defparameter *basic-help*
"~
Lish version ~a help:
  This is Lish, a command line shell with Lisp.
  A brief summary of what to type:

  command [arg*...]   Run a program in your path with the given ARGs.
  ([expressions]...)  Evaluate Lisp expressions.
  help [subject]      Show help on the subject.
  exit                Exit the shell.

Further help is available on these subjects:

  help builtins       Show help on built-in commands.
  help editor         Show help on the line editor.
  help keys           Show help on key bindings.
  help options	      Show help on shell options.
  help syntax         Show help on shell syntax.
  help startup        Show help on what happens when the shell starts.
  help differences    Show help on differences from other shells.
  help commands       Show help on added commands.
  help external       Show help on external commands.
  help <command>      Show help for the command.
")

(defparameter *editor-help*
"You can use some Emacs-like commands to edit the command line.

Some notable keys are:
 <Tab>        Try to complete the word in front of the cursor. Twice for more.
 ?            Show what input is expected. List possibilities.
 <Control-D>  Quit, when on an empty line, or delete the following character.
 <Control-P>  Previous history line. Also the <Up Arrow> key.
 <Control-N>  Next history line. Also the <Down Arrow> key.
 <Control-B>  Move the cursor back one character. Also the <Left Arrow> key.
 <Control-F>  Move the cursor forward one character. Also the <Right Arrow> key.
 <Control-Q>  Quote next character, like if you want to really type '?'.
 <F9>         Switch back and forth between LISH and the lisp REPL.
")

;; @@@ I want this to be markdown or something. At least fit the paragraphs to
;; the width of the terminal.
(defparameter *syntax-help*
"The syntax is a combination of POSIX shell and Lisp, hopefully in a way that
is familiar and not too surprising to those who know either.
It is vaguely like:
  ; comment
  command [arg...]
  command \"string\" ,*lisp-object* ,(lisp-code) $ENV_VAR
  command *.glob ?ooba[rz]
  command word\\ with\\ spaces \"string \\\" with a double quote\"
  command | command | ...
  command < file-name
  command > file-name
  ([lisp expressions...])

Basically, inside parentheses you get Lisp reader syntax. Outside parentheses,
you get a very simplified shell syntax with Lisp strings and comments.
Some typical shell expansions are done in command arguments, such as shell
globbing with *,?,and [], environment variable expansions with $VAR, and
home directory expansions with ~~user. Pipeline and redirections should work
nearly as expected.

Commands can be:
  - System executables in your standard PATH
  - Built-in or later defined commands, defined with DEFCOMMAND
  - Names of systems in your ASDF \"path\" which are expected to define a
    command with the same name as the system, which is then invoked.
  - Lisp functions or methods
")

(defparameter *startup-help*
  "Here's what the shell does when starting:

  - Increment the *lish-level* and LISH_LEVEL.
  - Update the LISH-USER package with symbols from CL-USER.
  - Load the startup commands file, which comes from one of:
      - The value passed to the shell with the :init-file keyword
      - The value of *lishrc*
      - The standard place for configuration files:
        (opsys:path-append (opsys:config-dir \"lish\") \"lishrc\")
        which is: ~s
      - The value of *default-lishrc*,
        which is: ~s
    The current value is: ~s
  - Make sure *theme* is set, using the (default-theme) if necessary.
  - Make a new line editor, which might make a new *terminal*. See the
    documentation for RL for details.
  - Evaluate the *enter-shell-hook* functions.
    The current value is: ~a.~%~%")

(defparameter *differences-help*
  "Lish is very different from a POSIX shell. The most notable differences are:

- Parentheses switch to Lisp syntax, and don't mean run in sub-shell.
  Lisp inside parentheses is evaluated and substituted in the current line.
- String quoting is done only with double quote \". Single quote ' and back
  quote `, are not special to avoid confusion with Lisp.
- The prefix VAR=value isn't supported. Use the ‘env’ command instead.
- Redirection syntax is different, e.g \"2>&1\" doesn't work.
- Commands can be Lish commands and Lisp functions, as well as executables
  in your PATH. Lish commands can be searched for and automatically loaded
  from ASDF places, manipulated by the ‘ldirs’ command.
- Most scripting related shell commands are missing, e.g. if, test, case.
  Scripting parameter expansion like $1 $* ${} are missing. Use Lisp instead.
- Shell expansions are different. Many expansions can be done by Lish functions
  starting with ! , such as (!_ \"ss\") expands to a list of strings of the
  lines of output, (!? \"grep fuse /proc/filesystems\") returns a boolean status.
  Comma can be used to substitute a Lisp value, e.g. \"echo ,*package*\".
- Comments start with ; not #

For more detail see the section ‘Differences from POSIX shells’ in docs/doc.org
")

(defun print-columnar-help (rows)
  ;; (with-input-from-string
  ;;     (in-str (with-output-to-string (str)
  ;; 		(nice-print-table
  ;; 		 rows nil :trailing-spaces nil
  ;; 		 :stream str)))
  ;;   (with-lines (l in-str)
  ;;     ;; add spaces in front and clip to screen columns
  ;;     (format t "  ~a~%" (subseq l 0 (min (length l)
  ;; 					  (- (get-cols) 2)))))))
  (let* ((prefix-size (loop :for a :in rows :maximize (length (car a))))
	 (prefix-len (+ prefix-size 5))
	 (prefix-string (make-string prefix-len :initial-element #\space)))
    (loop :for (a b) :in rows
       :do
       (format t "  ~v,a : " prefix-size a)
       (when b
	 (justify-text (substitute #\space #\newline b)
		       :prefix prefix-string :start-column prefix-len
		       :omit-first-prefix t :cols (get-cols)))
       (terpri))))
  
(defun command-list (type)
  "Return a list of commands of TYPE."
  (sort (loop :for k :being :the :hash-values :of (lish-commands)
	   :when (typep k type)
	   :collect k)
	#'string-lessp
	:key #'command-name))

(defun print-multiple-command-help (commands)
  (let ((rows
	 (loop :with doc :and pos
	    :for k :in commands :do
	    (setf doc (documentation (command-function k) 'function)
		  pos (position #\. doc))
	    :collect
	    (list
	     (command-name k)
	     ;; Only the first sentance, i.e. up to the first period,
	     ;; without newlines.
	     (substitute #\space #\newline
			 (if pos
			     (subseq doc 0 (1+ pos))
			     doc))))))
    (print-columnar-help rows)))

;; This has to make sure to be able to operate without a current shell or even,
;; current terminal, since it's called by the documentation method.
(defun print-command-help (cmd &optional (stream
					  ;; (or *terminal* *standard-output*)
					  *standard-output*
					  ))
  "Print documentation for a command. Return a table."
  (with-grout (*grout* stream)
    (grout-format "~a~%" (documentation cmd 'function))
    (let (table)
      (when (and (command-arglist cmd)
		 (not (zerop (length (command-arglist cmd)))))
	(grout-format "Arguments:~%")
	(grout-print-table
	 (setf table
	       (make-table-from
		(loop :for a :in (command-arglist cmd)
		   :when (not (arg-hidden a))
		   :collect
		   (list (if (arg-short-arg a) (s+ "  -" (arg-short-arg a)) "  ")
			 (if (arg-long-arg a)  (s+ "--" (arg-long-arg a))
			     (if (arg-short-arg a) "" (arg-name a)))
			 #| (or (arg-default a) "") |#
			 (string-downcase (arg-type a))
			 (or (and (slot-boundp a 'help)
				  (substitute #\space #\newline (arg-help a)))
			     (arg-name a))))
		:columns
		'((:name "Short") (:name "Long") (:name "Type")
		  (:name "Help" :align :wrap))))
	 :long-titles nil :print-titles nil #|:max-width (get-cols)|#
	 :trailing-spaces nil))
      (when (and (command-accepts cmd)
		 (not (eq (command-accepts cmd) :unspecified)))
	(grout-format "~&Accepts: ~a~%" (command-accepts cmd)))
      (typecase cmd
	(command
	 (when (and (not (command-built-in-p cmd))
		    (command-loaded-from cmd))
	   (grout-format "Loaded from: ~a~%" (command-loaded-from cmd))))
	;; (autoloaded-command
	;;  (grout-format "Load from: ~a~%" (command-load-from cmd)))
	)
      table)))

;; For use by other things. Like my "doc" command.
;; This used to be (eql :command), but that was take by lispworks, so fuck it.
(defmethod documentation ((symbol symbol) (type (eql 'command)))
  (let ((cmd (get-command (string-downcase (symbol-name symbol)))))
    (when cmd
      (with-output-to-string (str)
	(print-command-help cmd str)))))

(defbuiltin help ((subject help-subject :help "Subject to get help on."))
  "Show help on the subject. Without a subject show some subjects that are
available."
  (if (not subject)
      (progn
	(format t *basic-help* *version*))
      ;; topics
      (let ((subject-kw (keywordify subject)))
	(cond
	  ((eq subject-kw :builtins)
	   (format t "Built-in commands:~%")
	   (print-multiple-command-help (command-list 'builtin-command)))
	  ((eq subject-kw :commands)
	   (format t "Defined commands:~%")
	   (print-multiple-command-help (command-list 'shell-command)))
	  ((eq subject-kw :external)
	   (format t "Defined external commands:~%")
	   (print-multiple-command-help (command-list 'external-command)))
	  ((eq subject-kw :editor) (format t *editor-help*))
	  ((eq subject-kw :syntax) (format t *syntax-help*))
	  ((eq subject-kw :differences) (format t *differences-help*))
	  ((eq subject-kw :options)
	   (format t "~
Options can be examined and changed with the ‘opt’ command.~%~
Shell options:~%")
	   (print-columnar-help
	    (loop :for o :in (lish-options *shell*) :collect
	       `(,(arg-name o)
		  ,(substitute #\space #\newline (arg-help o))))))
	  ((eq subject-kw :keys)
	   (format t "Here are the keys active in the editor:~%")
	   (!bind :print-bindings t))
	  ((eq subject-kw :startup)
	   (format t *startup-help*
		   (opsys:path-append (opsys:config-dir "lish") "lishrc")
		   *default-lishrc*
		   *lishrc*
		   *enter-shell-hook*))
	  (t ;; Try a specific command
	   (let* ((cmd  (get-command subject))
		  (symb (intern (string-upcase subject) :lish))
		  (doc  (when cmd (documentation cmd 'function)))
		  (fdoc (when (fboundp symb)
			  (documentation (symbol-function symb) 'function))))
	     ;; (print-values* (subject cmd symb doc fdoc))
	     (cond
	       (doc  (setf *output* (print-command-help cmd)))
	       (fdoc (format t "Lisp function:~%~a~%" fdoc))
	       (cmd  (format t "Sorry, there's no help for \"~a\".~%" subject))
	       (t    (format t "I don't know about the subject \"~a\"~%"
			     subject)))))))))

(defmethod documentation ((b command) (doctype (eql 'function)))
  "Return the documentation string for the given shell command."
  (with-output-to-string (str)
    (format str "~a" (posix-synopsis b))
    (when (documentation (command-function b) 'function)
      (format str "~%~a" (documentation (command-function b) 'function)))
#|    (when (command-loaded-from b)
      (format str "~%Loaded from ~a" (command-loaded-from b))) |#
    ))

(defun set-alias (name expansion &key global (shell *shell*))
  "Define NAME to be an alias for EXPANSION.
NAME is replaced by EXPANSION before any other evaluation."
  (setf (gethash name
		 (if global
		     (lish-global-aliases shell)
		     (lish-aliases shell)))
	expansion))

(defun unset-alias (name &key global (shell *shell*))
  "Remove the definition of NAME as an alias."
  (remhash name (if global
		    (lish-global-aliases shell)
		    (lish-aliases shell))))

(defun get-alias (name &key global (shell *shell*))
  "Return the alias definition of ‘NAME’, or NIL if there isn't one.
- SHELL   The shell instance to look in, which defaults to *SHELL*.
- GLOBAL  Return the 'global' alias."
  (if global
      (gethash name (lish-global-aliases shell))
      (gethash name (lish-aliases shell))))

(defun alias (name)
  "Return the alias definition of NAME, or NIL if there isn't one."
  (gethash name (lish-aliases *shell*)))

(defsetf alias set-alias
 "Set the alias definition.")

(defun edit-alias (name &key global)
  (rl :prompt (s+ "alias " name " ")
      :string (or (get-alias name :global global
			     :shell *shell*)
		  "")))

(defbuiltin alias
    ((global    boolean :short-arg #\g :help "True to define a global alias.")
     (edit	boolean :short-arg #\e :help "True to edit the alias's value.")
     (name      string :help "Name of the alias.")
     (expansion string :help "Text to expand to."))
  "Define NAME to expand to EXPANSION when starting a line."
  (if (not name)
      (loop :for a :being :the :hash-keys
	 :of (if global (lish-global-aliases *shell*) (lish-aliases *shell*))
	 :do
	 (format t "alias ~a ~:[is not defined~;~:*~w~]~%"
		 a (get-alias a :global global :shell *shell*)))
      (if (not expansion)
	  (if edit
	      (set-alias name (edit-alias name) :global global)
	      (format t "alias ~a ~:[is not defined~;~:*~w~]~%"
		      name (get-alias name :global global :shell *shell*)))
	  (set-alias name expansion :global global :shell *shell*))))

(defbuiltin unalias
  (("global" boolean :short-arg #\g :help "True to define a global alias.")
   ("name" string :optional nil     :help "Name of the alias to forget."))
  "Remove the definition of NAME as an alias."
  (unset-alias name :global global :shell *shell*))

(defbuiltin exit (("values" string :repeating t :help "Values to return."))
  "Exit from the shell. Optionally return values."
  (when values
    (setf (lish-exit-values *shell*) (loop :for v :in values :collect v)))
  (setf (lish-exit-flag *shell*) t))

;; Override an implementations quit function, so that we only exit one level
;; of the shell, not the whole Lisp system.
(defbuiltin quit (("values" string :repeating t :help "Values to return."))
  :keys-as args
  "Exit from the shell. Optionally return values."
  (apply #'!exit args))

(defbuiltin source (("filename" pathname :optional nil
 		     :help "Filename to read."))
  "Evalute lish commands in the given file."
  (without-warning (load-file filename)))

;; XXX I wish this would work without using the :use-supplied-flag, just using
;; the default value of :toggle in boolean-toggle, but there is some kind of
;; bug or something about class default args at compile time that I don't
;; understand.

(defbuiltin debug
  ((state boolean-toggle :help "State of debugging." :use-supplied-flag t))
  "Toggle shell debugging."
  (setf (lish-debug *shell*)
	(if (or (not state-supplied-p) (eql state :toggle))
	    (not (lish-debug *shell*))
	    state))
  (format t "Debugging is ~:[OFF~;ON~].~%" (lish-debug *shell*)))

(defbuiltin export
  ((remove boolean :short-arg #\n
    :help "True to stop the NAME from being exported.")
   (edit boolean :short-arg #\e
    :help "True to edit the value of the variable.")
   (name  string :help "Name of the variable to export.")
   (value string :help "Value of the variable to export."))
  "Set environment variable NAME to be VALUE. Omitting VALUE, just makes sure
the current value of NAME is exported. Omitting both, prints all the exported
environment variables. If NAME and VALUE are converted to strings if necessary.
If NAME has an equal sign ‘=’ in it, do the POSIX shell style of NAME=value."
  (when (and name (not (stringp name)))
    (setf name (princ-to-string name)))
  (when (and value (not (stringp value)))
    (setf value (princ-to-string value)))
  (if name
      (let (pos)
	(cond
	  ((setf pos (position #\= name)) ; POSIX style
	   (let ((n (subseq name 0 pos))
		 (v (subseq name (1+ pos))))
	     (setf (nos:environment-variable n) v)))
	  (remove
	   (setf (nos:environment-variable name) nil))
	  (edit
	   (setf (nos:environment-variable name)
		 (rl :prompt (s+ "export " name #\=)
		     :string (or value (nos:environment-variable name)))))
	  (value
	   (setf (nos:environment-variable name) value))
	  (t
	   (nos:environment-variable name)))) ; Actually does nothing
      (dlib-interactive:printenv)))

#|-+
 |\|   So we have (from a man page):
 |\|
 |\|     env [-i] [name=value ...] [utility [argument ...]]
 |\|
 |\|   as a lambda list:
 |\|
 |\|     (&key ignore-environment variable-assignments shell-command)
 |\|
 |\|   but that doesn't have enough information. So
 |\|
 |\|    (:or "-i" "var=value" (:and "cmd")
 |\|@@@@@@@@@@@@@@@
 |\|
 |\|    (("ignore-environment" boolean :short-arg #\i
 |\|      :help "Ignore the environment.")
 |\|     :positional
 |\|     ("variable-assignment" string :repeating t :matches "\\S+=\\S+"
 |\|      :help "Assingment to make in the environment.")
 |\|     ("shell-command" shell-command
 |\|      :help "Command to execute with the possibly modified environment.")
 |\|     ("arguments" string :repeating t
 |\|      :help "Variable assignments, commands and command arguments."))
 |\|

Vauguely like how I would like:

(defbuiltin env
    ((ignore-environment boolean :short-arg #\i
      :help "Ignore the environment.")
     (variable-assignment string :repeating t
      :help "Assingment to make in the environment.")
     (shell-command shell-command
      :help "Command to execute with the possibly modified environment.")
     (arguments string :repeating t
      :help "Variable assignments, commands and command arguments."))
  "Modify the command environment. If ignore-environment"
  (if (and (not shell-command) (not arguments))
      ;; Just print variables
      (loop :for v :in variable-assignment
	 :do
	 (let ((var (if (position #\= v)
			(first (split-sequence #\= v))
			v)))
	   (when var
	     (format t "~a=~a~%" var (nos:environment-variable var)))))
      ;; Set variables and execute command
      (progn
	(loop :for v :in variable-assignment
	   :do
	   (let ((pos (position #\= v))
		 var val seq)
	     (if pos
		 (setf seq (split-sequence #\= v)
		       var (first seq)
		       val (third seq))
		 (setf var v))
	     (when (and var val)
	       (setf (nos:environment-variable var) val))))
	(apply #'do-system-command
	       `(,`(,shell-command ,@arguments)
		   ,(if ignore-environment
			(modified-context *context* :environment nil)
			*context*))))))

But instead we have to to a kludgey version:
|#

;; Another problem is, it's rather counter-intuitive to POSIX shell users
;; (including me), that, say,
;;
;;   env "CFLAGS=-g -Wall -O2" ./configure --enable-impossible-color ...
;;
;; works, but
;;
;;   env CFLAGS="-g -Wall -O2" ./configure --enable-impossible-color ...
;;
;; doesn't work.
;; It doesn't work because the shell reader splits words at double quotes,
;; so it becomes "env" "CFLAGS" "=-g -Wall -O2". The reader splits words like
;; that for what I think is good reason, so I'm not sure I want to change it.

(defbuiltin env
  ((ignore-environment boolean :short-arg #\i
    :help "Ignore the environment.")
   (arguments string :rest t
    :help "Variable assignments, command and command arguments.
Any number of leading words with equal signs '=' are taken to be environment
variable assignments. The rest of the arguments are taken to be a command with
and it's arguments. The environment variable assignments are only in effect for
the command."))
  "Modify the command environment. If ignore-environment is true, only
variables explicitly set in arguments are passed to the command."
  (if arguments
      ;; Set variables and execute command
      (let (env new-env cmd (a arguments) args)
	;; Accumulate environment modifications in env
	(loop
	   :while (and a (position #\= (car a)))
	   :do
	   (let* ((seq (split-sequence #\= (car a)))
		  (var (first seq))
		  (val (second seq)))
	     (when var
	       ;; (format t "push env var=~s val=~s~%" var val)
	       (push (cons var val) env)))
	   (setf a (cdr a)))
	;; Set the command and arguments
	(setf cmd (car a)
	      args (cdr a))
	(when (not ignore-environment)
	  (setf new-env (or (context-environment *context*)
			    (environment))))
	;; Set the variables
	;; (format t "cmd = ~s~%args = ~s~%env = ~s~%" cmd args env)
	;; (finish-output)
	(loop :with e
	   :for (var . val) :in env
	   :do
	   (setf e (assoc (intern var :keyword) new-env))
	   (if e
	       (rplacd e val)
	       (setf new-env (acons (intern var :keyword) val new-env))))
	;; Unixify the new-env
	;;(setf new-env
	;;      (mapcar (_ (format nil "~a=~a" (car _) (cdr _))) new-env))
	;; Run the command
	;; @@@ This should respect piping!!!
	(when cmd
	  ;; (funcall #'do-system-command
	  ;; 	   (make-shell-expr :words `(,cmd ,@args))
	  ;; 	   (modified-context *context* :environment new-env))))
	  (funcall #'shell-eval
		   ;; (make-shell-expr :words `(,cmd ,@args))
		   (shell-read (join-by-string (append (list cmd) args) " "))
		   :context (modified-context *context* :environment new-env))))
      ;; Just print the variables
      (loop :for e :in (environment)
	 :do (format t "~a=~a~%" (car e) (cdr e)))))

(defun get-cols ()
  ;; (let ((tty (rl:line-editor-terminal (lish-editor *shell*))))
  ;;   (terminal-get-size tty)
  ;;   (terminal-window-columns tty))
  (or (and *terminal* (tt-width)) 80))

(define-builtin-arg-type signal (arg-integer)
  "A system signal."
  ()
  :convert string
  (or #+unix (position value *signal-names* :test #'equalp)
      #+windows (cadr (assoc value *siggy* :test #'equalp))
      (ignore-errors (parse-integer value))
      value))

(defun pseudo-kill (sig job-or-pid)
  "Kill a job or an OS process ID."
  (labels ((kill-pid (p)
	     #+unix (os-unix:kill p (or sig uos:+SIGTERM+))
	     #+windows (funcall (caddr (find sig *siggy* :key #'second)) p)))
    (cond
      ((stringp job-or-pid)
       (let ((job (find-job job-or-pid)) pid-int)
	 (cond
	   ((job-p job)
	    (kill-job job :signal sig))
	   (t
	    (when (setf pid-int (ignore-errors (parse-integer job-or-pid)))
	      (kill-pid pid-int))))))
      ((job-p job-or-pid)
       (kill-job job-or-pid :signal sig))
      ((integerp job-or-pid)
       (kill-pid job-or-pid)))))

(defun pid-or-job-list ()
  "Return a list of jobs and process IDs."
  `(,@(job-id-list) ,@(mapcar #'nos:os-process-id (nos:process-list))))

;; (defclass arg-pid-or-job (arg-lenient-choice)
;;   ()
;;   (:default-initargs
;;    :choice-func #'pid-or-job-list)
;;   (:documentation "A job descriptor or process ID."))

(define-builtin-arg-type pid-or-job (arg-lenient-choice)
  "A process ID or a job."
  ()
  (:default-initargs
   :choice-func #'pid-or-job-list))

(defun pick-job ()
  )

(defbuiltin kill
  ((list-signals boolean    :short-arg #\l  :help "List available signals.")
   (interactive  boolean    :short-arg #\i  :help "Kill jobs interactively.")
   (signal       signal     :default "TERM" :help "Signal number to send.")
   (pids         pid-or-job :repeating t    :help "Process IDs to signal."))
  "Sends SIGNAL to PID."
  (let (job)
    (cond
      (list-signals
       (format t (s+ "~{~<~%~1," (get-cols) ":;~a~> ~}~%") ; bogus, but v fails
	       ;; (loop :for i :from 1 :below nos:*signal-count*
	       ;;  :collect (format nil "~2d) ~:@(~8a~)" i (nos:signal-name i))))
	       (loop :for s :in *siggy*
		  :collect (format nil "~2d) ~:@(~8a~)" (second s) (first s)))))
      (interactive
       ;; (when (setf job (pick-job))
       ;; 	 (pseudo-kill nil job))
       )
      (t
       (cond
	 (pids
	  (mapcar #'(lambda (x) (pseudo-kill signal x)) pids))
	 (signal
	  (typecase signal
	    (string
	     (if (setf job (find-job signal))
		 (pseudo-kill nil job)
		 (error "No such job ~s" signal)))
	    (integer
	     (pseudo-kill nil signal)))))))))

;; Actually I think that "format" and "read" are a bad idea / useless, because
;; they're for shell scripting which you should do in Lisp.

#|
;;; make printf an alias
(defbuiltin format
    (("format-string" string :optional nil :help "Format control string.")
     ("args" object :repeating t :help "Format arguments."))
  "Formatted output."
  (apply #'format t format-string args))

;; Since this is for scripting in other shells, I think we don't need to worry
;; about it, since the user can just call READ-LINE-like functions directly.
(defbuiltin read
    (("name"    string                 :help "Variable to read.")
     ("prompt"  string  :short-arg #\p :help "Prompt to print.")
     ("timeout" integer :short-arg #\t :help "Seconds before read times out.")
     ("editing" boolean :short-arg #\e :help "True to use line editing."))
  "Read a line of input."
  ;; @@@ totally faked & not working
  (declare (ignore timeout name))
  (if editing
      (rl:rl :prompt prompt)
      (read-line nil nil)))
|#

;; @@@ Maybe this should get the times of subprocesses too, but I think that
;; would necessitate forking and waiting, which I'm not sure is a good idea.
(defbuiltin time ((command string :repeating t :help "Command to time."))
  "Shows some time statistics resulting from the execution of COMMNAD."
  (time (shell-eval (expr-from-args command))))

(defun print-time (seconds micro-seconds &optional (stream t))
  (let* ((secs  (+ seconds (/ micro-seconds 1000000)))
	 days hours mins)
    (setf days  (/ secs (* 60 60 24))
	  secs  (mod secs (* 60 60 24))
	  hours (/ secs (* 60 60))
	  secs  (mod secs (* 60 60))
	  mins  (/ secs 60)
	  secs  (mod secs 60))
    ;; (format t "days ~a hours ~a min ~a sec ~a~%"
    ;; 	    (floor days) (floor hours) (floor mins) secs)
    (format stream
	    "~@[~dd ~]~@[~dh ~]~@[~dm ~]~5,3,,,'0fs"
            (when (>= days 1) (floor days))
            (when (>= hours 1) (floor hours))
            (when (>= mins 1) (floor mins))
            secs)))

(defbuiltin times ()
  "Show accumulated times for the shell."
  (let (self-u-sec self-u-ms self-s-sec self-s-ms
	children-u-sec children-u-ms children-s-sec children-s-ms)
    (setf (values self-u-sec self-u-ms self-s-sec self-s-ms)
	  (process-times :self)
	  (values children-u-sec children-u-ms children-s-sec children-s-ms)
	  (process-times :children))
    (format t "Self     User: ~a~32tSys: ~a~%"
	    (print-time self-u-sec self-u-ms nil)
	    (print-time self-s-sec self-s-ms nil))
    (format t "Children User: ~a~32tSys: ~a~%"
	    (print-time children-u-sec children-u-ms nil)
	    (print-time children-s-sec children-s-ms nil))))

(defbuiltin umask
    ((print-command boolean :short-arg #\p
      :help "Print a command which sets the umask.")
     (symbolic      boolean :short-arg #\S
      :help "Output in symbolic mode.")
     (mask	     string
      :help "Mask to set."))
  "Set or print the default file creation mode mask (a.k.a. permission mask).
If mode is not given, print the current mode. If PRINT-COMMAND is true, print
the mode as a command that can be executed. If SYMBOLIC is true, output in
symbolic format, otherwise output in octal."
  #+windows (declare (ignore print-command symbolic mask))
  #+windows (error "umask is not a thing on windows.")
  #+unix (declare (ignore symbolic)) ;; @@@
  #+unix
  (if (not mask)
      ;; printing
      (let ((current-mask (os-unix:umask 0)))
	(os-unix:umask current-mask)
	(when print-command
	  (format t "umask "))
	;; (if symbolic
	;;     (format t "~a~%" (symbolic-mode-offset current-mask))
	;;     (format t "~o~%" current-mode)))
	(format t "~o~%" current-mask))
      ;; setting
      (progn
	(multiple-value-bind (real-mask err)
	    (ignore-errors (parse-integer mask :radix 8))
	  (when (typep err 'error)
	    (error err))
	  (os-unix:umask real-mask)))))

(defparameter *limits*
  #+unix (loop :for l :in
	    '(:SBSIZE
	      :CORE
	      :DATA
	      :NICE
	      :FSIZE
	      :SIGPENDING
	      :KQUEUE
	      :MEMLOCK
	      :RSS
	      :NOFILE
	      :MSGQUEUE
	      :RTPRIO
	      :STACK
	      :CPU
	      :NPROC
	      :AS			; or maybe _VIRTMEM or _VMEM
	      :SWAP			; or maybe we need a getter?
	      :LOCKS
	      :NTHR			; _PTHREAD or _NTHR
	      ;; :PIPE not settable? uos:get-pipe-size
	      ;; :PTYS not settable? pseudo terminals?
	      )
	    :when (uos:rlimit-number l nil)
	    :collect l)
  #-unix '()
  "List of limits for ulimit command.")

(defbuiltin ulimit
  ((print-all boolean :short-arg #\a
    :help "True to print all limits.")
   (soft-limit boolean :short-arg #\S
    :help "True to operate on the soft limit.")
   (hard-limit boolean :short-arg #\H
    :help "True to operate on the hard limit.")
   (limit choice :choices *limits*
    :choice-test
    (lambda (x y) (or (eql x y) (eq (keywordify x) (keywordify y))))
    :help "Limit to print or set.")
   (value object :help "New value for the limit."))
  "Examine or set process resource limits."
  #-unix (declare (ignore print-all soft-limit hard-limit limit value))
  #-unix (values)
  #| #+unix (declare (ignore soft-limit)) |#
  #+unix
  (flet ((value->string (value)
	   (if (equal value uos:+RLIMIT-INFINITY+)
	       "unlimited"
	       value))
	 (string->value (value)
	   (if (and (or (stringp value) (symbolp value))
		    (equalp (string value) "unlimited"))
	       uos:+RLIMIT-INFINITY+
	       value))
	 (result->value (result)
	   (if hard-limit
	       (uos:rlimit-maximum result)
	       (uos:rlimit-current result))))
    (cond
      (print-all
       (let ((table (make-table-from
		     (loop :with val
			:for l :in *limits*
			:when (uos:rlimit-number l nil)
			:do
			(setf val (uos:getrlimit l))
			:and
			:collect (list (string-downcase l)
				       (value->string (uos:rlimit-current val))
				       (value->string (uos:rlimit-maximum val))
				       (documentation
					(symbolify (s+ "+RLIMIT-" l "+")
						   :package :uos)
					'variable)))
		     :columns
		     '((:name "Name")
		       (:name "Soft Value")
		       (:name "Hard Value")
		       (:name "Description" :align :wrap)))))
	 ;; (print-table table :max-width (get-cols))
	 ;; (terpri)
	 (with-grout ()
	   (grout-print-table table))
	 (setf *output* table)))
      (limit
       (let ((limit-number (uos:rlimit-number limit nil))
	     lim)
	 (if limit-number
	     (progn
	       (setf lim (uos:getrlimit limit-number))
	       (if value
		   (progn
		     (cond
		       ((and hard-limit soft-limit)
			(setf (uos:rlimit-maximum lim) (string->value value))
			      (uos:rlimit-current lim) (string->value value))
		       (hard-limit
			(setf (uos:rlimit-maximum lim) (string->value value)))
		       (t
			(setf (uos:rlimit-current lim) (string->value value))))
		     (uos:setrlimit limit-number lim))
		   (progn
		     (setf *output* (result->value lim))
		     (format t "~a~%"
			     (value->string *output*)))))
	     (error "Unknown limit ~s" limit))))
      (t
       (format t "~a~%"
	       (value->string
		(result->value (uos:getrlimit :fsize))))))))

(defbuiltin wait ()
  "Wait for commands to terminate."
  #+unix
  (let (job)
    ;; @@@ should we wait for threads or what?
    (multiple-value-bind (pid result status) (uos::wait)
      (when pid
	(if (setf job (find-if (_ (and (typep _ 'system-job)
				       (eql (job-pid _) pid)))
			       (lish-jobs *shell*)))
	    (handle-job-change job result status)
	    (format t "Unknown job changed ~a~%" pid)))))
  #-unix (values))

(defbuiltin exec ((command-words t :repeating t
                    :help "Words of the command to execute."))
  "Replace the whole Lisp system with another program. This seems like a rather
drastic thing to do to a running Lisp system."
  #+windows (declare (ignore command-words))
  #+windows (error "Wouldn't you prefer a nice game of chess?")
  #+unix
  (when command-words
    (let ((path (command-pathname (first command-words))))
      (format t "path = ~w~%command-words = ~w~%" path command-words)
      (os-unix:exec path command-words))))

;; (define-builtin-arg-type function (arg-symbol)
;;   "A function name."
;;   ()
;;   :convert string
;;   (find-symbol (string-upcase value)))

(define-builtin-arg-type key-sequence (arg-string)
  "A key sequence."
  ())

(defbuiltin bind
    (("print-bindings"		 boolean      :short-arg #\p
      :help "Print key bindings.")
     ("print-readable-bindings"	 boolean      :short-arg #\P
      :help "Print key bindings in a machine readable way.")
     ("query"			 function     :short-arg #\q
      :help "Ask what key invokes a function.")
     ("remove-function-bindings" function     :short-arg #\u
      :help "Remove the binding for FUNCTION.")
     ("remove-key-binding"	 key-sequence :short-arg #\r
      :help "Remove the binding for a KEY-SEQUENCE.")
     ("key-sequence"		 key-sequence
      :help "The key sequence to bind.")
     ("function-name"		 function
      :help "The function to bind the key sequence to."))
  "Manipulate key bindings."
  (when (> (count t (list print-bindings print-readable-bindings query
			  remove-function-bindings remove-key-binding)) 1)
    (error "Mutually exclusive arguments provided."))
  (cond
    (print-bindings
     (keymap:describe-keymap rl:*normal-keymap*))
    (print-readable-bindings
     (keymap:map-keymap
      #'(lambda (key val)
	  (format t "(keymap:define-key rl:*normal-keymap* ~w '~a)~%"
		  key val))
      rl:*normal-keymap*))
    ;; @@@ todo: query remove-function-bindings remove-key-binding
    ((and key-sequence (not function-name))
     (format t "~w: ~(~a~)~%" key-sequence
	     (keymap:key-sequence-binding
	      key-sequence rl:*normal-keymap*)))
    (query
     (if (not function-name)
	 (error "Missing function name.")
	 (keymap:map-keymap
	  #'(lambda (key val)
	      (when (equal val function-name)
		(format t "~w: ~a~%" key val)))
	  rl:*normal-keymap*)))
    ((and key-sequence function-name)
     (keymap:set-key key-sequence function-name rl:*normal-keymap*))))

#|
This is really just for simple things. You should probably use the
Lisp version instead.

This is what I might like to be able to say:
@ defcommand tf ((file filename :optional nil)) (! "file" ($$ "type -p" file))

Actually I think this whole thing is ill advised becuase of syntax mixing
problems. As a command the stuff in parens doesn't parse right, so it's
better just to use Lisp syntax.
|#

#|
(defbuiltin defcommand
    (("name"     string :optional nil)
     ("function" string :optional nil))
    "Defines a command which calls a function."
  (let (;(func-name (command-function-name name))
	(cmd-name (string-downcase name))
	(func-symbol (let ((*read-eval* nil))
		       (read-from-string (string-upcase function))))
	(cmd-symbol (intern (string name))))
    (if (fboundp func-symbol)
	(progn
	  (push cmd-symbol *command-list*)
	  (set-command cmd-name
		       (make-instance 'command
				      :name cmd-name
				      :function func-symbol
				      :arglist '())))
	(format t "~a is not a function" func-symbol))))
|#

#|
(defclass arg-command (arg-choice)
  ()
  (:default-initargs
   :choice-func #'verb-list)
  (:documentation "The name of a lish command."))
(defmethod convert-arg ((arg arg-command) (value string) &optional quoted)
  "Convert a string to a command."
  (get-command value))
|#

;; @@@ should it be here or in commands.lisp???
;; (define-builtin-arg-type command (arg-choice)
;;   "The name of a lish command."
;;   ()
;;   (:default-initargs
;;    :choice-func #'verb-list)
;;   :convert string (get-command value))

;; (defmethod convert-arg ((arg arg-command) (value string) &optional quoted)
;;   "Convert a string to a command."
;;   (get-command value))

(defbuiltin undefcommand
  ((command command :optional t :help "The command to forget.")
   (all-external boolean :short-arg #\e
    :help "True to undefine all external commands."))
  "Undefine a command."
  (cond
    (all-external
     (loop :for c :in *command-list* :do
	(when (typep (get-command c) 'external-command)
	  (undefine-command c))))
    (command
     (typecase command
       ((or string symbol)
	(undefine-command (string-downcase command)))
       (command
	(undefine-command (command-name command)))
       (t
	(error "I don't know how to undefine a command of type ~a."
	       (type-of command)))))))

;; @@@ resolve vs. nos:command-pathname
(defun command-paths (cmd)
  "Return all possible command paths. Don't cache the results."
  (loop :with r = nil
     :for dir :in (command-path-list)
     ;; (split-sequence *path-separator*
     ;; 		     (nos:environment-variable "PATH"))
     :do
     (setf r (when (probe-directory dir)
	       (loop :with full = nil
		  :for f :in (read-directory :dir dir)
		  :when (and (command-test #'equal cmd f)
			     (is-executable
			      ;; (setf full
			      ;; 	    (s+ dir *directory-separator* cmd))
			      (setf full (s+ dir *directory-separator* f))
			      :regular t))
		  :return full)))
     :if r
     :collect r))

#|
(defparameter *command-cache* nil
  "A hashtable which caches the of full names of commands.")

(defun get-command-path (cmd &key already-known)
  "Return the possibly cached command path. If ALREADY-KNOWN is true, only check
for already cached commands, don't bother consulting the file system."
  (when (not *command-cache*)
    (setf *command-cache* (make-hash-table :test #'equal)))
  (let ((result (gethash cmd *command-cache*)))
    (when (and (not result) (not already-known))
      (let ((path (command-pathname cmd)))
	(when path
	  (setf (gethash cmd *command-cache*) path
		result path))))
    result))
|#

(defun get-command-path (cmd &key already-known)
  "Return the possibly cached command path. If ALREADY-KNOWN is true, only check
for already cached commands, don't bother consulting the file system."
  (command-pathname cmd :cached (if already-known :only :default)))

;; Maybe this should be called "cache", and leave a posix compatible hash.
(defbuiltin hash
  ((rehash boolean :short-arg #\r
    :help "Forget about command locations.")
   (packages boolean :short-arg #\p
    :help "Forget about cached loadable packages.")
   (commands t :repeating t
    :help "Command to operate on."))
  "Show or forget remembered full pathnames of commands."
  (labels ((pr-cmd (dir cmd) (format t "~a~%" (path-append dir cmd))))
    (when rehash
      (if commands
	  (loop :for c :in commands :do
	     (opsys:command-pathname-cache-remove c))
	  (progn
	    (opsys:command-pathname-cache-clear)
	    (setf *verb-list* nil))))	; @@@ from complete.lisp
    (when packages
      (clear-loadable-system-cache))
    (let ((table (and opsys:*command-pathname-cache*
		      (opsys::command-cache-table
		       opsys:*command-pathname-cache*))))
      (when (and table (not (or rehash packages)))
	(if commands
	    (loop
	       :with path
	       ;; :and multi = (> (length commands) 1)
	       :for c :in commands :do
	       (setf path (gethash c table))
	       (if path
		   (pr-cmd path c)
		   (if (setf path (command-pathname c))
		       ;; (format t "~:[~;~a ~]~a~%" multi c path)
		       (format t "~a~%" path)
		       (format t "~a was not found~%" c))))
	    (maphash #'(lambda (c p) (pr-cmd p c))
		     table))))))

;; Since this is based on phonetics, we would need phonetic dictionaries to do
;; this right. Also, it's specific to language, so it should be in a language
;; package.
(defun indefinite (str)
  (declare (type string str))
  "Return an approximately appropriate indefinite article for the given ~
string. Sometimes gets it wrong for words startings with 'U', 'O', or 'H'."
  (when (> (length str) 0)
    (let ((c (aref str 0)))
      (if (position c "aeiouAEIOU") "an" "a"))))

(defun describe-command (cmd)
  (let (x)
    (cond
      ((setf x (gethash cmd (lish-aliases *shell*)))
       (when x
	 (format t "~a is aliased to ~a~%" cmd x)))
      ((setf x (gethash cmd (lish-global-aliases *shell*)))
       (when x
	 (format t "~a is a global alias for ~a~%" cmd x)))
      ((setf x (gethash cmd (lish-commands)))
       (when x
	 (format t "~a is the command ~a~%" cmd x)))
      ((setf x (get-command-path cmd))
       (when x
	 (format t "~a is ~a~%" cmd x)))
      ((setf x (read-from-string cmd))
       (when (and (symbolp x) (fboundp x))
	 (format t "~a is the function ~s~%" cmd (symbol-function x)))))))

(defbuiltin type
  ((type-only boolean :short-arg #\t
    :help "Show only the type of the name.")
   (path-only boolean :short-arg #\p
    :help "Show only the path of the name.")
   (all boolean :short-arg #\a
    :help "Show all definitions of the name.")
   (names string :repeating t
    :help "Names to describe."))
  "Describe what kind of command the name is."
  (when names
    (loop :with args = names :and n = nil :and did-one
       :while args :do
       (setf n (car args)
	     did-one nil)
       (cond
	 (path-only
	  (let ((paths (command-paths n)))
	    (when paths
	      (format t "~a~%" (setf *output* (first paths))))))
	 (all
	  (let ((x (gethash n (lish-aliases *shell*))))
	    (when x
	      (format t "~a is aliased to ~a~%" n x)
	      (push x *output*)
	      (setf did-one t)))
	  (let ((x (gethash n (lish-global-aliases *shell*))))
	    (when x
	      (format t "~a is globally aliased to ~s~%" n x)
	      (push x *output*)
	      (setf did-one t)))
	  (let ((x (gethash n (lish-commands))))
	    (when x
	      (format t "~a is the ~:[~;builtin ~]command ~a~%"
		      n (command-built-in-p x) x)
	      (push x *output*)
	      (setf did-one t)))
	  (let ((paths (command-paths n)))
	    (when paths
	      (format t (format nil "~~{~a is ~~a~~%~~}" n)
		      paths)
	      (setf *output* (append *output* paths))
	      (setf did-one t)))
	  (let* ((obj (read-from-string n)))
	    (when (and (symbolp obj) (fboundp obj))
	      (format t "~a is the function ~s~%" n (symbol-function obj))
	      (push (symbol-function obj) *output*)
	      (setf did-one t)))
	  (when (not did-one)
	    (format t "~a is unknown~%" n)))
	 (t
	  (let* ((type (command-type *shell* n))
		 (tt (string-downcase type)))
	    (if type
		(progn
		  (if type-only
		      (format t "~a~%" tt)
		      (describe-command n))
		  (setf *output* type))
	      (format t "~a is unknown~%" n)))))
       (setf args (cdr args))))
  *output*)

(defun edit-opt (name &optional (value nil value-supplied-p))
  (read-from-string
   (rl :prompt (s+ name " := ")
       :string (prin1-to-string
		(if value-supplied-p
		    value
		    (get-option *shell* name))))
   nil nil))

(defbuiltin opt
  ((readable boolean :short-arg #\r
    :help "True to output options that are re-readable by the shell.")
   (edit boolean :short-arg #\e :help "True to edit the option's value.")
   (help boolean :short-arg #\h :help "True to show help for the option.")
   (name  option :help "Option to set.")
   (value object :help "Value to set option to." :use-supplied-flag t))
  "Examine or set shell options. Type 'help options' for descriptions."
  (when (and help edit)
    (error "Please supply only one of --help or --edit."))
  (if name
      (if value-supplied-p
	  (set-option *shell* name (if edit (edit-opt name value) value))
	  (cond
	    (help
	     (let ((o (find-option *shell* name)))
	       (print-columnar-help
		`((,name ,(substitute #\space #\newline (arg-help o)))))))
	    (edit
	     (set-option *shell* name (edit-opt name)))
	    (t
	     (format t "~w~%" (get-option *shell* name)))))
      (cond
	(edit
	 (format t "I don't know how to edit all the options at once yet.~%"))
	(help
	 (print-columnar-help 
	  (loop :for o :in (lish-options *shell*) :collect
	     `(,(arg-name o) ,(substitute #\space #\newline (arg-help o))))))
	(readable
	 (loop :for o :in (lish-options *shell*) :do
	    (format t "opt ~a ~w~%" (arg-name o) (arg-value o))))
	(t
	 (print-properties
	  (loop :for o :in (lish-options *shell*)
	     :collect (cons (arg-name o) (format nil "~s" (arg-value o))))
	  :de-lispify nil :right-justify t)))))
#|
(defbuiltin lpath
  ((command sub-command :default :list :help "What to do with the lpath."
    :commands
    ((show ()
      "Show the Lisp path."
      (format t "~s)
     (append ((directory directory :help "The directory to append."))
      "Append an element to the end of path."
      (setf asdf:*central-registry* (append asdf:*central-registry*
					    (list directory))))
     (prepend ((directory directory :help "The directory to prepend."))
      "Prepend and element to the front of the path."
      (setf asdf:*central-registry*
	    (cons directory asdf:*central-registry*)))
     (set ((value list :help "The value to set the Lisp path to."))
      "Set the Lisp path to VALUE, which should be a list of directories."
      (setf asdf:*central-registry* value))))
   (command-args object :repeating t :help "Arguments to sub-commands."))
  "Manipulate the Lisp path."
  (setf *output* (call-command command command-args)))
|#

(defun loadable-system-list ()
  (dlib-misc:loadable-systems :as-strings t))

(define-builtin-arg-type system-designator (arg-keyword)
  "A system designator, either a keyword or an ASDF/SYSTEM:SYSTEM"
  ()
  :convert string
  (if (and (stringp value) (char= (char value 0) #\:))
      (intern (string-upcase (subseq value 1)) (find-package :keyword))
      value))

(defmethod argument-choices ((arg arg-system-designator))
  (declare (ignore arg))
  (dlib-misc:loadable-systems :as-strings t))

(defun asdf-load (system &key verbose force)
  "Load a system with ASDF."
  (let ((symbol
	 (if (or (stringp system) (keywordp system))
	     system
	     (intern (string-upcase (princ-to-string system)) :keyword))))
    (asdf:oos 'asdf:load-op symbol :verbose verbose :force force)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *default-quality-number* 1
    "The default optimization quality number."))

(defparameter *debug-profile*
  `((speed ,*default-quality-number*)
    (safety 3) (debug 3)
    (space ,*default-quality-number*)
    (compilation-speed ,*default-quality-number*))
  "Optimization settings for debugging.")

(defparameter *speed-profile*
  `((speed 3) (safety 2) (debug 0) (space 2) (compilation-speed 0))
  "Optimization settings for execution speed.")

(defparameter *speed-debug-profile*
  `((speed 3) (safety 0) (debug 3) (space 1) (compilation-speed 0))
  "Optimization settings for execution speed and debugging.")

(defbuiltin l
  ((system system-designator :optional nil
    :help "System designator to load.")
   (no-notes boolean :short-arg #\n
    :help "True to suppress compiler notes on some implementations.")
   (no-warn boolean :short-arg #\w
    :help "True to suppress compiler warnings on some implementations.")
   (force boolean :short-arg #\f :help "Force reloading systems.")
   (force-all boolean :short-arg #\F
    :help "Force reloading all systems, even dependencies.")
   ;; (no-verbose boolean :short-arg #\v
   ;;  :help "Turn off verbosity on some implementations.")
   (debug boolean :short-arg #\d :help "Use the debugging optimzation profile.")
   (speed boolean :short-arg #\s :help "Use the speed optimzation profile.")
   (optimization list :short-arg #\O
    :help "A list of optimize declaration qualities, e.g. '((speed 3) (safety 2))
Note that this option overrides the -d and -s options."))
  "Load a system."
  ;; We could theoretically use asdf:*compile-file-warnings-behaviour*
  ;; but it doesn't look like it applies to notes.
  (labels ((load-that ()
	     (asdf-load system #| :verbose (not no-verbose) |#
			:force (or force (and force-all :all))))
	   (with-opt (qualities-list)
	     (uiop/lisp-build:with-optimization-settings (qualities-list)
	       (load-that)))
	   ;; (with-opt (qualities-list)
	   ;;   (do-opt qualities-list system (or force (and force-all :all))))
	   ;; (do-opt (qualities-list system force)
	   ;;   ;; @@@ I don't know how else to do it?
	   ;;   (print `(locally (declare (optimize ,@qualities-list))
	   ;; 	       (asdf-load ,system :force ,force)))
	   ;;   (eval `(locally (declare (optimize ,@qualities-list))
	   ;; 	      (asdf-load ,system :force ,force))))
	   (load-it ()
	     (cond
	       (optimization
		(with-opt optimization))
	       ((and speed debug)
		(with-opt *speed-debug-profile*))
	       (speed
		(with-opt *speed-profile*))
	       (debug
		(with-opt *debug-profile*))
	       (t
		(load-that)))))
    (if (or no-notes no-warn)
	(handler-bind ((condition
			(lambda (c)
			  #-sbcl (ignore c)
			  #+sbcl
			  (cond
			    ((and no-notes (typep c 'sb-ext::compiler-note))
			     (muffle-warning))
			    ((and no-warn (typep c 'warning))
			     (muffle-warning))
			    (t (signal c))))))
	  (load-it))
	(load-it))))

(defbuiltin load
  ((file pathname :default '(pick-file) :help "A file name to load."))
  "Load a file."
  (load file))

;; This is not very close to being correct.
(defun load-dirs ()
  "Directories where things may get loaded from."
  (remove-if #'keywordp
	     (flatten (concatenate 'list
				   asdf:*central-registry*
				   asdf:*source-registry-parameter*))))

(defbuiltin ldirs
  ((push   boolean :short-arg #\p :help "Push directories on the load path.")
   (add    boolean :short-arg #\a :help "Append directories to the load path.")
   (remove boolean :short-arg #\r :help "Remove directories from the load path.")
   (dir pathname :optional t :repeating t :help "Directories to operate on."))
  "List some directories where things are maybe loaded from. Missing directories
are printed in red on some terminals. If a directory is given without an action
option, it is as if --push was given."
  (labels ((pr (d) (grout-format "~a~%" d))
	   ;; This should probably be some system specific thing in opsys.
	   (fix-dir (d)
	     (let ((result (nos:safe-namestring d)))
	       ;; @@@ special case, we should actually just convert to an
	       ;; absolute path.
	       (when (equal result ".")
		 (setf result (nos:current-directory)))
	       (when (and (plusp (length result))
			  (not (equal (char result (1- (length result)))
				      nos:*directory-separator*)))
		 (setf result (s+ result nos:*directory-separator*)))
	       (unless (nos:absolute-path-p result)
		 (error "Relative paths shouldn't be added."))
	       result))
	   (do-push ()
	     (map nil (_ (pushnew (fix-dir _) asdf:*central-registry*
				  :test #'equal)) dir)))
    (with-grout ()
      (when (> (count t `(,push ,add ,remove)) 1)
	(error "Please specidify only one of push, add, or remove."))
      (when (and (or push add remove) (not dir))
	(error "~:(~a) requires a directory." (or push add remove)))
      (cond
	(push (do-push))
	(add (nconc asdf:*central-registry* (map 'list (_ (fix-dir _)) dir)))
	(remove
	 (map nil (_ (setf asdf:*central-registry*
			   (delete (fix-dir _) asdf:*central-registry*
				   :key #'safe-namestring
				   :test #'equal)))
	      dir))
	(dir (do-push))
	(t ;; No args
	 (loop :for d :in (load-dirs)
	    :do
	    (if (not (nos:file-exists d))
		(progn
		  (grout-set-color :red :black)
		  (pr d)
		  (grout-set-normal))
		(pr d)))
	 (setf *output* asdf:*central-registry*))))))

(defun quicklisp-system-list ()
  (dlib-misc:quickloadable-systems :as-strings t))

(define-builtin-arg-type quicklisp-system-designator (arg-keyword)
  "A system designator for QUICKLOAD, either a keyword or a string."
  ()
  :convert ql-dist:system (values value)
  :convert string
  (if (and (stringp value) (char= (char value 0) #\:))
      (intern (string-upcase (subseq value 1)) (find-package :keyword))
      value))

(defmethod argument-choices ((arg arg-quicklisp-system-designator))
  (declare (ignore arg))
  (dlib-misc:quickloadable-systems :as-strings t))

(defbuiltin ql
  ((system quicklisp-system-designator :optional nil
    :help "System designator to load."))
  "QuickLoad a system."
  (ql:quickload system))

(defmethod documentation ((b autoloaded-command) (doctype (eql 'function)))
  "Return the documentation string for the given shell command."
  (with-output-to-string (str)
    (format str "Auto-loaded from ~a" (command-load-from b))
    (when (documentation (command-function-name (command-name b)) 'function)
      (format str "~%~a" (documentation
			  (command-function-name (command-name b))
			  'function)))))

(define-builtin-arg-type autoload-place (arg-keyword) ;(argument)
  "Where to load a command from either a keyword, a string, or a pathname."
  ()
  ;; :convert ql-dist:system (values value)
  ;; :convert symbol (values value)
  ;; :convert pathname (values value)
  :convert string
  (if quoted
      value
      (if (char= (char value 0) #\:)
	  (intern (string-upcase (subseq value 1)) (find-package :keyword))
	  value)))

(defbuiltin autoload
  ((force boolean :short-arg #\f :help "Replace an existing command.")
   (command-name string :optional nil
    :help "The name of the command to load.")
   (where autoload-place :optional nil
    :help "Where to load a command from. A keyword for an ASDF system, or a
string or pathname designating a file.")
   (docstring string :help "A documentation string."))
  "Set a command to be automatically loaded."
  (let ((old-command (get-command command-name)))
    ;; If there's already a non-autoloaded command loaded, don't overwrite it.
    (when (or (not old-command)
	      (typep old-command 'autoloaded-command)
	      force)
      (let ((cmd (make-instance 'autoloaded-command
				:name command-name
				:load-from where)))
	(pushnew command-name *command-list* :test #'equal)
	(setf (documentation (command-function-name command-name) 'function)
	      docstring)
	(set-command command-name cmd)))))

(defparameter *doc-types* '(compiler-macro function method-combination setf
			    structure type variable)
  "Types of documentation to try for the DOC function.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *doc-doc*
    "DOC tries to find documentation for something. It knows how to find
documentation for a Lisp symbol or object, regardless of what package it's in,
if it has one of the standard documentation types of:
  FUNCTION
  VARIABLE
  STRUCTURE
  TYPE
  SETF
  COMPILER-MACRO
  METHOD-COMBINATION
It also can find the documenation for shell commands."
    "This is the *doc-doc* doc.")

  (defparameter *%doc-doc*
    (s+ *doc-doc*  #\newline
	"%doc is useful directly if you want to find documentation for a value,
or direct the output to somewhere other than *STANDARD-OUTPUT*."))

  (defparameter *doc-doc-doc*
    (s+ *doc-doc* #\newline
	"The DOC macro is convenient from a REPL so you don't have to quote
things.")
    "This is the *doc-doc-doc* doc.")

  (defparameter *doc-doc-command-doc*
    (s+ "Show documentation for something named by THING." #\newline
	*doc-doc* #\newline
	"Useful from the shell. The \"doc\" command also just calls describe if
it can't find anything else.")
    "Be serious now!"))

(defun output-text (s)
  (syntax:format-comment-text (make-instance 'syntax-lisp:lisp-token :object s)
			      (grout-stream *grout*)))

(defun %doc (x &key (stream *standard-output*) (all t))
  #.*%doc-doc*
  (when (stringp x)
    (setf x (make-symbol x)))
  (let ((did-one nil) (did-structure nil)
	(p (ignore-conditions (type-error) (find-package x)))
        (cols (or (and *terminal*
		       (or (ignore-errors
			     (terminal-get-size *terminal*)
			     (terminal-window-columns *terminal*))
			   80))
		  80)))
    (with-grout (*grout* stream)
      (dbugf :poo "1 *grout* = ~s stream = ~s~%" *grout* stream)
      (labels ((maybe-doc (obj type)
		 (without-warning (documentation obj type)))
	       (print-doc (sym pkg doc-type &optional fake-type)
		 (when (maybe-doc sym doc-type)
		   (when did-one
		     (grout-princ #\newline))
		   (if (and (eq doc-type 'function) (maybe-doc sym doc-type))
		       (progn
			 (when (and pkg (not (eq pkg (find-package :cl))))
			   (grout-color :green :default
					(format nil "~a " (package-name pkg)))
			   (grout-color :green :default
					(format nil "~:(~a~):~%"
						(or fake-type doc-type))))
			 (grout-format "~a~%" (function-help sym 0 :width cols)))
		       (progn
			 (grout-color :green :default
				      (format nil "~:(~a~): "
					      (or fake-type doc-type)))
			 (when pkg
			   (grout-color :green :default
					(format nil "~a:" (package-name pkg))))
			 (grout-color :green :default (format nil "~a~%" sym))))
		   (if (eq doc-type :command)
		       (grout-color :white :default
				    (format nil "~a"
					    (maybe-doc sym doc-type)))
		       (progn
			 (output-text (maybe-doc sym doc-type))
			 ;;(grout-princ #\newline)
			 ))
		   (setf did-one t)))
	       (do-docs (sym pkg)
		 (loop :for d :in *doc-types*
		    :do
		    ;; Don't print duplicate type documentation for structures,
		    (when (and (eq d 'structure) (maybe-doc sym d))
		      (setf did-structure t))
		    (when (not (and (eq d 'type) did-structure))
		      (print-doc sym pkg d)))))
	;;(do-docs x nil)
	(when (and (or (not did-one) all)
		   #+sbcl (not (symbolp x)) ; sbcl warns about this
		   (maybe-doc x t))
	  ;;(grout-color :white :default
	  ;;       (format nil "~a~%" (maybe-doc x t)))
	  (when did-one
	    (grout-princ #\newline))
	  (output-text (maybe-doc x t))
	  (setf did-one t))
	;; We have to check packages separately
	(when (and p (maybe-doc p t))
	  (when did-one
	    (grout-princ #\newline))
	  (grout-color :green :default "Package:")
	  (grout-princ #\newline)
	  ;;(grout-color :white :default
	  ;;	       (format nil "~%~a~%" (maybe-doc p t)))
	  (output-text (maybe-doc p t))
	  (setf did-one t))
	;; Check for the symbol name in other packages
	(when (and (symbolp x) (or (not did-one) all))
	  (loop :for s :in (find-all-symbols (symbol-name x))
	     :do
	     ;(when (not (eq (symbol-package s) (find-package :cl)))
	     (do-docs s (symbol-package s))))
	;; Check for the command symbol in other packages
	;; (when (or (not did-one) all)
	;;   (loop :for s :in (find-all-symbols (s+ "!" (symbol-name x)))
	;;      :do
	;;      ;;(do-docs s (symbol-package s))
	;;      (print-doc s (symbol-package s) 'function 'command)))
	(when (or (not did-one) all)
	  (print-doc x nil :command))
	;;(when (not did-one)
	;; (grout-format "Nothing.~%"))
	))
    did-one))

;; This is a macro just so we don't have to quote the argument.
(defmacro doc (x)
  #.*doc-doc-doc*
  (cond
    ((or (listp x)
	 (stringp x))
     `(%doc ,x))
    ((symbolp x)			; Unbound symbol, so:
     `(%doc ',x))			; quote it.
    (t
     `(progn (format t "Nothing.~%") nil))))

(defbuiltin doc (("thing" object :required nil))
  #.*doc-doc-command-doc*
  (if (not thing)
      (format t "~
This shows documentation strings for something.
Try typing \"doc doc\".
")
      (progn
	;;(format t "Thing is a ~s with the value ~s.~%" (type-of thing) thing)
	(when (not (%doc thing))
	  (describe thing)))))

;; This is just a convenient wrapper around defparameter. I'm not really sure
;; this is a "good thing", but it can be useful for interactive programming.
;; Not having to declare variables, was probably first invented for interactive
;; programming in Lisp, but this can still have it's use. Of; course
;; implementations vary on getting a warning.
(defbuiltin var
  ((name string :optional nil :help "Name of the variable.")
   (value object :help "Initial value of the variable." )
   (documentation string :use-supplied-flag t
    :help "Documentation string for the variable."))
  "Define a variable, if you are so inclined."
  (eval `(defparameter ,(intern (string-upcase name))
	   ,value
	   ,@(when documentation-supplied-p (list documentation)))))

;; EOF
