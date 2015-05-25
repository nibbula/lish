;;
;; builtin.lisp - Lish built-in commands.
;;

;; $Revision$

;; Here we define the commands that are built in to Lish.

;; Most of these are really just for compatability with a POSIX shell, so
;; perhaps on another operating system you might not need them.
;; For example we might have a set of commands for an internet appliance
;; like a router.
;; @@@ Perhaps we should make some Windows PowerShell commands.
;; @@@ Perhaps we should be able to load a built-in ‘personality’.

(in-package :lish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command definitions

(defbuiltin cd (("directory" pathname))
  "Usage: cd [directory]
Change the current directory to DIRECTORY."
  (setf (lish-old-pwd *shell*) (nos:current-directory))
  (nos:change-directory (or directory (nos:getenv "HOME"))))

(defbuiltin pwd ()
  "Usage: pwd
Print the current working directory."
  (format t "~a~%" (nos:current-directory)))

(defbuiltin pushd (("directory" pathname))
  "Usage: pushd [dir]
Change the current directory to DIR and push it on the the front of the
directory stack."
  (when (not directory)
    (setf directory (pop (lish-dir-list *shell*))))
  (push (nos:current-directory) (lish-dir-list *shell*))
  (!cd directory))

(defbuiltin popd (("number" number))
  "Usage: popd [n]
Change the current directory to the top of the directory stack and remove it
from stack."
  (declare (ignore number))
  (let ((dir (pop (lish-dir-list *shell*))))
    (!cd dir)
    dir))

(defbuiltin dirs ()
  "Usage: dirs
Show the directory stack."
  (format t "~a~%" (lish-dir-list *shell*)))

(defbuiltin suspend ()
  "Usage: suspend
Suspend the shell."
;  (opsys:kill (opsys:getpid) opsys:sigstop))
  (opsys:kill (opsys:getpid) 17))	; SIGSTOP

(define-builtin-arg-type job-descriptor (arg-integer)
  "A job descriptor."
  ())

(defbuiltin resume (("job-descriptor" job-descriptor :optional t))
  "Resume a suspended job."
  (let (job)
    (cond
      ((or (null (lish-suspended-jobs *shell*))
	   (= (length (lish-suspended-jobs *shell*)) 0))
       (format t "No jobs to resume.~%")
       (return-from !resume (values)))
      ((= (length (lish-suspended-jobs *shell*)) 1)
       (setf job (first (lish-suspended-jobs *shell*))))
      (t
       (setf job (find job-descriptor
		       (lish-suspended-jobs *shell*)
		       :test #'equalp
		       :key #'suspended-job-name))))
    (if (not job)
	(format t "Couldn't find a job matching ~a.~%" job-descriptor)
	(if (suspended-job-resume-function job)
	    (progn
	      (setf (lish-suspended-jobs *shell*)
		    (delete job (lish-suspended-jobs *shell*)))
	      (funcall (suspended-job-resume-function job)))
	    (format t "The job doesn't have a resume function ~a.~%"
		    job-descriptor)))))

(defbuiltin jobs (("long" boolean :short-arg #\l))
  "Lists spawned processes that are active."
  ;; @@@ not working yet for system commands
  (loop :for j :in (lish-suspended-jobs *shell*)
     :do
     (with-slots (id name command-line resume-function) j
       (format t "~3d ~10a ~20a ~:[~;~a ~]~a~%"
	       id "LISP" name long resume-function command-line)))
  (when (find-package :bt)
    (loop :for j :in (ignore-errors (funcall (find-symbol "ALL-THREADS" :bt)))
       :do
       (format t "~3d ~10a ~20a ~:[~;~a ~]~a~%"
	       0 "THREAD"
	       (funcall (find-symbol "THREAD-NAME" :bt) j)
	       long j ""))))

(defbuiltin history
    (("clear"	      boolean  :short-arg #\c)
     ("write"	      boolean  :short-arg #\w)
     ("read"	      boolean  :short-arg #\r)
     ("append"	      boolean  :short-arg #\a)
     ("read-not-read" boolean  :short-arg #\n)
     ("filename"      pathname :short-arg #\f)
     ("show-times"    boolean  :short-arg #\t)
     ("delete"	      integer  :short-arg #\d))
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
     (tiny-rl:history-clear :lish))
    ;; @@@ TODO: finish this when history saving in tiny-rl is done.
    (t
     (tiny-rl:show-history :lish))))

(defbuiltin #:|:| (("args" t :repeating t))
  "Usage: : [args]
Arguments are evaluated for side effects."
  (declare (ignore args))
  (values))

(defbuiltin echo
    (("no-newline" boolean :short-arg #\n)
     ("args" t :repeating t))
  "Usage: echo [-n] ...
Output the arguments. If -n is given, then don't output a newline a the end."
  (format t "~{~a~#[~:; ~]~}" args)
  (when (not no-newline)
    (format t "~%")))

(defparameter *help-subjects*
  '("commands" "builtins" "editor" "keys")
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

(defclass arg-help-subject (arg-choice)
  ()
  (:default-initargs
   :choice-func
      #+clisp 'help-choices		; I'm not sure why.
      #-clisp #'help-choices)
  (:documentation "Something which we can get help on."))

(defvar *basic-help*
"~
Lish version ~a help:
  command [arg*...]   Run a program in your path with the given ARGs.
  ([expressions]...)  Evaluate Lisp expressions.
  help [subject]      Show help on the subject.
  exit                Exit the shell.
Subjects:
  help commands       Show help on built-in commands.
  help editor         Show help on the line editor.
  help keys           Show help on key bindings.
  help <command>      Show help for the command.
")

(defvar *editor-help*
"You can use some Emacs-like commands to edit the command line.

Some notable keys are:
 <Tab>        Try to complete the word in front of the cursor.
 ?            Show what input is expected. List possibilities.
 <Control-D>  Quit, when on an empty line, or delete the following character.
 <Control-P>  Previous history line. Also the <Up Arrow> key.
 <Control-N>  Next history line. Also the <Down Arrow> key.
 <Control-B>  Move the cursor back one character. Also the <Left Arrow> key.
 <Control-F>  Move the cursor forward one character. Also the <Right Arrow> key.
 <Control-Q>  Quote next character, like if you want to really type '?'.
 <F9>         Switch back and forth between LISH and the lisp REPL.
")

(defbuiltin help (("subject" help-subject))
  "help [subject]         Show help on the subject.
Without a subject show some subjects that are available."
  (if (not subject)
      (progn
	(format t *basic-help* *version*))
      ;; topics
      (cond
	((or (equalp subject "commands") (equalp subject "builtins"))
;	 (format t "  ~c[4mName~14t~c[0m  ~c[4mSynopsis~80t~c[0m~%"
;		 #\escape #\escape #\escape #\escape)
	 (let ((commands
		(sort
		 (loop :for k :being :the :hash-keys :of (lish-commands)
		    :collect k)
		 #'string-lessp)))
	   (format t "Built-in commands:~%")
	   (loop :for k :in commands :do
	      (let ((b (get-command k)))
		  (when (and b (command-built-in-p b))
		    (format t "  ~a~%" (posix-synopsis b)))))
	   (format t "Added commands:~%")
	   (loop :for k :in commands :do
	      (let ((b (get-command k)))
		  (when (and b (not (command-built-in-p b)))
		    (format t "  ~a~%" (posix-synopsis b)))))))
	((or (equalp subject "editor"))
	 (format t *editor-help*))
	((or (equalp subject "keys"))
	 (format t "Here are the keys active in the editor:~%")
	 (!bind :print-bindings t))
	(t ;; Try a specific command
	 (let* ((cmd  (get-command subject))
		(symb (intern (string-upcase subject) :lish))
		(doc  (when cmd (documentation cmd 'function)))
		(fdoc (when (fboundp symb)
			(documentation (symbol-function symb) 'function))))
;	   (print-values* (subject cmd symb doc fdoc))
	   (cond
	     (doc  (format t "~a~%" doc))
	     (fdoc (format t "Lisp function:~%~a~%" fdoc))
	     (cmd  (format t "Sorry, there's no help for \"~a\".~%" subject))
	     (t    (format t "I don't know about the subject \"~a\"~%"
			   subject))))))))

(defmethod documentation ((b command) (doctype (eql 'function)))
  "Return the documentation string for the given shell command."
  (with-output-to-string (str)
    (format str "~a" (posix-synopsis b))
    (when (documentation (command-function b) 'function)
      (format str "~%~a" (documentation (command-function b) 'function)))
#|    (when (command-loaded-from b)
      (format str "~%Loaded from ~a" (command-loaded-from b))) |#
    ))

(defun set-alias (sh name expansion)
  "Define NAME to be an alias for EXPANSION.
NAME is replaced by EXPANSION before any other evaluation."
  (setf (gethash name (lish-aliases sh)) expansion))

(defun unset-alias (sh name)
  "Remove the definition of NAME as an alias."
  (remhash name (lish-aliases sh)))

(defun get-alias (sh name)
  (gethash name (lish-aliases sh)))

(defbuiltin alias
    (("name" string)
     ("expansion" string))
  "Define NAME to expand to EXPANSION when starting a line."
  (if (not name)
      (loop :for a :being :the :hash-keys :of (lish-aliases *shell*)
	    :do
	    (format t "alias ~a ~:[is not defined~;~:*~w~]~%"
		    a (get-alias *shell* a)))
      (if (not expansion)
	  (format t "alias ~a ~:[is not defined~;~:*~w~]~%"
		  name (get-alias *shell* name))
	  (set-alias *shell* name expansion))))

(defbuiltin unalias (("name" string :optional nil))
  "Remove the definition of NAME as an alias."
  (unset-alias *shell* name))

(defbuiltin exit (("values" string :repeating t))
  "Exit from the shell. Optionally return values."
  (when values
    (setf (lish-exit-values *shell*) (loop :for v :in values :collect v)))
  (setf (lish-exit-flag *shell*) t))

(defbuiltin source (("filename" pathname :optional nil))
  "Evalute lish commands in the given file."
  (without-warning (load-file *shell* filename)))

;; This is so if it's not provided, it can toggle.
(defclass arg-boolean-toggle (arg-boolean)
  ()
  (:default-initargs
   :default :toggle)
  (:documentation "A true or false value, that can be toggled."))
;; (defmethod convert-arg ((arg arg-boolean-toggle) (value string))
;;   (cond
;;     ((position value +true-strings+ :test #'equalp) t)
;;     ((position value +false-strings+ :test #'equalp) nil)
;;     (t (error "Can't convert ~w to a boolean." value))))

;(defvar howji (macroexpand-

(defbuiltin debug (("state" boolean-toggle))
  "Toggle shell debugging."
  (setf (lish-debug *shell*)
	(if (eql state :toggle)
	    (not (lish-debug *shell*))
	    state))
  (format t "Debugging is ~:[OFF~;ON~].~%" (lish-debug *shell*)))

#|
;; Just use the version from dlib-misc	;
;; @@@ Or maybe the version from there should live here, since it's shellish?? ;
  (defun printenv (&optional original-order) ; copied from dlib-misc ;
"Like the unix command."
(let ((mv (reduce #'max (nos:environ)
:key #'(lambda (x) (length (symbol-name (car x))))))
(sorted-list (if original-order
(nos:environ)
(sort (nos:environ) #'string-lessp
:key #'(lambda (x) (symbol-name (car x)))))))
(loop :for v :in sorted-list
:do (format t "~va ~30a~%" mv (car v) (cdr v)))))
  |#

(defbuiltin export (("name" string) ("value" string))
  "Set environment variable NAME to be VALUE. Omitting VALUE, just makes sure
the current value of NAME is exported. Omitting both, prints all the exported
environment variables. If NAME and VALUE are converted to strings if necessary."
  (when (and name (not (stringp name)))
    (setf name (princ-to-string name)))
  (when (and value (not (stringp value)))
    (setf value (princ-to-string value)))
  (if name
      (if value
	  (nos:setenv name value)
	  (nos:getenv name))		; Actually does nothing
      (printenv)))

(defbuiltin env (("ignore-environment" boolean :short-arg #\i)
		 ("variable-assignment" string :repeating t)
		 ("shell-command" shell-command)
		 ("arguments" object :repeating t))
  "Modify the command environment. If ignore-environment"
  (if (and (not shell-command) (not arguments))
      ;; Just print variables
      (loop :for v :in variable-assignment
	 :do
	 (let ((var (if (position #\= v)
			(first (split-sequence #\= v))
			v)))
	   (when var
	     (format t "~a=~a~%" var (nos:getenv var)))))
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
	       (nos:setenv var val))))
	(apply #'do-system-command
	       `(,`(,shell-command ,@arguments)
		   ,@(if ignore-environment '(nil nil nil)))))))

(defun get-cols ()
  (let ((tty (tiny-rl::line-editor-terminal (lish::lish-editor *shell*))))
    (ansiterm:terminal-get-size tty)
    (ansiterm:terminal-window-columns tty)))

(defparameter *signal-names* (make-array
			      (list nos:*signal-count*)
			      :initial-contents
			      (cons ""
			        (loop :for i :from 1 :below nos:*signal-count*
				   :collect (nos:signal-name i))))
  "Names of the signals.")

(define-builtin-arg-type signal (arg-integer)
  "A system signal."
  ()
  :convert string
    (or (position value *signal-names* :test #'equalp)
	(parse-integer value)))

(defbuiltin kill
    (("list-signals" boolean :short-arg #\l)
     ("signal" 	     signal  :default   15)
     ("pids" 	     integer :repeating t))
  ;; @@@ pid should be job # type to support %job
  "Sends SIGNAL to PID."
  ;; @@@ totally faked & not working
  (if list-signals
      (format t (s+ "~{~<~%~1," (get-cols) ":;~a~> ~}~%") ; bogus, but v fails
	      (loop :for i :from 1 :below nos:*signal-count*
		 :collect (format nil "~2d) ~:@(~8a~)" i (nos:signal-name i))))
      (when pids
	(mapcar #'(lambda (x) (nos:kill signal x)) pids))))

;; Actually I think that "format" and "read" are a bad idea / useless, because
;; they're for shell scripting which you should do in Lisp.

;;; make printf an alias
(defbuiltin format
    (("format-string" string :optional nil)
     ("args" t :repeating t))
  "Formatted output."
  ;; @@@ totally faked & not working
  (apply #'format t format-string args))

;; Since this is for scripting in other shells, I think we don't need to worry
;; about it, since the user can just call READ-LINE-like functions directly.
(defbuiltin read
    (("name"    string)
     ("prompt"  string  :short-arg #\p)
     ("timeout" integer :short-arg #\t)
     ("editing" boolean :short-arg #\e))
  "Read a line of input."
  ;; @@@ totally faked & not working
  (declare (ignore timeout name))
  (if editing (tiny-rl:tiny-rl :prompt prompt)
      (read-line nil nil)))

(defbuiltin time (("command" string :repeating t))
  "Usage: time command ...
Shows some time statistics resulting from the execution of COMMNAD."
  (time (shell-eval *shell* (make-shell-expr :words command))))

(defun print-timeval (tv &optional (stream t))
  (let* ((secs  (+ (timeval-seconds tv)
		   (/ (timeval-micro-seconds tv) 1000000)))
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
  "Usage: times
Show accumulated times for the shell."
  (let ((self (getrusage :SELF))
	(children (getrusage :CHILDREN)))
    (format t "Self     User: ~a~32tSys: ~a~%"
	    (print-timeval (rusage-user self) nil)
	    (print-timeval (rusage-system self) nil))
    (format t "Children User: ~a~32tSys: ~a~%"
	    (print-timeval (rusage-user children) nil)
	    (print-timeval (rusage-system children) nil))))

(defbuiltin umask
    (("print-command" boolean :short-arg #\p)
     ("symbolic"      boolean :short-arg #\S)
     ("mask"	     string))
  "Set or print the default file creation mode mask (a.k.a. permission mask).
If mode is not given, print the current mode. If PRINT-COMMAND is true, print
the mode as a command that can be executed. If SYMBOLIC is true, output in
symbolic format, otherwise output in octal."
  (declare (ignore symbolic)) ;; @@@
  (if (not mask)
      ;; printing
      (let ((current-mask (nos:umask 0)))
	(nos:umask current-mask)
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
	  (nos:umask real-mask)))))

(defbuiltin ulimit ())
(defbuiltin wait ())

(defbuiltin exec (("command-words" t :repeating t))
  "Replace the whole Lisp system with another program. This seems like a rather
drastic thing to do to a running Lisp system. Wouldn't you prefer a nice game
of chess?"
  (when command-words
    (let ((path (command-pathname (first command-words))))
      (format t "path = ~w~%command-words = ~w~%" path command-words)
      (nos:exec path command-words))))

(define-builtin-arg-type function (arg-symbol)
  "A function name."
  ()
  :convert string (find-symbol (string-upcase value)))

(define-builtin-arg-type key-sequence (arg-string)
  "A key sequence."
  ())

(defbuiltin bind
    (("print-bindings"		 boolean      :short-arg #\p)
     ("print-readable-bindings"	 boolean      :short-arg #\P)
     ("query"			 function     :short-arg #\q)
     ("remove-function-bindings" function     :short-arg #\u)
     ("remove-key-binding"	 key-sequence :short-arg #\r)
     ("key-sequence"		 key-sequence)
     ("function-name"		 function))
  "Manipulate key bindings."
  (when (> (count t (list print-bindings print-readable-bindings query
			  remove-function-bindings remove-key-binding)) 1)
    (error "Mutually exclusive arguments provided."))
  (cond
    (print-bindings
     (keymap:dump-keymap tiny-rl:*normal-keymap*))
    (print-readable-bindings
     (keymap:map-keymap
      #'(lambda (key val)
	  (format t "(keymap:define-key tiny-rl:*normal-keymap* ~w '~a)~%"
		  key val))
      tiny-rl:*normal-keymap*))
    ;; @@@ todo: query remove-function-bindings remove-key-binding
    ((and key-sequence (not function-name))
     (format t "~w: ~(~a~)~%" key-sequence
	     (keymap:key-sequence-binding
	      key-sequence tiny-rl:*normal-keymap*)))
    (query
     (if (not function-name)
	 (error "Missing function name.")
	 (keymap:map-keymap
	  #'(lambda (key val)
	      (when (equal val function-name)
		(format t "~w: ~a~%" key val)))
	  tiny-rl:*normal-keymap*)))
    ((and key-sequence function-name)
     (keymap:set-key key-sequence function-name tiny-rl:*normal-keymap*))))


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
(defmethod convert-arg ((arg arg-command) (value string))
  "Convert a string to a command."
  (get-command value))
|#

(defbuiltin undefcommand (("command" command))
  "Undefine a command."
  (undefine-command (string-downcase command)))

(defun is-executable (s)
  (logand (file-status-mode s) S_IXUSR))

(defun is-regular (s)
  (logand (file-status-mode s) S_IXUSR))

(defun is-regular-executable (p)
  (let ((st (stat p)))
    (and st (is-executable st) (is-regular st))))

(defun has-directory-p (p)
  (position *directory-separator* p))

(defun command-pathname (cmd)
  "Return the full pathname of the first executable file in the PATH or nil
if there isn't one."
  (when (has-directory-p cmd)
    (return-from command-pathname cmd))
  (loop :for dir :in (split-sequence *path-separator* (getenv "PATH")) :do
	(when (probe-directory dir)
	  (loop :with full = nil
		:for f :in (read-directory :dir dir) :do
		(when (and (equal f cmd)
			   (is-regular-executable
			    (setf full
				  (format nil "~a~c~a"
					  dir *directory-separator* cmd))))
		  (return-from command-pathname full)))))
  nil)

(defun command-paths (cmd)
  "Return all possible command paths. Don't cache the results."
  (loop :with r = nil
    :for dir :in (split-sequence *path-separator* (getenv "PATH"))
    :do
    (setf r (when (probe-directory dir)
	      (loop :with full = nil
		    :for f :in (read-directory :dir dir)
		    :when (and (equal f cmd)
			       (is-regular-executable
				(setf full
				      (format nil "~a~c~a"
					      dir *directory-separator* cmd))))
		    :return full)))
    :if r
    :collect r))

(defparameter *command-cache* nil
  "A hashtable which caches the of full names of commands.")

(defun get-command-path (cmd)
  "Return the possibly cached command path."
  (when (not *command-cache*)
    (setf *command-cache* (make-hash-table :test #'equal)))
  (let ((result (gethash cmd *command-cache*)))
    (when (not result)
      (let ((path (command-pathname cmd)))
	(when path
	  (setf (gethash cmd *command-cache*) path
		result path))))
    result))

(defbuiltin hash
    (("rehash" boolean :short-arg #\r)
     ("commands" t :repeating t))
  "Usage: hash [-r] [commands...]
Show remembered full pathnames of commands. If -r is given, forget them all."
  (labels ((pr-cmd (c) (format t "~a~%" c)))
    (if rehash
	(if commands
	    (loop :for c :in commands :do
	       (remhash c *command-cache*))
	    (setf *command-cache* nil))
	(when *command-cache*
	  (if commands
	      (loop :for c :in commands :do
		 (pr-cmd (gethash c *command-cache*)))
	      (maphash #'(lambda (c p) (declare (ignore c)) (pr-cmd p))
		       *command-cache*))))))

;; Since this is based on phonetics, we would need phonetic dictionaries to do
;; this right.
(defun indefinite (str)
  (declare (type string str))
  "Return an approximately appropriate indefinite article for the given ~
string. Sometimes gets it wrong for words startings with 'U', 'O', or 'H'."
  (when (> (length str) 0)
    (let ((c (aref str 0)))
      (if (position c "aeiouAEIOU") "an" "a"))))

(defun command-type (sh command)
  "Return a string representing the command type of command."
  (cond
    ((gethash command (lish-commands))   "command")
    ((gethash command (lish-aliases sh)) "alias")
    ((get-command-path command)          "file")
    (t "")))

(defun describe-command (cmd)
  (let (x)
    (cond
      ((setf x (gethash cmd (lish-aliases *shell*)))
       (when x
	 (format t "~a is aliased to ~a~%" cmd x)))
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
    (("type-only" boolean :short-arg #\t)
     ("path-only" boolean :short-arg #\p)
     ("all" 	  boolean :short-arg #\a)
     ("names" 	  string  :repeating t))
  "Describe what kind of command the name is."
  (when names
    (loop :with args = names :and n = nil
       :while args :do
       (setf n (car args))
       (cond
	 (path-only
	  (let ((paths (command-paths n)))
	    (when paths
	      (format t "~a~%" (first paths)))))
	 (all
	  (let ((x (gethash n (lish-aliases *shell*))))
	    (when x
	      (format t "~a is aliased to ~a~%" n x)))
	  (let ((x (gethash n (lish-commands))))
	    (when x
	      (format t "~a is the command ~a~%" n x)))
	  (let ((paths (command-paths n)))
	    (when paths
	      (format t (format nil "~~{~a is ~~a~~%~~}" n)
		      paths)))
	  (let* ((obj (read-from-string n)))
	    (when (and (symbolp obj) (fboundp obj))
	      (format t "~a is the function ~s~%" n (symbol-function obj)))))
	 (t
	  (let ((tt (command-type *shell* n)))
	    (when tt
	      (if type-only
		  (format t "~a~%" tt)
		  (describe-command n))))))
	 (setf args (cdr args)))))

(defbuiltin stats (("command" choice :choices ("save" "show")))
  "Show command statistics."
  (cond
    ((equal command "save")
     (format t "Stats saved in ~a.~%" (save-command-stats)))
    ((equal command "show")
     (show-command-stats))
    (t
     (show-command-stats))))

;; EOF
