;;
;; lish.lisp - Unix Shell & Lisp somehow smushed together
;;

;; $Revision: 1.15 $

(in-package :lish)

;(declaim (optimize (debug 3)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
;(declaim (optimize (speed 3) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

(defclass shell ()
  ((debug
    :initarg :debug
    :accessor lish-debug
    :documentation "True to enter the debugger on errors in lish.")
   (exit-flag
    :initarg :exit-flag
    :accessor lish-exit-flag
    :documentation "Set to true to exit the shell.")
   (exit-values
    :initarg :exit-values
    :accessor lish-exit-values
    :documentation "List of values to return to the caller.")
   (sub-prompt
    :initarg :sub-prompt
    :accessor lish-sub-prompt
    :documentation "Prompt for continuation lines.")
   (prompt
    :initarg :prompt-char
    :accessor lish-prompt-char
    :documentation "Normal prompt character.")
   (prompt-function
    :initarg :prompt-function
    :accessor lish-prompt-function
    :documentation "Function returning the prompt string.")
   (aliases
    :accessor lish-aliases
    :documentation "Hash table of aliases.")
   (editor
    :accessor lish-editor
    :documentation "Line editor instance.")
   (old-pwd
    :accessor lish-old-pwd
    :initform nil
    :documentation "The last working directory.")
   (dir-list
    :accessor lish-dir-list
    :initform nil
    :documentation "Directory list for pushd and popd.")
   (suspended-jobs
    :accessor lish-suspended-jobs :initarg :suspended-jobs :initform nil
    :documentation "List of suspended jobs.")
   )
  (:documentation "A lispy system command shell.")
  (:default-initargs
   :prompt-char #\@
   :prompt-function #'make-prompt
   :sub-prompt "- "	; @@@ maybe we need sub-prompt-char & sub-prompt-func?
   :debug nil
   :exit-flag nil
   :exit-values '()
  ))

(defmethod initialize-instance :after ((sh shell) &rest initargs)
  (declare (ignore initargs))
;  (setf (slot-value sh 'commands) (make-hash-table :test #'equal))
  (setf (slot-value sh 'aliases) (make-hash-table :test #'equal))
  (init-commands))

(defvar *shell-path* '()
  "List of directories to autoload commands from.")

(defvar *lish-user-package*
  (make-package "LISH-USER" :use '(:cl :lish :cl-ppcre :glob)
		:nicknames '("LU"))
  "Package for lish to hang out in. Auto-updates from :cl-user.")

;; The "result" argument is not for the caller, but rather so we can detect
;; cycles in the package inheritance graph.
(defun flattened-package-use-list (package &optional result)
  (loop :for p :in (package-use-list package) :do
     (when (not (position p result))
       (push p result)
       (loop :for ip :in (flattened-package-use-list p result) :do
	  (pushnew ip result))))
  result)

;; This tries to keep :lish-user up to date with respect to :cl-user.
(defun update-user-package ()
  ;; Update uses
  (loop :with isym :and isymbol-type :and esym :and esymbol-type
     :for p :in (package-use-list :cl-user) :do
     (when (not (position p (flattened-package-use-list *lish-user-package*)))
       (dbug "Package ~w~%" p)
       ;; Things directly in lish-user are uninterned in favor of one
       ;; in cl-user.
       (unintern-conflicts *lish-user-package* p)
       ;; Conflicts in inherited symbols are resolved by having the "explicitly"
       ;; used package symbol (i.e. things used by :lish-user such as :lish)
       ;; interned and made shadowing.
       (do-symbols (sym p)
	 (setf (values esym esymbol-type)
	       (find-symbol (symbol-name sym) p)
	       (values isym isymbol-type)
	       (find-symbol (symbol-name sym) *lish-user-package*))
	 (when (not (equal esym isym))
	   (case isymbol-type
	     ((:internal :external)
	      (dbug "CONFLICT ~w ~w ~w~%" p (symbol-name sym) isymbol-type)
	      (shadow isym *lish-user-package*))
	     (:inherited
	      (when (not (eq (symbol-package esym) (symbol-package isym)))
		(dbug "CONFLICT ~w ~w ~w~%" p (symbol-name sym) isymbol-type)
		(shadowing-import isym *lish-user-package*))))))
       (use-package p *lish-user-package*)))
  ;; Update all symbols
  (do-symbols (sym :cl-user)
    ;; @@@ deal with conflicts between imported symbols from different packages
    ;; @@@ keep symbols from packages used directly by :lish-user
    (when (not (find-symbol (symbol-name sym) *lish-user-package*))
      (import sym *lish-user-package*)))
  ;; Export exported symbols
  (do-external-symbols (sym :cl-user)
    (export sym *lish-user-package*)))

(defvar *lish-level* nil
  "Number indicating the depth of lish recursion. Corresponds to the ~
LISH_LEVEL environment variable.")
;(declaim (special *lish-level*))

(defun fixed-homedir ()
  "(user-homedir-pathname) with a trailing slash removed if there was one."
  (let ((h (namestring (user-homedir-pathname))))
    (if (equal #\/ (aref h (1- (length h))))
	(subseq h 0 (1- (length h)))
	h)))

(defun twiddlify (name)
  "Turn (user-homedir-pathname) occuring in name into a tilde."
  (replace-subseq (namestring (fixed-homedir)) "~"
		  (namestring name) :count 1))

;; I personally favor the prompt function, so this is basically a ploy to get
;; people that just want their bash prompt to work, to use my shell.

(defun format-prompt (sh prompt &optional (escape-char #\%))
  "Return the prompt string with bash-like formatting character replacements.
So far we support:
%%	A percent.
%a	#\bell
%e	#\escape
%n	#\newline
%r	#\return
%NNN	The character whose ASCII code is the octal value NNN.
%s	The name of the shell, which is usually “Lish”.
%v	Shell version.
%V	Even more shell version.
%u	User name.
%h	Host name truncated at the first dot.
%H	Host name.
%w	Working directory, tildified.
%W	The basename of `$PWD', tildified.
%$	If the effective UID is 0, `#', otherwise `$'.
%d      <3 char weekday> <3 char month name> <date>.
%t	24 hour HH:MM:SS
%T	12 hour HH:MM:SS
%@	The time, in 12-hour am/pm format.
%A	The time, in 24-hour HH:MM format.
Not implemented yet:
%!	The history number of this command.
%#	The command number of this command.
%[	Start of non-printing characters.
%]	End of non-printing characters.
%l	The basename of the shell's terminal device name.
%D{FORMAT}
	Some date formated by Unix strftime. Without the FORMAT just put some
	locale-specific date.
%j	The number of jobs currently managed by the shell.
"
  (declare (ignore sh))
  (with-output-to-string (str)
    (loop :with c :for i :from 0 :below (length prompt) :do
       (setf c (aref prompt i))
       (if (equal c escape-char)
         (progn
	   (incf i)
	   (when (< i (length prompt))
	     (setf c (aref prompt i))
	     (case c
	       (#\% (write-char escape-char str))
	       (#\a (write-char #\bell str))
	       (#\e (write-char #\escape str))
	       (#\n (write-char #\newline str))
	       (#\r (write-char #\return str))
	       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
		(write-char (code-char
			     (parse-integer (subseq prompt i (+ i 3)) :radix 8))
			    str))
	       (#\s (write-string *shell-name* str))
	       (#\v (write-string *major-version* str))
	       (#\V (write-string *version* str))
	       (#\u (write-string (nos:getlogin) str))
	       (#\h (write-string dlib:*host* str))
	       (#\H (write-string (machine-instance) str))
	       (#\w (write-string (twiddlify (nos:current-directory)) str))
	       (#\W (write-string
		     (twiddlify (basename (nos:current-directory))) str))
	       (#\$ (write-char (if (= (nos:geteuid) 0) #\# #\$) str))
	       (#\d (write-string
		     (format-date "~3a ~3a ~2d"
				  (:day-abbrev :month-abbrev :date)) str))
	       (#\t (write-string
		     (format-date "~2,'0d:~2,'0d:~2,'0d"
				  (:hours :minutes :seconds)) str))
	       (#\T (write-string
		     (format-date "~2,'0d:~2,'0d:~2,'0d"
				  (:12-hours :minutes :seconds)) str))
	       (#\@ (write-string
		     (format-date "~2,'0d:~2,'0d ~2a"
				  (:12-hours :minutes :am)) str))
	       (#\A (write-string
		     (format-date "~2,'0d:~2,'0d" (:hours :minutes)) str))
	       )))
	 (write-char c str)))))

(defgeneric make-prompt (shell)
  (:documentation "Return a string to prompt with."))
(defmethod make-prompt ((sh shell))
  "Return a string to prompt with."
  (format nil "~a " (make-string (+ 1 *lish-level*)
				 :initial-element (lish-prompt-char sh))))

(defparameter *real-eof-symbol* :Z-REAL-EOF)
(defparameter *continue-symbol* :Z-CONTINUE)
(defparameter *empty-symbol* :Z-EMPTY)
(defparameter *error-symbol* :Z-ERROR)
(defparameter *quit-symbol* :Z-QUIT)

(defstruct shell-expr
  "The result of the shell lexer. A sequence of words and their start and ~
end points in the original string."
  words
  word-start
  word-end
  word-quoted
  line)

(defstruct lisp-expression
  "Nothing fancy. Just a wrapper for a lisp value for now."
  object)

(defun in-shell-word (exp word-num position)
  (declare (type shell-expr exp)
	   (type number word-num position))
  "Return true if the POSITION is in the shell word numbered WORD-NUM."
  (and (>= position (elt (shell-expr-word-start exp) word-num))
       (<= position (elt (shell-expr-word-end   exp) word-num))))

(defun shell-word-number (exp pos
			  &key (exp-len (length (shell-expr-words exp))))
  (declare (type shell-expr exp))
  "Return the shell expression's word number that position POS is in."
;  (with-slots (word-start word-end) exp
  (loop :for w :from 0 :below exp-len
#|
  :when (and (>= pos (elt (shell-expr-word-start exp) w))
     (<= pos (elt (shell-expr-word-end exp) w))) 
  |#
     :when (in-shell-word exp w pos)
     :return w))

(defun read-string (s)
  "Read a lish string. It has similar syntax to a lisp string. ~
Assumes the opening double quote has already been read. ~
Read until a double quote. Backslash escapes the special meaning of ~
the following character. Return the string and how long it is. If we got to ~
the end and didn't get a close quote the third value is true.~
"
  (let ((v (make-stretchy-string 10))
	(i 0)
	(end-quote nil)
	(do-quote nil))
    (loop :for c :across s :do
       (setf end-quote (and (eql c #\") (not do-quote)))
       :while (not end-quote)
       :do
       (if (and (eql c #\\) (not do-quote))
	   (setf do-quote t)
	   (progn
	     (setf do-quote nil)
	     (vector-push-extend c v)))
       (incf i))
    (values v i (not end-quote))))

;; I'm not so old fashioned that I think ^L should be in here, but are there
;; any other unicode things that should?
(defparameter *whitespace* #(#\space #\newline #\tab #\return)
  "Word separators for lish.")

(defun contains-whitespace-p (s)
  (position-if #'(lambda (x) (position x *whitespace*)) s))

;; Previously this was a method so you could make a speicalized one for
;; different shell reader syntax, but since it doesn't use anything in the
;; shell object and you might want to call it without having that, I made
;; it into a function. If we really need to, we can make a wrapper method.

(defun shell-read (line &key partial (package *lish-user-package*))
  "Read objects in shell syntax and return them. If PARTIAL is true, don't 
signal an error if we can't read a full expression.
The syntax is vaguely like:
  ; comment
  command [arg...]
  command \"string\" !*lisp-object* (lisp-code)
  command word\ with\ spaces \"string \\\" with a double quote\"
  command | command | ...
  command < file-name
  command > file-name
  ([lisp expressions...])"
  (let (word-start word-end word-quoted words
	(c nil)				; current char
	(i 0)				; index in line
	(len (length line))
	(args '())
	(w (make-stretchy-string 12))	; temp word
	(in-word nil)			; t if in word
	(do-quote nil)			;
	(did-quote nil))		;
    (labels ((finish-word ()
	       "Finish the current word."
	       (when in-word
		 (push (copy-seq w) args)
		 (push i word-end)
		 (push did-quote word-quoted)
		 (setf (fill-pointer w) 0
		       in-word nil
		       did-quote nil)))
	     (do-continue ()
	       "Handle when the expression is incomplete."
	       (if partial
		   (progn
		     (push i word-start)
		     (push (subseq line i) args)
		     (push (length line) word-end)
		     (push nil word-quoted)
		     (return-from shell-read
		       (make-shell-expr
			:line line
			:words (nreverse args)
			:word-start (reverse word-start)
			:word-end (nreverse word-end) 
			:word-end (nreverse word-quoted))))
		   (return-from shell-read *continue-symbol*)))
	     (reverse-things ()
	       "Reverse the things we've been consing, so they're in order."
	       (setf word-start  (reverse word-start)
		     word-end    (nreverse word-end)
		     word-quoted (nreverse word-quoted)
		     words       (nreverse args)))
	     (make-the-expr ()
	       "Make an expression, with it's own copy the lists."
	       (make-shell-expr
		:line line
		:words (copy-seq words)
		:word-start (copy-seq word-start)
		:word-end (copy-seq word-end)
		:word-quoted (copy-seq word-quoted))))
      (loop
	 :named tralfaz
	 :while (< i len)
	 :do
	 (setf c (aref line i))
	 (cond
	   ;; quoted char
	   (do-quote
	       (vector-push-extend c w)
	     (when (not in-word)
	       (push (1- i) word-start))
	     (setf in-word t)
	     (setf do-quote nil)
	     (setf did-quote t)
	     (incf i))
	   ;; a string
	   ((eql c #\")
	    (finish-word)
	    ;; read a string as a separate word
	    (multiple-value-bind (str ink cont)
		(read-string (subseq line (1+ i)))
	      (when (and cont (not partial))
		(return-from shell-read *continue-symbol*))
	      (push i word-start)
	      (push str args)
	      (incf i (+ 2 ink))
	      (push i word-end)
	      (push t word-quoted)))
	   ;; a lisp function application
	   ((eql c #\()
	    (finish-word)
	    (handler-case
		;; read a form as a separate word
		(multiple-value-bind (obj pos)
		    (with-package package
		      (read-from-string line nil *continue-symbol* :start i))
		  (push i word-start)
		  (setf i pos)
		  (push obj args)
		  (push i word-end)
		  (push nil word-quoted))
;;	      (end-of-file ()
	      (error () (do-continue))
	      (condition (c) (signal c))))
	   ;; a lisp expr
	   ((eql c #\!)
	    (finish-word)
	    ;; read a form as a separate word
	    (handler-case
		(multiple-value-bind (obj pos)
		    (with-package package
		      (read-from-string line nil *continue-symbol*
					:start (+ i 1)))
		  (push i word-start)
		  (setf i pos)
		  (push i word-end)
		  (push nil word-quoted)
		  (push obj args))
	      (error () (do-continue))
;	      (end-of-file () (do-continue))
	      (condition (c) (signal c))))
	   ;; quote char
	   ((eql c #\\)
	    (setf do-quote t)
	    (incf i))
	   ;; whitespace
	   ((position c *whitespace*)
	    (finish-word)
	    (incf i))
	   ;; comment
	   ((eql c #\;)
	    (finish-word)
	    (loop :for j :from i :below len
	       :while (not (eql (aref line j) #\newline))
	       :do (incf i)))
	   ;; pipe
	   ((eql c #\|)
	    (finish-word)
	    (reverse-things)
	    (let ((e (list :pipe (make-the-expr))))
	      (setf args (list e)))
	    (setf word-start (list i))
	    (incf i)
	    (setf word-end (list i)
		  word-quoted (list nil)))
	   ;; redirect
	   ((or (eql c #\<) (eql c #\>))
	    (finish-word)
	    (reverse-things)
	    ;; @@@ need to get the file name as a word
	    (let ((e (list :redirect (make-the-expr))))
	      (setf args (list e)))
	    (setf word-start (list i))
	    (incf i)
	    (setf word-end (list i)
		  word-quoted (list nil)))
	   ;; any other character: add to word
	   (t
	    (when (not in-word)
	      (push i word-start))
	    (setf in-word t)
	    (vector-push-extend c w)
	    (incf i)))
        :finally
	(progn
	  (when in-word
	    (push (copy-seq w) args)
	    (push i word-end)
	    (push did-quote word-quoted))
	  (reverse-things)))
      (if (and (= (length words) 1) (consp (first words)))
	  ;; just a lisp expression to be evaluated
	  (first words)
	  ;; a normal shell expression
	  (make-the-expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Job control

(defun start-job-control ()
  (setf (signal-action nos::SIGTSTP) :ignore
	(signal-action nos::SIGTTIN) :ignore
	(signal-action nos::SIGTTOU) :ignore))

(defun stop-job-control (saved-sigs)
  (let ((tstp (first saved-sigs))
	(ttin (second saved-sigs))
	(ttou (third saved-sigs)))
    (setf (signal-action nos::SIGTSTP) (if (keywordp tstp) tstp :default)
	  (signal-action nos::SIGTTIN) (if (keywordp tstp) ttin :default)
	  (signal-action nos::SIGTTOU) (if (keywordp tstp) ttou :default))))

(defun set-default-job-sigs ()
  (setf (signal-action nos::SIGTSTP) :default
	(signal-action nos::SIGTTIN) :default
	(signal-action nos::SIGTTOU) :default))

;(defun run (cmd args)
  ; block sigchld & sigint
  ; give terminal to child if not running it bg?
  ; fork
  ; in the child:
  ;   unblock sigchld & sigint
  ;   set default action tty signals (TSTP, TIN, TOU)
  ;     or ignore them if not going to be in the foreground
  ;   set the process group setpgid to it's own pid (or group of the pipeline)
  ;   give terminal to child's process group
  ;   exec
  ; in the parent:
  ;   just to be sure:
  ;     set the child's process group (setpgid) to it's own pid
  ;     (or group of the pipeline)
  ;   unblock sigchld & sigint
  ;   wait for the child
;  )

; (defun set-terminal-group (tty group)
;   "Make the terminal TTY be controled by process group GROUP."
;   ;block TTOU TTIN TSTP & CHLD while we do this:
;   (tcsetpgrp tty group))

; (defun init-job-control (sh)
;   (let ((our-process-group (getpgid 0))
; 	(tty-process-group (tcgetpgrp tty))
; 	(our-pid (getpid)))
;     (loop :while (/= our-process-group tty-process-group)
; 	  :do 
; 	  ;; If we're not the foreground process
; 	  ;; Signal the process group that we want input, which will likely
; 	  ;; stop us. Keep demanding the tty until we get it or die.
; 	  (kill SIGTTIN)
; 	  (setf tty-process-group (tcgetpgrp tty)))
;     ;; If for some reason we're not the process group leader,
;     ;; then become it, and take control of the terminal.
;     (when (/= our-process-group our-pid)
;       (setpgid 0 our-pid)
;       (set-terminal-group our-pid))))

(defun in-lisp-path (command)
  "Return true if a command is in the lisp path."
  ;; (loop :with path
  ;;    :for dir :in *lisp-path* :do
  ;;    (when (setf path (probe-file (s+ dir command)))
  ;;      (asdf::resolve-symlinks path))))	; XXX I know, this is cheating.
  (ignore-errors (asdf:find-component nil command)))
;  (asdf:find-component nil command))

(defun load-lisp-command (command)
  "Load a command in the lisp path."
  (let* ((pkg (intern (string-upcase command) :keyword)))
    (if (ignore-errors (asdf:oos 'asdf:load-op pkg :verbose nil))
	;; succeeded
	(progn 
	  ;; (init-commands sh)
	  (get-command command))
	;; failed
	nil)))

(defun do-system-command (command-line &optional in-pipe out-pipe)
  "Run a system command. IN-PIPE is an input stream to read from, if non-nil.
OUT-PIPE is T to return a input stream which the output of the command can be
read from."
  ;; Since run-program can't throw an error when the program is not found, we
  ;; try to do it here.
  (let* ((program (car command-line))
	 (args    (cdr command-line))
	 (path    (get-command-path program))
	 result result-stream)
    (if (not path)
	(error "~a not found." program)
	(progn
	  (set-default-job-sigs)
	  (cond
	    (out-pipe
	     (setf result-stream
		   (if in-pipe
		       (nos:popen path args :in-stream in-pipe)
		       (nos:popen path args))))
	    (in-pipe
	     (nos:popen path args :in-stream in-pipe :out-stream t))
	    (t
	     (setf result
		   #+(or clisp ecl cmu lispworks) (fork-and-exec path args)
		   #+sbcl (nos:run-program path args)	;@@@ until fork fixed
		   #+ccl (nos:system-command path args)	;@@@ until fork fixed
		   )))))
    (values (or result '(0)) result-stream)))

(defun unquoted-string-p (w expr i)
  "True if W in EXPR with index I is _not_ quoted."
  (and (stringp w) (> (length w) 0)
       (and (shell-expr-word-quoted expr)
	    (not (elt (shell-expr-word-quoted expr) i)))))

(defun do-expansions (expr pos)
  "Perform shell syntax expansions / subsitutions on the expression."
  (let ((new-words '()))
    (loop
       :for w :in (shell-expr-words expr)
       :for i = 0 :then (1+ i)
       :do
       (if (unquoted-string-p w expr i)
	 (cond
	   ;; $ environment variable expansion
	   ((eql #\$ (aref w 0))
	    (let ((v (nos:getenv (subseq w 1))))
	      (push (or v "") new-words)))
	   ;; filename globbing, with ~ expansion on
	   ((glob:pattern-p w nil t)
	    (let ((g (glob:glob w :tilde t)))
	      (if g
		(dolist (x g) (push x new-words))
		;; If there's no existing file expansions, but try just twiddle,
		;; and also keep the glob expression if no matches.
		(push (glob:expand-tilde w) new-words))))
	   (t (push w new-words)))
	 ;; Quoted, so just push it verbatim
	 (push w new-words)))
    (setf (shell-expr-words expr) (nreverse new-words)))
  pos)

(defun lisp-exp-eval (words)
  "Evaluate lisp expr in words."
;  (format t "Evaling ~w~%" words)
  (loop :with results
     :for e :in words
     :if (or (consp e) (symbolp e))
       :do (setf results (eval e))
       :and :if (listp results)
         :append results	      ; Spread list results into separate args
       :else
         :collect results
     :else
        :collect e))

(defun expand-alias (sh alias words in-pipe out-pipe)
  (let* ((expr (shell-read alias))
	 (new-expr
	  ;; XXX This trashes the rest of the things in the expr, like
	  ;; the quoted, etc. which could cause problems.
	  (make-shell-expr
	   :words (append (shell-expr-words expr) (cdr words))
	   :line (format nil "~s ~{~s ~}" (shell-expr-line expr) words))))
    (shell-eval sh new-expr :no-alias t :in-pipe in-pipe :out-pipe out-pipe)))

(defun do-command (command args &optional in-pipe out-pipe)
  "Call a command with the given POSIX style arguments."
  (labels ((runky (command args)
	     (let ((lisp-args (posix-to-lisp-args command args))
		   (cmd-func (symbol-function (command-function command))))
	       (if (> (length lisp-args) 0)
		   (apply cmd-func lisp-args)
		   (funcall cmd-func)))))
    (if out-pipe
	(let ((out-str (make-stretchy-string 20)))
	  (values
	   (list (with-output-to-string (*standard-output* out-str)
		   (if in-pipe
		       (let ((*standard-input* in-pipe))
			 (runky command args))
		       (runky command args))))
	   (make-string-input-stream out-str)
	   nil))
	(if in-pipe
	    (let ((*standard-input* in-pipe))
	      (runky command args))
	    (runky command args)))))

(defun read-parenless-args (string)
  "Read and shell-eval all the expressions possible from a string and return
them as a list."
  (loop :with start = 0 :and expr
     :while (setf (values expr start)
		  (read-from-string string nil nil :start start))
     :collect (eval expr)))

(defun shell-eval-command (sh expr &key no-alias in-pipe out-pipe)
  "Evaluate an expression that is a command."
  (let* ((cmd (elt (shell-expr-words expr) 0))
	 #| (args (subseq (shell-expr-words expr) 1)) |#
	 (command (gethash cmd (lish-commands)))
	 (alias (gethash cmd (lish-aliases sh)))
	 (expanded-words (lisp-exp-eval (shell-expr-words expr)))
	 result result-stream)
    (dbug "words = ~w~%" (shell-expr-words expr))
    (dbug "expanded words = ~w~%" expanded-words)
    ;; These are in order of precedence, so:
    ;;  aliases, lisp path, commands, system path
    (cond
      ;; Alias
      ((and alias (not no-alias))
       ;; re-read and re-eval the line with the alias expanded
       (expand-alias sh alias expanded-words in-pipe out-pipe))
      ;; Autoload
      ((and (in-lisp-path cmd)	
	    (setf command (load-lisp-command cmd)))
       ;; now try it as a command
       (do-command command (subseq expanded-words 1) in-pipe out-pipe))
      ;; Lish command
      (command			
       (do-command command (subseq expanded-words 1) in-pipe out-pipe))
      (t
       (flet ((sys-cmd ()
		"Do a system command."
		(setf (values result result-stream)
		      (do-system-command expanded-words in-pipe out-pipe))
		(dbug "result = ~w~%" result)
		(when (not result)
		  (format t "Command failed.~%"))
		(force-output)	   ; @@@ is this really a good place for this?
		(values result result-stream nil)))
	 ;; If we can find a command in the path, try it first.
	 (if (get-command-path (first expanded-words))
	     (sys-cmd)
	     ;; Otherwise try a parenless Lisp line.
	     (multiple-value-bind (symb pos)
		 (read-from-string (shell-expr-line expr) nil nil)
	       (if (and (symbolp symb) (fboundp symb))
		   (values
		    (multiple-value-list
		     (apply (symbol-function symb)
			    (read-parenless-args
			     (subseq (shell-expr-line expr) pos))))
		    nil ;; stream
		    t)	;; show the values
		   ;; Just try a system command anyway, which will likely fail.
		   (sys-cmd)))))))))

(defun shell-eval (sh expr &key no-alias in-pipe out-pipe)
  "Evaluate a shell expression. If NO-ALIAS is true, don't expand aliases.
Return a list of the result values, a stream or NIL, and a boolean which is T to show the values."
  (typecase expr
    (shell-expr
;     (format t "(shell-expr-words expr) = ~w~%" (shell-expr-words expr))
     (when (= (length (shell-expr-words expr)) 0)
       (return-from shell-eval (values nil nil nil)))
     (let ((w0 (elt (shell-expr-words expr) 0)))
       (do-expansions expr 0)
       (dbug "~w~%" expr)
       (if (listp w0)
	 (case (first w0)
	   (:pipe
	    (multiple-value-bind (vals out-stream show-vals)
		(shell-eval sh (second w0) :in-pipe in-pipe :out-pipe t)
	      (declare (ignore show-vals))
	      (when (and vals (> (length vals) 0))
		(with-package *lish-user-package*
		  (shell-eval-command
		   sh
		   (make-shell-expr
		    :words (cdr (shell-expr-words expr))
		    :line (format nil "~{~s ~}"
				  (cdr (shell-expr-words expr))))
		   :no-alias no-alias
		   :in-pipe out-stream
		   :out-pipe out-pipe)))))
	   (:and
	    )
	   (:or
	    )
	   (:sequence
	    ))
	 ;; Not a list
	 (with-package *lish-user-package*
	   (shell-eval-command sh expr
			       :no-alias no-alias
			       :in-pipe in-pipe :out-pipe out-pipe)))))
    (t ;; A full Lisp expression all by itself
     (with-package *lish-user-package*
       (values (multiple-value-list (eval expr)) nil t)))))

(defun load-rc-file (sh)
  "Load the users start up (a.k.a. run commands) file."
;  (without-warning
    (load-file sh (merge-pathnames
		   (user-homedir-pathname)
		   (make-pathname :name ".lishrc"))))

(defun load-file (sh file)
  (if (probe-file file)
      (with-open-file (streamy file :direction :input)
	(with-package *lish-user-package*
	  (loop :with line = nil :and newy-line = t :and expr = nil
	     :while (and (setf line (read-line streamy nil))
			 newy-line)
	     :do
	     (loop :while (and (eql (setf expr (shell-read line))
				    *continue-symbol*)
			       (setf newy-line (read-line streamy nil)))
		:do
		(setf line (format nil "~a~%~a" line newy-line)))
	     (shell-eval sh expr))))))

(defstruct suspended-job
  id
  name
  command-line
  resume-function)

(defun find-id (shell)
  "Return the lowest ID that isn't in use."
  (loop :for i = 1 :then (1+ i)
     :if (not (position i (lish-suspended-jobs shell)
			:key #'suspended-job-id))
     :return i
     :if (> i 10000)
     :do (error "Something probably went wrong with finding a job ID.")))
  
(defun suspend-job (name command-line resume-function)
  "Suspend a job. This should be called by the program that wants to
suspend itself."
  (push (make-suspended-job
	 :id (find-id *shell*)
	 :name name
	 :command-line command-line
	 :resume-function resume-function)
	(lish-suspended-jobs *shell*)))

(defvar *shell-non-word-chars*
  #(#\space #\tab #\newline #\linefeed #\page #\return
    #\( #\) #\[ #\] #\: #\; #\/ #\" #\' #\\ #\# #\, #\` #\| #\.
    #\- #\$ #\~ #\! #\&)
  "Characters that are not considered to be part of a word in the shell.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

;; Just the state of the REPL-y area to make it easy to pass around.
(defstruct read-state
  "The line we've read and the previous line."
  string
  prefix-string)

(defun lish-read (sh state)
  "Read a string with the line editor and convert it shell expressions, handling errors."
  (with-slots ((str string) (pre-str prefix-string)) state
    (handler-case
	(handler-bind
	    (#+sbcl
	     (sb-sys:interactive-interrupt
	      #'(lambda (c)
		  (declare (ignore c))
		  (format t "~%") (finish-output)
		  (invoke-restart (find-restart 'abort))))
	     (condition #'(lambda (c)
			    (if (lish-debug sh)
				(invoke-debugger c)
				(format t "~&~a" c))))
	     (error #'(lambda (c)
			(if (lish-debug sh)
			    (invoke-debugger c)
			    (progn
			      #| (format t "~&~a" c) |#
			      (signal c))))))
	  (progn
	    (setf str (tiny-rl
		       :eof-value *real-eof-symbol*
		       :quit-value *quit-symbol*
		       :context :lish
		       :editor (lish-editor sh)
		       :prompt
		       (if pre-str
			   (lish-sub-prompt sh)
			   (funcall (lish-prompt-function sh) sh)))))
	  (cond
	    ((and (stringp str) (equal 0 (length str))) *empty-symbol*)
	    ((equal str *real-eof-symbol*)		*real-eof-symbol*)
	    ((equal str *quit-symbol*)	  		*quit-symbol*)
	    (t (shell-read (if pre-str
			       (format nil "~a~%~a" pre-str str)
			       str)))))
      #+sbcl
      (sb-sys:interactive-interrupt ()
	(format t "~%") (finish-output)
	(invoke-restart (find-restart 'abort)))
      (end-of-file () *continue-symbol*)
      #| (condition (c) |#
      (error (c)
	(if (lish-debug sh)
	    (invoke-debugger c)
	    (format t "~&~a" c)
	    )
	*error-symbol*))))

(defun lish-eval (sh result state)
  "Evaluate the shell expressions in RESULT."
  (dbug "~s (~a) ~s~%" result (type-of result) (eq result *empty-symbol*))
  (with-slots ((str string) (pre-str prefix-string)) state
    (cond
      ((eq result *continue-symbol*)
       (if (stringp pre-str)
	   (setf pre-str (format nil "~a~%~a" pre-str str))
	   (setf pre-str (format nil "~a" str)))
       (dbug "DO CONTIUE!!~%"))
      ((or (eq result *empty-symbol*) (eq result *error-symbol*))
       ;; do nothing
       (dbug "DO NOTHING!!~%"))
      (t
       (dbug "Do Something!!~%")
       (setf pre-str nil)
       (handler-case
	   (handler-bind
	       (#+sbcl
		(sb-sys:interactive-interrupt
		 #'(lambda (c)
		     (declare (ignore c))
		     (format t "~%") (finish-output)
		     (invoke-restart (find-restart 'abort))))
		#| (warning
		 #'(lambda (c)
		     (format t "Warning: ~a~%" c)
		     (muffle-warning))) |#
		#| #+excl (excl::compiler-note
		    #'(lambda (c)
		(format t "Note: ~a~%" c))) |#
		(serious-condition
		 #'(lambda (c)
		     (if (lish-debug sh)
			 (invoke-debugger c)))))
	     (force-output)
	     (multiple-value-bind (vals stream show-vals)
		 (shell-eval sh result)
	       (declare (ignore stream))
	       (when show-vals
		 (loop :with len = (length vals) :and i = 0
		    :for v :in vals
		    :do
		    (format t "~s" v)
		    (if (and (> len 1) (< i (- len 1)))
			(format t " ;~%"))
		    (incf i)
		    :finally (format t "~&")))))
	 ;; (condition (c)
	 ;; 	 (if (lish-debug sh)
	 ;; 	     (invoke-debugger c)
	 ;; 	     (format t "GOO ~a~%" c)))
	 (error (c)
	   (if (lish-debug sh)
	       (invoke-debugger c)
	       (format t "~a~%" c))))))))

(defun lish (&key debug terminal-name)
  "Unix Shell & Lisp somehow smushed together."
  (let* ((*shell* (make-instance 'shell :debug debug))
	 (sh *shell*)		; shorthand
	 (state (make-read-state))
	 (*lish-level* (if *lish-level*
			   (funcall #'1+ (symbol-value '*lish-level*))
			   0))
	 (saved-sigs (list (signal-action nos::SIGTSTP)
			   (signal-action nos::SIGTTIN)
			   (signal-action nos::SIGTTOU))))
    (declare (special *shell*))	; XXX it's probably already special from defvar
    (update-user-package)
    (nos:setenv "LISH_LEVEL" (format nil "~d" lish::*lish-level*))
    (load-rc-file sh)
    ;; Make a customized line editor
    (setf (lish-editor sh)
	  (make-instance 'tiny-rl:line-editor
			 :non-word-chars *shell-non-word-chars*
			 :completion-func #'shell-complete
			 :context :lish
			 :terminal-device-name terminal-name
			 :prompt-func nil))
    (unwind-protect
	 (progn
	   (start-job-control)
	   (when (not (eq :lish-quick-exit (catch :lish-quick-exit
             (loop
		:named pippy
		:with result = nil :and lvl = *lish-level*
		:if (lish-exit-flag sh)
		  :return (values-list (lish-exit-values sh))
		:end
		:do
		(restart-case
		    (progn
		      (setf result (lish-read sh state))
		      (when (or (eq result *real-eof-symbol*)
				(eq result *quit-symbol*))
			(return-from pippy result))
		      (lish-eval sh result state))
		  (abort ()
		    :report
		    (lambda (stream)
		      (format stream
			      "Return to Lish ~:[~;TOP ~]level~:[~; ~d~]."
			      (= lvl 0) (/= lvl 0) lvl))
		    nil))))))))
      (stop-job-control saved-sigs))
    (when (lish-exit-flag sh)
      (return-from lish (when (lish-exit-values sh)
			  (values-list (lish-exit-values sh)))))
    (format t "*EOF*~%")
    ;; Well, let's hope that this will clear the EOF on *standard-input*
    (clear-input *standard-input*)))

(defvar *standalone* nil
  "True if we are nearly just a shell.") ; [sic]

(defun flash-msg (msg)
  "Temporarity flash a message, nearly subliminally."
  (format t msg) (finish-output) (sleep .2)
  (format t "~v,,,va" (length msg) #\backspace #\backspace)
  (finish-output) (sleep .1)
  (format t "~v,,,va" (length msg) #\space #\space)
  (format t "~v,,,va" (length msg) #\backspace #\backspace))

(defun lishity-split ()
  "Get out real quick."
  (if *standalone*
      (flash-msg "You the man now dog."))
      (throw :lish-quick-exit :lish-quick-exit))

(defun shell-toplevel (&key debug)
  "For being invoked as a standalone shell."
  (setf *standalone* t)
  (format t "Welcome to ~a ~a~%" *shell-name* *version*)
  (let* ((level-string (nos:getenv "LISH_LEVEL")))
    (when level-string
      (setf *lish-level* (parse-integer level-string)))
    (lish :debug debug))
  (nos:exit-lisp))

;; So, like, to do it cleanly, for me:
;;   $LISP -- -norl
;;   (l :tiny-repl)
;;   (l :lish)
;;   (lish:make-standalone)
;; where LISP can be either sbcl or clisp.
;; @@@ what about ccl?

; (defun make-lish ()
;   (

(defun make-standalone (&optional (name "lish"))
  "FUFKFUFUFUFUFF"
  #+sbcl (sb-ext:save-lisp-and-die name :executable t
				   :toplevel #'lish:shell-toplevel)
  #+clisp (ext:saveinitmem name :executable t :quiet t :norc t
			   :init-function #'lish:shell-toplevel)
  #-(or sbcl clisp) (declare (ignore name))
  #-(or sbcl clisp) (missing-implementation 'make-standalone)
  )

;; So we can conditionalize adding of lish commands in other packages.
(d-add-feature :lish)

;; EOF
