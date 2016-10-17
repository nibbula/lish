;;
;; lish.lisp - Unix Shell & Lisp somehow smushed together
;;

;; This file contains the basic REPL and dispatch, and some other odd and ends.

(in-package :lish)

;; (declaim (optimize (debug 3)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

;; The "result" argument is not for the caller, but rather so we can detect
;; cycles in the package inheritance graph.
(defun flattened-package-use-list (package &optional result)
  (loop :for p :in (package-use-list package) :do
     (when (not (position p result))
       (push p result)
       (loop :for ip :in (flattened-package-use-list p result) :do
	  (pushnew ip result))))
  result)

;; This tries to keep :LISH-USER up to date with respect to :CL-USER,
;; because I just so love to push the package system beyond it's limits.
;; This would probably be better done with something like conduits.
(defun update-user-package ()
  ;; Update uses
  (loop :with isym :and isymbol-type :and esym :and esymbol-type
     :for p :in (package-use-list :cl-user) :do
     (when (not (position p (flattened-package-use-list *lish-user-package*)))
       (dbug "Package ~w~%" p)
       ;; Things directly in lish-user are uninterned in favor of one
       ;; in cl-user.
       (unintern-conflicts *lish-user-package* p)
       ;; Conflicts in inherited symbols are resolved by having the
       ;; "explicitly" used package symbol (i.e. things used by :lish-user
       ;; such as :lish) interned and made shadowing.
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
  (let ((h (safe-namestring (truename (user-homedir-pathname)))))
    (if (equal #\/ (aref h (1- (length h))))
	(subseq h 0 (1- (length h)))
	h)))

(defun twiddlify (name)
  "Turn (user-homedir-pathname) occuring in name into a tilde."
  (replace-subseq (safe-namestring (fixed-homedir)) "~"
		  (safe-namestring name) :count 1))

;; This is mostly for bash compatibility.

(defun format-prompt (sh prompt &optional (escape-char #\%))
  "Return the prompt string with bash-like formatting character replacements.
So far we support:
%%	A percent.
%a	#\\bell
%e	#\\escape
%n	#\\newline
%r	#\\return
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
%i      The lisp implementation nickname.
%p      The shortest nickname of *lish-user-package*.
%P      The current value of *lish-user-package*.
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
  (let ((out (make-stretchy-string 80)))
    (with-output-to-string (str out)
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
				 (parse-integer
				  (subseq prompt i (+ i 3)) :radix 8))
				str))
		   (#\s (write-string *shell-name* str))
		   (#\v (princ *major-version* str))
		   (#\V (write-string *version* str))
		   (#\u (write-string (nos:user-name) str))
		   (#\h (write-string dlib:*host* str))
		   (#\H (write-string (machine-instance) str))
		   (#\w (write-string (twiddlify (nos:current-directory)) str))
		   (#\W (write-string
			 (twiddlify (basename (nos:current-directory))) str))
		   (#\$ (write-char
			 (if (= (nos:user-id :effective t) 0) #\# #\$) str))
		   (#\i (write-string *lisp-implementation-nickname* str))
		   (#\p (write-string
			 (s+ (and *lish-user-package*
				  (shortest-package-nick *lish-user-package*)))
			 str))
		   (#\P (write-string
			 (s+ (and *lish-user-package*
				  (package-name *lish-user-package*))) str))
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
	     (write-char c str))))
    out))

(defun symbolic-prompt-to-string (symbolic-prompt &optional ts-in)
  "Take a symbolic prompt and turn it into a string. A symbolic prompt can be
any printable lisp object, which is converted to a string. If it is a list, it
translates sublists starting with certain keywords, to terminal codes to do
text effects to the enclosed objects. The keywords recognized are:
  :BOLD :UNDERLINE :INVERSE
and the colors
  :BLACK :RED :GREEN :YELLOW :BLUE :CYAN :WHITE and :DEFAULT.
The colors can be prefixed by :FG- or :BG- for the foreground or background.
Symbols will be replaced by their value."
  (with-output-to-string (str)
    (if (not (consp symbolic-prompt))
	(princ symbolic-prompt str)
	(let ((ts (or ts-in (make-terminal-stream str 'terminal-ansi))))
	  (loop :for s :in symbolic-prompt :do
	     (typecase s
	       (string (tt-write-string ts s))
	       (character (tt-write-char ts s))
	       (cons
		(cond
		  ((keywordp (car s))
		   (case (car s)
		     (:normal
		      (tt-normal ts)
		      (symbolic-prompt-to-string (cdr s) ts))
		     (:bold
		      (tt-bold ts t)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-bold ts nil))
		     (:underline
		      (tt-underline ts t)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-underline ts nil))
		     (:inverse
		      (tt-inverse ts t)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-inverse ts nil))
		     ((:black :fg-black)
		      (tt-color ts :black nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts :default nil))
		     ((:red :fg-red)
		      (tt-color ts :red nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts :default nil))
		     ((:green :fg-green)
		      (tt-color ts :green nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts :default nil))
		     ((:yellow :fg-yellow)
		      (tt-color ts :yellow nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts :default nil))
		     ((:blue :fg-blue)
		      (tt-color ts :blue nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts :default nil))
		     ((:magenta :fg-magenta)
		      (tt-color ts :magenta nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts :default nil))
		     ((:cyan :fg-cyan)
		      (tt-color ts :cyan nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts :default nil))
		     ((:white :fg-white)
		      (tt-color ts :white nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts :default nil))
		     ((:default :fg-default)
		      (tt-color ts :default nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts :default nil))
		     ;; background
		     ((:bg-black)
		      (tt-color ts nil :black)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts nil :default))
		     ((:bg-red)
		      (tt-color ts nil :red)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts nil :default))
		     ((:bg-green)
		      (tt-color ts nil :green)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts nil :default))
		     ((:bg-yellow)
		      (tt-color ts nil :yellow)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts nil :default))
		     ((:bg-blue)
		      (tt-color ts nil :blue)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts nil :default))
		     ((:bg-magenta)
		      (tt-color ts nil :magenta)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts nil :default))
		     ((:bg-cyan)
		      (tt-color ts nil :cyan)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts nil :default))
		     ((:bg-white)
		      (tt-color ts nil :white)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts nil :default))
		     ((:bg-default)
		      (tt-color ts nil :default)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (tt-color ts nil :default))
		     (otherwise
		      (error "Unrecognized attribute ~a" (car s)))))
		  ((and (symbolp (car s)) (fboundp (car s)))
		   ;; (tt-format ts "~a"
		   ;;   (apply (symbol-function (car s)) (cdr s))))
		   (tt-format ts "~a" (eval s)))
		  (t
		   (error "Unrecognized thing in attribute list ~a" (car s))
		   )))
	       (symbol
		(when (boundp s)
		  (tt-format ts "~a" (symbol-value s))))
	       (t
		(tt-format ts "~a" s)
		)))
	  (tt-finish-output ts)))))

#|
(defun fill-prompt ()
  (#\f (let ((len (length out)) out-char cols)
	 (when (> len 1)
	   (setf out-char (aref out (1- len))
		 cols (terminal-window-columns
		       (tiny-rl::line-editor-terminal
			(lish-editor sh))))
	   (loop :repeat (- cols len)
	      :do (write-char out-char str))))))

1. Expand the %<things> in the symbolic version.
2. Convert to fatchar and expand the %fill which can now know the true size
3. Convert from fatchar to final device form

|#

(defvar *fallback-prompt* "Lish> "
  "Prompt to use as a last resort.")

(defgeneric make-prompt (shell)
  (:documentation "Return a string to prompt with."))
(defmethod make-prompt ((sh shell))
  "Return a string to prompt with."
  (or (and (lish-prompt sh)
	   (format-prompt
	    sh (symbolic-prompt-to-string (lish-prompt sh))))
      ;; (if (and (lish-prompt-char sh)
      ;; 	       (characterp (lish-prompt-char sh)))
      ;; 	  (format nil "~a "
      ;; 		  (make-string (+ 1 *lish-level*)
      ;; 			       :initial-element (lish-prompt-char sh)))
	  *fallback-prompt*))

;; @@@ I know how stupid and unnecessary this is
(defparameter *real-eof-symbol* :Z-REAL-EOF)
(defparameter *continue-symbol* :Z-CONTINUE)
(defparameter *empty-symbol* :Z-EMPTY)
(defparameter *error-symbol* :Z-ERROR)
(defparameter *quit-symbol* :Z-QUIT)

;; Get rid of this is if it's unnecessary.
(defun modified-context (context
			 &key
			   (in-pipe nil in-pipe-p)
			   (out-pipe nil out-pipe-p)
			   (environment nil environment-p)
			   (flipped-io nil flipped-io-p))
  "Return a new context based on CONTEXT, with the given slots."
  (if (not context)
      (make-context 
       :in-pipe in-pipe
       :out-pipe out-pipe
       :environment environment
       :flipped-io flipped-io)
      (let ((c (copy-structure context)))
	(when in-pipe-p     (setf (context-in-pipe     c) in-pipe))
	(when out-pipe-p    (setf (context-out-pipe    c) out-pipe))
	(when environment-p (setf (context-environment c) environment))
	(when flipped-io-p  (setf (context-flipped-io  c) flipped-io))
	c)))

;; (defstruct lisp-expression
;;   "Nothing fancy. Just a wrapper for a lisp value for now."
;;   object)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Job control

(defun start-job-control ()
  #+unix
  (setf (os-unix:signal-action os-unix:+SIGTSTP+) :ignore
	(os-unix:signal-action os-unix:+SIGTTIN+) :ignore
	(os-unix:signal-action os-unix:+SIGTTOU+) :ignore))

(defun stop-job-control (saved-sigs)
  #+unix
  (let ((tstp (first saved-sigs))
	(ttin (second saved-sigs))
	(ttou (third saved-sigs)))
    (setf (os-unix:signal-action os-unix:+SIGTSTP+)
	  (if (keywordp tstp) tstp :default)
	  (os-unix:signal-action os-unix:+SIGTTIN+)
	  (if (keywordp tstp) ttin :default)
	  (os-unix:signal-action os-unix:+SIGTTOU+)
	  (if (keywordp tstp) ttou :default))))

(defun job-control-signals ()
  #+unix (list (os-unix:signal-action os-unix:+SIGTSTP+)
	       (os-unix:signal-action os-unix:+SIGTTIN+)
	       (os-unix:signal-action os-unix:+SIGTTOU+)))

(defun set-default-job-sigs ()
  #+unix
  (setf (os-unix:signal-action os-unix:+SIGTSTP+) :default
	(os-unix:signal-action os-unix:+SIGTTIN+) :default
	(os-unix:signal-action os-unix:+SIGTTOU+) :default))

;(defun run (cmd args)
  ; block sigchld & sigint
  ; give terminal to child if not running in bg?
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
  "Return true if a command can be found by ASDF."
  ;; (loop :with path
  ;;    :for dir :in *lisp-path* :do
  ;;    (when (setf path (probe-file (s+ dir command)))
  ;;      (asdf::resolve-symlinks path))))	; XXX I know, this is cheating.
  (typecase command
    ((or string keyword symbol)
     (ignore-errors (asdf:find-component nil command)))
    (t nil)))

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

(defun handle-job-change (job result status)
  "Take appropriate action when JOB changes status."
  (case status
    (:exited
     ;;(format t ";; Exited ~a~%" result)
     (finish-output)
     (delete-job job)
     result)
    ((:signaled :coredump)
     (format t ";; Killed ~a ~a" (job-name job) (job-pid job))
     #+unix (progn
	      (when (and result (integerp result))
		(format t "~a" (os-unix:signal-description result)))
	      (when (eq status :coredump)
		(format t " Core dump")))
     (terpri)
     (finish-output)
     (delete-job job)
     nil)
    (:error
     (format t ";; Error ~a" result)
     #+unix (when (and result (integerp result))
	      (format t " ~a" (os-unix:error-message result)))
     (terpri)
     (finish-output)
     ;; (delete-job job) ;; ??? Should we?
     result)
    (:stopped
     (format t ";; Stopped ~a ~a~%" (job-name job) (job-pid job))
     (finish-output)
     (setf (job-status job) :suspended)
     nil)))


(defun unquoted-string-p (w expr i)
  "True if W in EXPR with index I is _not_ quoted."
  (and (stringp w) (> (length w) 0)
       (and (shell-expr-word-quoted expr)
	    (not (elt (shell-expr-word-quoted expr) i)))))

(defstruct fake-var
  name
  value
  cacheable
  cached-value
  description)

(defparameter *fake-vars*
  `(("HOSTNAME" nil "Name of the host."		  	 *host*)
    ("HOSTTYPE" t   "Type of the host."		  	 ,#'machine-type)
    ("MACHTYPE" nil "Fully specific platform"	  	 *arch*)
    ("OLDPWD"   nil "Last working directory"
     ,#'(lambda () (lish-old-pwd *shell*)))
    ("PPID"     t   "Parent process ID"		  	 ,#'os-unix:getppid)
    ("PWD"      nil "Current working directory"	  	 ,#'current-directory)
    ("SHLVL"    nil "Shell level"		  	 *lish-level*)
    ("COLUMNS"  nil "Terminal character columns"
     ,#'(lambda () (terminal-window-columns
	      (tiny-rl::line-editor-terminal (lish-editor *shell*)))))
    ("ROWS"     nil "Terminal character rows"
     ,#'(lambda () (terminal-window-rows
	      (tiny-rl::line-editor-terminal (lish-editor *shell*)))))
    ("LINES"    nil "Terminal character rows"
     ,#'(lambda () (terminal-window-rows
	      (tiny-rl::line-editor-terminal (lish-editor *shell*)))))
    ("$"        nil "Current process ID"	  	 ,#'os-unix:getpid)
    ("!"        nil "Process ID of the previous command" nil)	;; @@@
    ("?"        nil "Result of the last command." 	 nil))) ;; @@@

(defparameter *fake-var-table* nil
  "The fake var table.")

(defparameter *fake-var-single-chars* nil
  "Sequence of single characters that are fake vars.")

(defun fake-var-list ()
  "Return a list of the fake variable names."
  (make-fake-var-table)
  (loop :for name :being :the :hash-keys :of *fake-var-table*
     :collect name))

(defun make-fake-var-table ()
  ;;(format t "Making the fake var table.")
  (when (not *fake-var-table*)
    (setf *fake-var-table* (make-hash-table :test #'equal))
    (loop :for (name cacheable desc value) :in *fake-vars* :do
       (setf (gethash name *fake-var-table*)
	     (make-fake-var
	      :name name
	      :value value
	      :cacheable cacheable
	      :description desc))
       ;; Note single char vars
       (when (= (length name) 1)
	 (pushnew (char name 0) *fake-var-single-chars*)))
    (setf *fake-vars* nil)) 		; maybe it could be gc'd
  *fake-var-table*)

(defun fake-var (name)
  "Return the value of fake var named NAME."
  (make-fake-var-table)
  (flet ((evaluate (x)
	   (cond
	     ((functionp x) (funcall x))
	     ((symbolp x) (symbol-value x))
	     (t nil))))
    (let ((var (gethash name *fake-var-table*)))
      ;;(format t "fake-var ~s = ~s~%" name var)
      (when var
	(if (fake-var-cacheable var)
	    (or (fake-var-cached-value var)
		(setf (fake-var-cached-value var)
		      (evaluate (fake-var-value var))))
	    (evaluate (fake-var-value var)))))))

;; @@@ This is overly consy.
(defun remove-backquotes (s)
  "Remove backquotes from the string S, except don't remove double backquotes."
  ;;(join (substitute "\\" "" (split-sequence #\\ s) :test #'equal) ""))
  (with-output-to-string (str)
    (let ((start 0) (last-start 0) (len (length s)))
      (loop
	 :while (and (< start len) (setf start (position #\\ s :start start)))
	 :do
	 (format t "start = ~s last-start = ~s~%" start last-start)
	 (when (> (- start last-start) 0)
	   (princ (subseq s last-start start) str))
	 (when (and (< (1+ start) len) (char= #\\ (char s (1+ start))))
	   (write-char #\\ str)
	   (incf start))
	 (incf start)
	 (setf last-start start))
      (when (< last-start len)
	(princ (subseq s last-start) str)))))

;; a.k.a. parameter expansion
(defun expand-variables (s)
  "Return S with variables expanded."
  (let ((start 0) (last-start 0) (len (length s)))
    (with-output-to-string (str)
      (loop
	 :while (and (< last-start len)
		     (setf start
			   (position-if (_ (or (char= #\$ _) (char= #\\ _)))
					s :start last-start)))
	 :do
	 (cond
	   ;; backslash
	   ((char= #\\ (char s start))
	    (incf start)		; skip over backslash
	    (when (< start len)		; if there is one,
	      (incf start))		; skip over the next char
	    ;; output the first part
	    (when (not (zerop (- start last-start)))
	      (princ (subseq s last-start start) str)))
	   ;; dollar
	   ((char= #\$ (char s start))
	    (when (not (zerop (- start last-start)))
	      (princ (subseq s last-start start) str))
	    (make-fake-var-table)
	    (if (and (< (1+ start) len)
		     (position (char s (1+ start)) *fake-var-single-chars*))
		(progn
		  (princ (or (fake-var (subseq s (1+ start) (+ start 2)))
			     "") str)
		  (incf start 2))
		(let ((end (or (position-if
				(_ (not (or (alphanumericp _) (char= #\_ _))))
				s :start (1+ start))
			       (length s))))
		  (incf start)
		  (if (not (zerop (- end start)))
		      (progn
			;;(format t "looking up |~s| start=~s end=~s~%"
			;;	(subseq s start end) start end)
			(princ (or (fake-var (subseq s start end))
				   (nos:environment-variable
				    (subseq s start end))
				   "") str))
		      (princ "$" str))
		  (setf start end))))
	   (t (error "Shouldn't happen")))
	 ;;(format t "start = ~s last-start = ~s~%" start last-start)
	 (setf last-start start)
	 )
      ;;(format t "end: start = ~s last-start = ~s~%" start last-start)
      (when (not (zerop (- (or start len) last-start)))
	(princ (subseq s last-start (or start len)) str)))))

(defun do-expansions (expr)
  "Perform shell syntax expansions / subsitutions on the expression.
Remove backquotes."
  (let ((new-words '()))
    (loop
       :for w :in (shell-expr-words expr)
       :for i = 0 :then (1+ i)
       :do
       (if (unquoted-string-p w expr i)
	 (cond
	   ;; $ environment variable expansion
	   ((position #\$ w)
	    (push (expand-variables w) new-words))
	   ;; filename globbing, with ~ expansion on
	   ((glob:pattern-p w nil t)
	    (let ((g (glob:glob w :tilde t)))
	      (if g
		(dolist (x g) (push x new-words))
		;; There's no existing file expansions, but try just twiddle,
		;; and also keep the literal glob expression if no matches.
		(push (glob:expand-tilde w) new-words))))
	   ;; global aliases
	   #|((setf v (get-alias w :global t :shell sh))
	    (loop :for a :in (shell-expr-words
			      (shell-read v #| :package *junk-package* |#))
	       :do (push (or v "") new-words))) |#
	   (t (push (remove-backquotes w) new-words)))
	 ;; Quoted, so just push it verbatim
	 (push w new-words)))
    (setf (shell-expr-words expr) (nreverse new-words)))
  expr)

(defun shell-expand-line (editor)
  "A command to expand the current line."
  ;;(format t "editor is a ~a = ~s~%" (type-of editor) editor)
  (let ((buf (tiny-rl::buf editor)))
    (let ((words (possibly-expand-aliases
		  *shell*
		  (lisp-exp-eval
		   (expr-to-words (do-expansions (shell-read buf)))))))
      (tiny-rl::replace-buffer
       editor
       (with-output-to-string (str)
	 (when (first words)
	   (princ (shell-word-word (first words)) str))
	 (loop :for w :in (rest words)
	    :do (write-char #\space str)
	    (if (shell-word-quoted w)
		(format str "\"~a\"" (shell-word-word w))
		(princ (shell-word-word w) str))))))))

(defun expr-to-words (expr)
  (loop
     :for w :in (shell-expr-words expr)
     :for s = (shell-expr-word-start  expr) :then (cdr s)
     :for e = (shell-expr-word-end    expr) :then (cdr e)
     :for q = (shell-expr-word-quoted expr) :then (cdr q)
     :for v = (shell-expr-word-eval   expr) :then (cdr v)
     :collect (make-shell-word :word w
			       :start (car s) :end (car e) :quoted (car q)
			       :eval (car v))))

(defun words-to-expr (words &optional line)
  (let ((expr (make-shell-expr)))
    (loop :for w :in words :do
       (push (shell-word-word   w) (shell-expr-words       expr))
       (push (shell-word-start  w) (shell-expr-word-start  expr))
       (push (shell-word-end    w) (shell-expr-word-end    expr))
       (push (shell-word-quoted w) (shell-expr-word-quoted expr))
       (push (shell-word-eval   w) (shell-expr-word-eval   expr)))
    (setf
     (shell-expr-words       expr) (nreverse (shell-expr-words       expr))
     (shell-expr-word-start  expr) (nreverse (shell-expr-word-start  expr))
     (shell-expr-word-end    expr) (nreverse (shell-expr-word-end    expr))
     (shell-expr-word-eval   expr) (nreverse (shell-expr-word-eval   expr))
     (shell-expr-word-quoted expr) (nreverse (shell-expr-word-quoted expr))
     (shell-expr-line expr)        line)
    expr))

(defun append-words (w1 w2)
  (let ((max (+ 2 (loop :for w :in w1 :maximize (shell-word-end w)))))
    ;; Perhaps unnecessary, but move start and end over.
    (loop :for w :in w2 :do
       (when (shell-word-start w)
	 (incf (shell-word-start w) max))
       (when (shell-word-end w)
	 (incf (shell-word-end w) max)))
    (append w1 w2)))

(defun lisp-exp-eval (words)
  "Evaluate Lisp expressions in the array of shell-words, return a possibly
expanded array of shell-words."
  (loop :with results
     :for w :in words
     :if (and (or (consp (shell-word-word w)) (symbolp (shell-word-word w)))
	      (shell-word-eval w))
     :do (setf results (eval (shell-word-word w)))
     :and :if (listp results)
	;; Spread list results into separate args
        :append (mapcar #'(lambda (x) (make-shell-word :word x))
			results)
     :else
        :collect (make-shell-word :word results)
    :else
      :collect w))

(defvar *input* nil
  "The output of the previous command in pipeline.")

(defvar *output* nil
  "The output of the current command.")

(defvar *accepts* nil
  "What the next command in the pipeline accepts.")

(defun resolve-command (command &optional seen)
  "Figure some crap out, okay."
  (let ((alias (gethash command (lish-aliases *shell*)))
	word)
    (if alias
	(progn
	  (setf word (elt (shell-expr-words (shell-read alias)) 0))
	  (if (not (position command seen :test #'equal)) ; don't circle
	      (progn
		(pushnew command seen :test #'equal)
		(resolve-command word seen))
	      word))
	command)))

(defun get-accepts (expr)
  (typecase expr
    (shell-expr
     (dbugf :accepts "get-accepts: shell-expr ~s~%" expr)
     (get-accepts (elt (shell-expr-words expr) 0)))
    (list
     (dbugf :accepts "get-accepts: list ~s~%" expr)
     (get-accepts (if (keywordp (car expr))
		      (cdr expr)
		      (car expr))))
    (string
     ;;(dbugf :accepts "get-accepts: string ~s~%" expr)
     (let* ((cmd-name (resolve-command expr))
	    (cmd (get-command cmd-name)))
       (when cmd
         (dbugf :accepts "command ~a accepts ~s~%"
		cmd-name (command-accepts cmd)))
       (and cmd (command-accepts cmd))))
    (t
     (dbugf :accepts "get-accepts: -T- ~s ~s~%" (type-of expr) expr)
     :unspecified)))

(defun accepts (first-type &rest other-types)
  "Return true if *ACCEPTS* matches or is a subtype of one of the given types.
This should be used rather than directly testing *ACCEPTS*."
  (let ((types (cons first-type other-types)))
    (labels ((is-like (x type)
	       (or (equal x type) (subtypep x type))))
      (typecase *accepts*
	(sequence (some (_ (position _ *accepts* :test #'is-like)) types))
	(keyword  (some (_ (eq       _ *accepts*)) types))
	(t        (some (_ (is-like  _ *accepts*)) types))))))

(defun successful (obj)
  "Return true if the object represents a successful command result."
  (or
   ;; Zero return value from a system command?
   (and obj (and (numberp obj) (zerop obj)))
   (consp obj))) ;; Any other value from lisp code or commands.

(defun read-parenless-args (string)
  "Read and shell-eval all the expressions possible from a string and return
them as a list."
  ;;; @@@ I think I want to change this to do a shell-read
  ;;(format t "p-l line = ~s~%" string)
  (loop :with start = 0 :and expr
     :while (setf (values expr start)
		  (read-from-string string nil nil :start start))
     ;;:do
     ;;(format t "p-l before eval arg ~s of type ~a~%" expr (type-of expr))
     :collect (eval expr)))

(defmacro with-first-value-to-output (&body body)
  "Evaluuate BODY and set *OUTPUT* to first value."
  (with-unique-names (vals)
    `(values-list
      (let* ((,vals (multiple-value-list (progn ,@body))))
	(setf *output* (first ,vals))
	,vals))))

(defun expand-alias-words (alias words)
  "Take an alias and a shell-words array and return a shell-words array
with the alias expanded."
  (let* ((alias-words (expr-to-words (shell-read alias)))
	 (new-words (append alias-words (subseq words 1))))
    new-words))

(defun possibly-expand-aliases (sh words)
  (if (zerop (length words))
      words
      (let* ((cmd (shell-word-word (elt words 0)))
	     (alias (gethash cmd (lish-aliases sh))))
	(if alias
	    (lisp-exp-eval (expand-alias-words alias words))
	    words))))

;; XXX This can be problematic because in the back and forth to words things
;; in expr can get trashed or are not set, or faked like the shell-expr-line
;; below.
(defun expand-alias (alias words)
  "Take an alias and a shell words array and return a shell-expr with the alias
expanded."
  (let ((new-expr (words-to-expr (expand-alias-words alias words))))
    (setf (shell-expr-line new-expr)
	  (format nil "~{~a ~}" (shell-expr-words new-expr)))
    new-expr))

#|
(defun call-command (command args context)
  "Call a command with the given POSIX style arguments.
COMMAND is a COMMAND object.
ARGS is a list of POSIX style arguments, which are converted to Lisp arguments
by POSIX-TO-LISP-ARGS and given to the COMMAND's function.
If OUT-PIPE is true, return the values:
 a list of the values returned by COMMAND
 a input stream from which can be read the output of command
 and NIL.
If IN-PIPE is true, it should be an input stream to which *STANDARD-INPUT* is
bound during command."
  (with-slots (in-pipe out-pipe environement) context
    (dbugf :accepts "command ~s ~s~%" (command-name command) *accepts*)
    (labels ((runky (command args)
	       (let ((lisp-args (posix-to-lisp-args command args))
		     (cmd-func (symbol-function (command-function command)))
		     (*context* context))
		 (if (> (length lisp-args) 0)
		     (apply cmd-func lisp-args)
		     (funcall cmd-func)))))
      (if out-pipe
	  (let ((out-str (make-stretchy-string 20)))
	    (values
	     ;; @@@ This is probably inefficient.
	     (list (with-output-to-string (*standard-output* out-str)
		     (if in-pipe
			 (let ((*standard-input* in-pipe))
			   (runky command args))
			 (runky command args))))
	     (let ((oo (make-string-input-stream out-str)))
	       ;; (format t "out-str = ~w~%" out-str)
	       ;; (format t "(slurp oo) = ~w~%" (slurp oo))
	       ;; (file-position oo 0)
	       oo)
	     nil))
	  (if in-pipe
	      (let ((*standard-input* in-pipe))
		(runky command args))
	      (runky command args))))))
|#

(defun call-thing (thing args context)
  "Call a command with the given POSIX style arguments.
THING is a COMMAND object or a function/callable symbol.
ARGS is a list of POSIX style arguments, which are converted to Lisp arguments
by POSIX-TO-LISP-ARGS and given to the COMMAND's function.
If OUT-PIPE is true, return the values:
 a list of the values returned by COMMAND
 a input stream from which can be read the output of command
 and NIL.
If IN-PIPE is true, it should be an input stream to which *STANDARD-INPUT* is
bound during command."
  (with-slots (in-pipe out-pipe environement) context
    (let ((command-p (typep thing 'command)))
      (when command-p
	(dbugf :accepts "command ~s ~s~%" (command-name thing) *accepts*))
      (labels ((runky (thing args)
		 (if command-p
		     (let ((lisp-args (posix-to-lisp-args thing args))
			   (cmd-func (symbol-function (command-function thing)))
			   (*context* context))
		       (if (> (length lisp-args) 0)
			   (apply cmd-func lisp-args)
			   (funcall cmd-func)))
		     (eval thing))))
      (if out-pipe
	  (let ((out-str (make-stretchy-string 20)))
	    (values
	     ;; @@@ This is probably inefficient.
	     (list (with-output-to-string (*standard-output* out-str)
		     (if in-pipe
			 (let ((*standard-input* in-pipe))
			   (runky thing args))
			 (runky thing args))))
	     (let ((oo (make-string-input-stream out-str)))
	       ;; (format t "out-str = ~w~%" out-str)
	       ;; (format t "(slurp oo) = ~w~%" (slurp oo))
	       ;; (file-position oo 0)
	       oo)
	     nil))
	  (if in-pipe
	      (let ((*standard-input* in-pipe))
		(runky thing args))
	      (runky thing args)))))))

(defun call-parenless (func line context)
  "Apply the function to the line, and return the proper values. If there are
not enough arguements supplied, and *INPUT* is set, i.e. it's a recipient of
a non-I/O pipeline, supply *INPUT* as the missing tail argument."
  (let ((parenless-args (read-parenless-args line))
	(function-args (lambda-list
			(if (functionp func)
			    (third
			     (multiple-value-list
			      (function-lambda-expression func)))
			    func)))
	(*context* context))
    (if (and (< (length parenless-args) (length function-args))
	     *input*)
	(progn
	  ;;(format t "WOO HOO! parenless input!~%")
	  (if parenless-args
	      (progn
		;;(format t "parenless-args = ~s~%" parenless-args)
		(with-first-value-to-output
		    (apply func `(,@parenless-args ,*input*))))
	      (progn
		;;(format t "just input~%")
		(with-first-value-to-output (apply func (list *input*))))))
	;; no *input* stuffing
	(with-first-value-to-output (apply func parenless-args)))))

(defun do-system-command (words context)
  #| &key in-pipe out-pipe (environment nil env-p) |#
  "Run a system command.
WORDS is a list of shell-words or strings.
IN-PIPE is an input stream to read from, if non-nil.
OUT-PIPE is T to return a input stream which the output of the command can be
read from."
  ;; Since run-program can't throw an error when the program is not found, we
  ;; try to do it here.
  (dbugf :lish-eval "system command ~w ~w~%" words context)
  (let* ((command-line
	  ;; System command arguments must be strings
	  (mapcar (_ (or (and (stringp _) _)
			 (princ-to-string (shell-word-word _))))
		     words))
	 (program (car command-line))
	 (args    (cdr command-line))
	 (path    (get-command-path program))
	 result result-stream pid status job)
    (when (not path)
      (error 'unknown-command-error
	     :command-string program :format "not found."))

    ;; This actually should be in the child process:
    ;;(set-default-job-sigs)
    (with-slots (in-pipe out-pipe environment) context
      (if (or in-pipe out-pipe)
	  (progn
	    ;; (when in-pipe
	    ;;   (format t "thingy: ~s~%would have been: ~s~%"
	    ;; 	  in-pipe
	    ;; 	  (slurp in-pipe))
	    ;;   (file-position in-pipe 0))
	    (setf result-stream
		  (apply #'nos:pipe-program
			 `(,path ,args
				 ,@(when in-pipe `(:in-stream ,in-pipe))
				 ,@(when (not out-pipe) '(:out-stream t))
				 ,@(when environment
					 `(:environment ,environment)))))
	    (when (not out-pipe)
	      (setf result-stream nil)))
	  ;; No pipes
	  (let ((tail (last args))
		background)
	    ;;(format t "tail = ~s~%" tail)
	    (when (equal (car tail) "&")
	      (setf args (nbutlast args))
	      (setf background t)
	      ;; (format t "background = ~a~%args = ~s~%" background args)
	      )
	    (setf pid
		  (apply
		   ;; #+(or clisp ecl lispworks) #'fork-and-exec
		   ;; #-(or clisp ecl lispworks) #'nos:run-program
		   #'uos::forky
		   `(,path ,args
			   ,@(when environment
				   `(:environment ,environment))
			   :background ,background))
		  job (add-job program (join command-line #\space) pid))
	    (if background
		(setf (job-status job) :running)
		;; Wait for it...
		(progn
		  (multiple-value-setq (result status)
		    (uos::wait-and-chill pid))
		  (handle-job-change job result status))))))
    (values (or result '(0)) result-stream)))

;; This ends up calling one of the following to do the actual work:
;;   do-system-command , if it's an external command
;;   call-parenless    , if it's a function
;;   call-command      , if it's a Lish command
;;   eval	       , if it's a object
;; This also directs alias expansion, and lisp sub-expression evaluation.

(defun shell-eval-command (sh expr context &key no-alias)
  "Evaluate a shell expression that is a command.
If the first word is an alias, expand the alias and re-evaluate.
If the first word is a system that can be loaded, load it and try to call it
as a lish command. This is vaugely like autoload.
If the first word is lish command, call it.
If the first word is an executable file in the system path, try to execute it.
If the first word is a symbol bound to a function, call it with the arguments,
which are read like lisp code. This is like a ‘parenless’ function call.
Otherwise just try to execute it with the system command executor, which will
probably fail, but perhaps in similar way to other shells."
  (let* ((words (expr-to-words expr))
	 (cmd (shell-word-word (elt words 0)))
	 #| (args (subseq (shell-expr-words expr) 1)) |#
	 ;; (command (gethash cmd (lish-commands)))
	 (command (get-command cmd))
	 (alias (gethash cmd (lish-aliases sh)))
	 (expanded-words (lisp-exp-eval words))
	 result result-stream)
    (dbugf :lish-eval "words = ~w~%" (shell-expr-words expr))
    (dbugf :lish-eval "expanded words = ~w~%" expanded-words)
    ;; These are in order of precedence, so:
    ;;  aliases, lisp path, commands, system path
    (flet ((sys-cmd ()
	     "Do a system command."
	     (run-hooks *pre-command-hook* cmd :system-command)
	     (setf (values result result-stream)
		   ;; (apply #'do-system-command 
		   ;; 	  `(,expanded-words
		   ;; 	    :in-pipe ,in-pipe
		   ;; 	    :out-pipe ,out-pipe
		   ;; 	    ,@(when evp-p `(:environment ,environment)))))
		   (do-system-command expanded-words context))
	     (run-hooks *post-command-hook* cmd :system-command)
	     (dbugf :lish-eval "result = ~w~%" result)
	     (when (not result)
	       (format t "Command failed.~%"))
	     (force-output)	   ; @@@ is this really a good place for this?
	     (values result result-stream nil))
	   (rest-of-the-line (expr)
	     "Return the rest of the line after the first word."
	     (if (> (length (shell-expr-word-start expr)) 1)
		 ;; (subseq (shell-expr-line expr)
		 ;; 	 (elt (shell-expr-word-start expr) 1))
		 ;; (join (subseq (shell-expr-words expr) 1) " ")
		 (with-output-to-string (str)
		   (loop :with first = t
		      :for w :in (rest (shell-expr-words expr))
		      :for q :in (rest (shell-expr-word-quoted expr))
		      :if first
		        :do (setf first nil)
		      :else
		        :do (princ " " str)
		      :end
		      :if q
		        :do (print w str) ; make strings be strings
		      :else
		        :do (princ w str)))
		 ""))
	   (run-fun (func line)
	     "Apply the func to the line, and return the proper values."
	     (run-hooks *pre-command-hook* cmd :function)
	     (values
	      (prog1
		  (multiple-value-list (call-parenless func line context))
		(run-hooks *post-command-hook* cmd :function))
	      nil  ;; stream
	      t))) ;; show the values
      (cond
	;; Alias
	((and alias (not no-alias))
	 (dbugf :lish-eval "Expanding alias~%")
	 ;; re-read and re-eval the line with the alias expanded
	 (shell-eval sh (expand-alias alias expanded-words) context
		     :no-expansions t))
	;; Lish command
	(command
	 (dbugf :lish-eval "Calling command ~s ~s~%" command context)
	 (run-hooks *pre-command-hook* cmd :command)
	 (multiple-value-prog1
	     (call-thing command (subseq expanded-words 1) context)
	   (run-hooks *post-command-hook* cmd :command)))
	;; Autoload
	((and (lish-autoload-from-asdf sh)
	      (in-lisp-path cmd)
	      (setf command (load-lisp-command cmd)))
	 (dbugf :lish-eval "Trying autoload~%")
	 ;; now try it as a command
	 (run-hooks *pre-command-hook* cmd :command)
	 (multiple-value-prog1
	     (call-thing command (subseq expanded-words 1) context)
	   (run-hooks *post-command-hook* cmd :command)))
	((functionp cmd)
	 (dbugf :lish-eval "Function eval~%")
	 ;; (format t "CHOWZA ~s~%" (rest-of-the-line expr))
	 (run-fun cmd (rest-of-the-line expr)))
	((and (symbolp cmd) (fboundp cmd))
	 (dbugf :lish-eval "fbound symbol eval~%")
	 ;; (format t "FLEOOP ~s~%" (rest-of-the-line expr))
	 (run-fun (symbol-function cmd) (rest-of-the-line expr)))
	((stringp cmd)
	 (dbugf :lish-eval "String command~%")
	 ;; If we can find a command in the path, try it first.
	 (if (get-command-path cmd)
	     (sys-cmd)
	     ;; Otherwise try a parenless Lisp line.
	     (multiple-value-bind (symb pos)
		 (read-from-string (shell-expr-line expr) nil nil)
	       (declare (ignore pos))
	       (if (and (symbolp symb) (fboundp symb))
		   (progn
		     (run-fun (symbol-function symb)
			      ;;(subseq (shell-expr-line expr) pos)
			      (rest-of-the-line expr)
			      ))
		   ;; Just try a system command anyway, which will likely fail.
		   (sys-cmd)))))
	(t ;; Some other type, just return it, like it's self evaluating.
	 ;;(values (multiple-value-list (eval cmd)) nil t))))))
	 (dbugf :lish-eval "Self evaluating ~s ~s~%" cmd context)
	 ;; (values (multiple-value-list
	 ;; 	  ;;(with-first-value-to-output
	 ;; 	  (call-thing (car cmd) (cdr cmd) context))
	 ;; 	 nil t)
	 (call-thing cmd nil context)
	 )))))

;; This does normal expansions, sets up piping and redirections and
;; eventually calls shell-eval-command.

(defun shell-eval (sh expr context &key no-expansions)
  "Evaluate the shell expression EXPR. If NO-EXPANSIONS is true, don't expand
aliases. Return a list of the result values, a stream or NIL, and a boolean
which is true to show the values.

Generally SHELL-EVAL takes the result of SHELL-READ. EXPR is either a
SHELL-EXPR structure or some other Lisp type. If it's not a SHELL-EXPR then
just eval it. If it is a SHELL-EXPR then do the shell expansions on it, as done
by DO-EXPANSIONS. If the first word of EXPR is a list, then it is a compound
command, which is a :PIPE, :AND, :OR, :SEQUENCE.
:PIPE	   takes the output from the piped command, and feeds it as input to
           the subcommand.
:AND	   evaluates each subcommand until one of them is false.
:OR	   evaluates each subcommand until one of them is true.
:SEQUENCE  evaluates each subcommand in sequence, ignoring return values.
"
  (let ((*context* (or context (make-context)))
	first-word vals out-stream show-vals)
    (with-slots (in-pipe out-pipe environment flipped-io) *context*
      (macrolet
	  ((eval-compound (test new-pipe)
	     "Do a compound command. TEST determines whether the next~
	      part of the command gets done. NEW-PIPE is true to make a~
	      new pipe."
	     `(multiple-value-bind (vals out-stream show-vals)
		  (let ((*accepts*
			 (get-accepts (elt (shell-expr-words expr) 1))))
		    (shell-eval sh (second first-word)
				(make-context
				 :in-pipe in-pipe
				 :out-pipe ,new-pipe
				 :environment environment
				 :flipped-io flipped-io)))
		(declare (ignore show-vals) (ignorable vals))
		(when ,test
		  (with-package *lish-user-package*
		    (shell-eval
		     sh
		     (make-shell-expr
		      :words       (cdr (shell-expr-words expr))
		      :word-start  (cdr (shell-expr-word-start expr))
		      :word-end    (cdr (shell-expr-word-end expr))
		      :word-quoted (cdr (shell-expr-word-quoted expr))
		      :word-eval   (cdr (shell-expr-word-eval expr))
		      ;; @@@ perhaps we should retain original,
		      ;; since indexes not adjusted?
		      :line (format nil "~{~a ~}"
				    (cdr (shell-expr-words expr))))
		     ;; @@@ is this right with out-stream?
		     (if out-stream
			 (modified-context *context* :in-pipe out-stream)
			 *context*)
		     :no-expansions no-expansions))))))
	(cond
	  ((not (shell-expr-p expr))
	   (dbugf :lish-eval "Evaluating a Lisp expression.~%")
	   ;; A full Lisp expression all by itself
	   (with-package *lish-user-package*
	     (values (multiple-value-list (eval expr)) nil t)))
	  ((= (length (shell-expr-words expr)) 0)
	   ;; Quick return when no words
	   (return-from shell-eval (values nil nil nil)))
	  ((and (listp (setf first-word (elt (shell-expr-words expr) 0)))
		(keywordp (first first-word)))
	   ;; First word is a list with a keyword, so it's a compound command.
	   (dbugf :lish-eval "Evaluating a compound expression ~a.~%" first-word)
	   (unless no-expansions
	     (do-expansions expr))
	   (case (first first-word)
	     (:pipe
	      (when (not flipped-io)
		(setf *input* *output*
		      *output* nil
		      flipped-io t))
	      (dbugf 'pipe "*input* = ~s~%" *input*)
	      ;;(dbugf :accepts "*accepts* = ~s~%" *accepts*)
	      (setf (values vals out-stream show-vals)
		    (eval-compound (successful vals) t))
	      (dbugf 'pipe "*output* = ~s~%" *output*)
	      (values vals out-stream show-vals))
	     (:and      (eval-compound (successful vals) nil))
	     (:or       (eval-compound (not (successful vals)) nil))
	     (:sequence (eval-compound t nil))
	     (:redirect-to
	      (run-with-output-to
	       (elt (shell-expr-words expr) 1) (elt first-word 1)))
	     (:append-to
	      (run-with-output-to
	       (elt (shell-expr-words expr) 1) (elt first-word 1) :append t))
	     (:redirect-from
	      (run-with-input-from
	       (elt (shell-expr-words expr) 1) (elt first-word 1)))
	     (t
	      (error "Unknown compound command type."))))
	  (t
	   ;; The first word is not a list, so it's a ‘simple’ command.
	   (dbugf :lish-eval "Evaluating a simple command.~%")
	   (unless no-expansions
	     (do-expansions expr))
	   (dbug "~w~%" expr)
	   (with-package *lish-user-package*
	     ;; accepts is :unspecified because we're last in the
	     ;; pipeline.
	     (when (not flipped-io)
	       (setf *input* *output*
		     *output* nil
		     flipped-io t))
	     (dbugf 'pipe "*input* = ~s~%" *input*)
	     (setf (values vals out-stream show-vals)
		   (shell-eval-command sh expr *context*
				       :no-alias no-expansions))
	     (dbugf 'pipe "*output* = ~s~%" *output*)
	     (values vals out-stream show-vals))))))))

(defun load-file (sh file)
  "Load a lish syntax file."
  (let ((*load-pathname* (pathname file))
	(line-number 1)
	expression-start-line
	expr)
    (with-open-file (stream file :direction :input)
      (with-package *lish-user-package*
	(labels
	    ((read-a-line ()
	       (prog1 (read-line stream nil)
		 (incf line-number))))
	  (loop :with line = nil
	     :and new-line = t
	     :while (and (setf line (read-a-line)) new-line)
	     :do
	     (loop :while (and (eql (setf expr (shell-read line))
				    *continue-symbol*)
			       (setf new-line (read-a-line)))
		:do
		;; Keep track of the start line of a continued expression.
		(when (and expr
			   (eql expr *continue-symbol*)
			   (not expression-start-line))
		  (setf expression-start-line line-number))
		(setf line (s+ line #\newline new-line)))
	     (when (and expr (not (eql expr *continue-symbol*)))
	       (setf expression-start-line nil))
	     (shell-eval sh expr *context*))
	  (when (eql expr *continue-symbol*)
	    (error "End of file in expression. Probably starting at line ~a."
		   expression-start-line)))))))

(defun load-rc-file (sh)
  "Load the users start up (a.k.a. run commands) file, if it exists."
;;;  (without-warning
  (let ((file (or *lishrc*
		  (merge-pathnames (user-homedir-pathname)
				   (make-pathname :name ".lishrc"))))
	;; I don't like this special case, but ...
	(*lish-user-package* (find-package :lish-user)))
    (if (probe-file file)
	(load-file sh file)
	;(when (lish-debug sh)
	(format t "Couldn't find RC file: ~s~%" file))))

(defun find-id (shell)
  "Return the lowest ID that isn't in use."
  (loop :for i = 1 :then (1+ i)
     :if (not (position i (lish-jobs shell)
			:key #'job-id))
     :return i
     :if (> i 10000)
     :do (error "Something probably went wrong with finding a job ID.")))

(defun add-job (name command-line thing &key (status :running))
  "Add a job with the given NAME and COMMAND-LINE. THING is either an integer
process ID or a resume function designator. STATUS defaults to :RUNNING."
  (let ((job (make-job
	      :id (find-id *shell*)
	      :name name
	      :command-line command-line
	      :status status)))
    (etypecase thing
      (integer
       (setf (job-pid job) thing
	     (job-process-group job) thing))
      ((or symbol function)
       (setf (job-resume-function job) thing)))
    (push job (lish-jobs *shell*))
    job))

(defun suspend-job (name command-line resume-function)
  "Suspend a job. This should be called by the program that wants to
suspend itself."
  (add-job name command-line resume-function))

(defun delete-job (job)
  "Delete the job. JOB is either a JOB struct or a JOB-ID."
  (when (not (or (job-p job) (integerp job)))
    (error "JOB must be a JOB or an integer JOB-ID."))
  (let ((job-id (if (integerp job) job (job-id job))))
    (setf (lish-jobs *shell*)
	  (delete job-id
		  (lish-jobs *shell*) :test #'= :key #'job-id))))

(defun check-job-status (sh)
  (let (job)
    (multiple-value-bind (pid result status)
	(uos::check-jobs)
      (when pid
	(if (setf job (find pid (lish-jobs sh) :test #'eql :key #'job-pid))
	    (handle-job-change job result status)
	    (format t "Unknown job changed ~a~%" pid))))))

(defvar *shell-non-word-chars*
  #(#\space #\tab #\newline #\linefeed #\page #\return
    #\( #\) #\[ #\] #\: #\; #\/ #\" #\' #\\ #\# #\, #\` #\| #\.
    #\- #\$ #\~ #\! #\&)
  "Characters that are not considered to be part of a word in the shell.")

(defun safety-prompt (sh)
  "Return a prompt, in a manner unlikely to fail."
  (when (lish-prompt-function sh)
    (or (ignore-errors
	  (return-from safety-prompt (funcall (lish-prompt-function sh) sh)))
	(format t "Your prompt function failed.~%")))
  (or (ignore-errors (make-prompt sh))
      (progn
	(format t "Your prompt is broken.~%")
	*fallback-prompt*)))
	;; (when (lish-prompt-char sh)
	;;   (string (lish-prompt-char sh))
	;;   "> "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;
;; This is conceptually like the traditional REPL:
;;
;;  (lish (loop (lish-print (lish-eval (lish-read)))))
;;
;; These functions pass arount a READ-STATE to keep track of continued lines.
;; LISH-READ calls SHELL-READ to get SHELL-EXPRs.
;; LISH-EVAL calls SHELL-EVAL which is the top of everything else.

;; Just the state of the REPL-y area to make it easy to pass around.
(defstruct read-state
  "The line we've read and the previous line."
  string
  prefix-string
  this-command
  last-command)

(defun lish-read (sh state)
  "Read a string with the line editor and convert it shell expressions,
handling errors."
  (with-slots ((str string) (pre-str prefix-string) this-command last-command)
      state
    (handler-case
	(handler-bind
	    (#+sbcl
	     ;; So we can do something on ^C
	     (sb-sys:interactive-interrupt
	      #'(lambda (c)
		  (declare (ignore c))
		  (format t "~%") (finish-output)
		  (invoke-restart (find-restart 'abort))))
	     ;; So we can step through functions
#|	     #+sbcl (sb-ext::step-condition 'tiny-rl::repple-stepper) |#
#|	     (condition #'(lambda (c)
			    (if (lish-debug sh)
				(invoke-debugger c)
				(format t "~&~a" c)))) |#
	     (error #'(lambda (c)
			(if (lish-debug sh)
			    (invoke-debugger c)
			    (progn
			      #| (format t "~&~a" c) |#
			      (signal c))))))
	  (progn
	    ;;(break)
	    (setf str (tiny-rl
		       :eof-value *real-eof-symbol*
		       :quit-value *quit-symbol*
		       :context :lish
		       :editor (lish-editor sh)
		       :prompt
		       (if pre-str
			   (lish-sub-prompt sh)
			   (safety-prompt sh)))))
	  (cond
	    ((and (stringp str) (equal 0 (length str))) *empty-symbol*)
	    ((equal str *real-eof-symbol*)		*real-eof-symbol*)
	    ((equal str *quit-symbol*)	  		*quit-symbol*)
	    (t
	     (setf ! last-command
		   last-command (copy-seq this-command))
	     (shell-read (setf this-command
				 (if pre-str
				     (s+ pre-str #\newline str)
				     str))))))
      #+sbcl
      (sb-sys:interactive-interrupt ()
	(format t "~%") (finish-output)
	(invoke-restart (find-restart 'abort)))
      (end-of-file () *continue-symbol*)
      #| (condition (c) |#
      (error (c)
	(if (lish-debug sh)
	    (invoke-debugger c)
	    (format t "~&~a" c))
	*error-symbol*))))

(defun lish-print (values)
  "Print the results of an evaluation. VALUES are a list of values to print."
  (loop :with len = (length values) :and i = 0
     :for v :in values
     :do
     (format t "~s" v)
     (if (and (> len 1) (< i (- len 1)))
	 (format t " ;~%"))
     (incf i)
     :finally (format t "~&")))

(defun lish-eval (sh result state)
  "Evaluate the shell expressions in RESULT."
  (dbugf 'lish-repl
	 "~s (~a) ~s~%" result (type-of result) (eq result *empty-symbol*))
  (with-slots ((str string) (pre-str prefix-string)) state
    (cond
      ((eq result *continue-symbol*)
       (if (stringp pre-str)
	   (setf pre-str (format nil "~a~%~a" pre-str str))
	   (setf pre-str (format nil "~a" str)))
       (dbugf 'lish-repl "DO CONTIUE!!~%"))
      ((or (eq result *empty-symbol*) (eq result *error-symbol*))
       ;; do nothing
       (dbugf 'lish-repl "DO NOTHING!!~%"))
      (t
       (dbugf 'lish-repl "Do Something!!~%")
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
		 (shell-eval sh result nil)
	       (declare (ignore stream))
	       (when show-vals
		 (lish-print vals))))
	 ;; (condition (c)
	 ;; 	 (if (lish-debug sh)
	 ;; 	     (invoke-debugger c)
	 ;; 	     (format t "GOO ~a~%" c)))
	 (error (c)
	   (if (lish-debug sh)
	       (invoke-debugger c)
	       (format t "~a~%" c))))))))

(defun confirm-quit ()
  (if (lish-jobs *shell*)
      (progn
	(format t "There are stopped jobs. ")
	(confirm "quit the shell"))
      t))

(defun lish (&key debug terminal-name)
  "Unix Shell & Lisp somehow smushed together."
  (let* ((*shell* (make-instance 'shell :debug debug))
	 (sh *shell*)		; shorthand
	 (state (make-read-state))
	 (*lish-level* (if *lish-level*
			   (funcall #'1+ (symbol-value '*lish-level*))
			   0))
	 ! ;-) !
	 (saved-sigs (job-control-signals)))
    ;; Don't do the wacky package updating in other packages.
    (when (eq *lish-user-package* (find-package :lish-user))
      (update-user-package))
    (setf (nos:environment-variable "LISH_LEVEL")
	  (format nil "~d" lish::*lish-level*))
    (load-rc-file sh)
    ;; Make a customized line editor
    (setf (lish-editor sh)
	  (make-instance 'tiny-rl:line-editor
			 :non-word-chars *shell-non-word-chars*
			 :completion-func #'shell-complete
			 :context :lish
			 :terminal-device-name terminal-name
			 :local-keymap (lish-keymap sh)
			 :prompt-func nil))
    (unwind-protect
      (progn
	(start-job-control)
	(when (not (eq :lish-quick-exit (catch :lish-quick-exit
          (loop
           :named pippy
	   :with result = nil
	   :and lvl = *lish-level*
	   :and eof-count = 0
	   :if (lish-exit-flag sh)
	     :if (confirm-quit)
	       :return (values-list (lish-exit-values sh))
	     :else
	       :do (setf (lish-exit-flag sh) nil)
	     :end
	   :end
	   :do
	   (restart-case
	     (progn
	       (check-job-status sh)
	       (setf result (lish-read sh state))
	       (when (and (eq result *real-eof-symbol*) (confirm-quit))
		 (return-from pippy result))
	       (if (eq result *quit-symbol*)
		   (if (and (not (lish-ignore-eof sh)) (confirm-quit))
		       (return-from pippy result)
		       (progn
			 (when (numberp (lish-ignore-eof sh))
			   (if (< eof-count (lish-ignore-eof sh))
			       (incf eof-count)
			       (if (confirm-quit)
				   (return-from pippy result)
				   (setf eof-count 0))))
			 (format t "Type 'exit' to exit the shell.~%")))
		   (lish-eval sh result state)))
	     (abort ()
	       :report
	       (lambda (stream)
		 (format stream
			 "Return to Lish ~:[~;TOP ~]level~:[~; ~d~]."
			 (= lvl 0) (/= lvl 0) lvl))
	       nil))))))))
      (stop-job-control saved-sigs))
    ;;(save-command-stats)
    (run-hooks *exit-shell-hook*)
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
      (flash-msg "You the man now dog.")
      (throw :lish-quick-exit :lish-quick-exit)))

(defun shell-toplevel (&key debug)
  "For being invoked as a standalone shell."
  (setf *standalone* t)
  (format t "Welcome to ~a ~a~%" *shell-name* *version*)
  (let* ((level-string (nos:environment-variable "LISH_LEVEL")))
    (when level-string
      (setf *lish-level* (parse-integer level-string)))
    (lish :debug debug))
  (nos:exit-lisp))

(defun make-standalone (&optional (name "lish"))
  "FUFKFUFUFUFUFF"
  (update-version)
  (save-image-and-exit name #'lish:shell-toplevel))

;; So we can conditionalize adding of lish commands in other packages.
(d-add-feature :lish)

;; EOF
