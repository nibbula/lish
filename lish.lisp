;;
;; lish.lisp - Unix Shell & Lisp somehow smushed together
;;

(in-package :lish)

;(declaim (optimize (debug 3)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
;(declaim (optimize (speed 3) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

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
  (let ((h (safe-namestring (user-homedir-pathname))))
    (if (equal #\/ (aref h (1- (length h))))
	(subseq h 0 (1- (length h)))
	h)))

(defun twiddlify (name)
  "Turn (user-homedir-pathname) occuring in name into a tilde."
  (replace-subseq (safe-namestring (fixed-homedir)) "~"
		  (safe-namestring name) :count 1))

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
	 (write-char c str)))))

(defun symbolic-prompt-to-string (symbolic-prompt &optional ts-in)
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
		   ;; (tt-format ts "~a" (apply (symbol-function (car s)) (cdr s))))
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

(defgeneric make-prompt (shell)
  (:documentation "Return a string to prompt with."))
(defmethod make-prompt ((sh shell))
  "Return a string to prompt with."
  (or (and (lish-prompt-string sh)
	   (format-prompt
	    sh (symbolic-prompt-to-string (lish-prompt-string sh))))
      (if (and (lish-prompt-char sh)
	       (characterp (lish-prompt-char sh)))
	  (format nil "~a "
		  (make-string (+ 1 *lish-level*)
			       :initial-element (lish-prompt-char sh)))
	  "> ")))

;; @@@ I know how stupid and unnecessary this is
(defparameter *real-eof-symbol* :Z-REAL-EOF)
(defparameter *continue-symbol* :Z-CONTINUE)
(defparameter *empty-symbol* :Z-EMPTY)
(defparameter *error-symbol* :Z-ERROR)
(defparameter *quit-symbol* :Z-QUIT)

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

(defun read-string (s)
  "Read a lish string. It has similar syntax to a lisp string. ~
Assumes the opening double quote has already been read. ~
Read until a double quote. Backslash escapes the special meaning of ~
the following character. Return the string and how long it is. If we got to ~
the end and didn't get a close quote, the third value is true.~
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

(defun shell-read (line &key partial (package *lish-user-package*))
  "Read objects in shell syntax and return them. If PARTIAL is true, don't 
signal an error if we can't read a full expression.
The syntax is vaguely like:
  ; comment
  command [arg...]
  command \"string\" !*lisp-object* !(lisp-code)
  command word\ with\ spaces \"string \\\" with a double quote\"
  command | command | ...
  command < file-name
  command > file-name
  ([lisp expressions...])"
;  (setf line (expand-global-aliases line))
  (let (word-start word-end word-quoted word-eval words
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
		 (push nil word-eval)
		 (setf (fill-pointer w) 0
		       in-word nil
		       did-quote nil)))
	     (return-partial ()
	       (push i word-start)
	       (push (subseq line i) args)
	       (push (length line) word-end)
	       (push nil word-quoted)
	       (push nil word-eval)
	       (return-from shell-read
		 (make-shell-expr
		  :line line
		  :words (nreverse args)
		  :word-start (reverse word-start)
		  :word-end (nreverse word-end) 
		  :word-quoted (nreverse word-quoted)
		  :word-eval (nreverse word-eval)
		  )))
	     (do-continue ()
	       "Handle when the expression is incomplete."
	       (if partial
		   (return-partial)
		   (return-from shell-read *continue-symbol*)))
	     (do-reader-error (c)
	       "Handle when the expression has an error."
	       (format t "Gots an ~a ~s~%" partial c)
	       (if partial
		   (return-partial)
		   (signal c)))
	     (next-char ()
	       "Return the next character or NIL."
	       (when (< (+ i 1) len) (aref line (1+ i))))
	     (reverse-things ()
	       "Reverse the things we've been consing, so they're in order."
	       (setf word-start  (reverse word-start)
		     word-end    (nreverse word-end)
		     word-quoted (nreverse word-quoted)
		     word-eval   (nreverse word-eval)
		     words       (nreverse args)))
	     (make-the-expr ()
	       "Make an expression, with it's own copy of the lists."
	       (make-shell-expr
		:line line
		:words (copy-seq words)
		:word-start (copy-seq word-start)
		:word-end (copy-seq word-end)
		:word-quoted (copy-seq word-quoted)
		:word-eval (copy-seq word-eval)))
	     (make-compound (key &optional (inc 2))
	       "Make a compound expression with type KEY."
	       (finish-word)
	       (reverse-things)
	       (let ((e (list key (make-the-expr))))
		 (setf args (list e)))
	       (setf word-start (list i))
	       (incf i inc)
	       (setf word-end (list i)
		     word-quoted (list nil)
		     word-eval (list nil))))
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
	     ;; XXX I don't think \ in words should make them not expand
	     ;(setf did-quote t)
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
	      (push t word-quoted)
	      (push nil word-eval)))
	   ;; a lisp function application
	   ((eql c #\()
	    (finish-word)
	    (handler-bind
		((end-of-file (_  (declare (ignore _)) (do-continue)))
		 (reader-error (_ (do-reader-error _))))
		;; read a form as a separate word
		(multiple-value-bind (obj pos)
		    (with-package package
		      (if partial
			  (clean-read-from-string line *junk-package* nil
						  *continue-symbol* :start i)
			  (read-from-string line nil
					    *continue-symbol* :start i)))
		  (push i word-start)
		  (setf i pos)
		  (push obj args)
		  (push i word-end)
		  (push nil word-quoted)
		  (push nil word-eval))))
	   ;; a lisp expr
	   ((eql c #\!)
	    (finish-word)
	    ;; read a form as a separate word
	    (handler-bind
		((end-of-file (_  (declare (ignore _)) (do-continue)))
		 (reader-error (_ (do-reader-error _))))
	      (multiple-value-bind (obj pos)
		  (with-package package
		    (if partial
			(clean-read-from-string line *junk-package* nil
						*continue-symbol*
						:start (+ i 1))
			(read-from-string line nil *continue-symbol*
					  :start (+ i 1))))
		  (push i word-start)
		  (setf i pos)
		  (push obj args)
		  (push i word-end)
		  (push nil word-quoted)
		  (push t word-eval))))
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
	   ((and (eql c #\|) (not (eql (next-char) #\|)))
	    (make-compound :pipe 1))
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
		  word-quoted (list nil)
		  word-eval (list nil)))
	   ;; and
	   ((and (eql c #\&) (eql (next-char) #\&))
	    (make-compound :and))
	   ;; or
	   ((and (eql c #\|) (eql (next-char) #\|))
	    (make-compound :or))
	   ;; sequence
	   ((eql c #\^)
	    (make-compound :sequence 1))
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
	    (push did-quote word-quoted)
	    (push nil word-eval))
	  (reverse-things)))
      (if (and (= (length words) 1) (consp (first words)))
	  ;; just a lisp expression to be evaluated
	  (first words)
	  ;; a normal shell expression
	  (make-the-expr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Job control

(defun start-job-control ()
  #+unix
  (setf (unix:signal-action unix:+SIGTSTP+) :ignore
	(unix:signal-action unix:+SIGTTIN+) :ignore
	(unix:signal-action unix:+SIGTTOU+) :ignore))

(defun stop-job-control (saved-sigs)
  #+unix
  (let ((tstp (first saved-sigs))
	(ttin (second saved-sigs))
	(ttou (third saved-sigs)))
    (setf (unix:signal-action unix:+SIGTSTP+)
	  (if (keywordp tstp) tstp :default)
	  (unix:signal-action unix:+SIGTTIN+)
	  (if (keywordp tstp) ttin :default)
	  (unix:signal-action unix:+SIGTTOU+)
	  (if (keywordp tstp) ttou :default))))

(defun job-control-signals ()
  #+unix (list (unix:signal-action unix:+SIGTSTP+)
	       (unix:signal-action unix:+SIGTTIN+)
	       (unix:signal-action unix:+SIGTTOU+)))

(defun set-default-job-sigs ()
  #+unix
  (setf (unix:signal-action unix:+SIGTSTP+) :default
	(unix:signal-action unix:+SIGTTIN+) :default
	(unix:signal-action unix:+SIGTTOU+) :default))

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

(defun do-system-command (words
			  &optional in-pipe out-pipe
			    (environment nil env-p))
  "Run a system command. IN-PIPE is an input stream to read from, if non-nil.
OUT-PIPE is T to return a input stream which the output of the command can be
read from."
  ;; Since run-program can't throw an error when the program is not found, we
  ;; try to do it here.
  (let* ((command-line (mapcar #'shell-word-word words))
	 (program (car command-line))
	 (args    (cdr command-line))
	 (path    (get-command-path program))
	 result result-stream)
    (if (not path)
	(error 'unknown-command-error
	       :command-string program :format "not found.")
	(progn
	  (set-default-job-sigs)
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
			       ,@(when env-p `(:environment ,environment)))))
		(when (not out-pipe)
		  (setf result-stream nil)))
	      (setf result
		    #+(or clisp ecl lispworks)
		    (apply #'fork-and-exec
		     `(,path ,args
		       ,@(when env-p `(:environment ,environment))))
		    #-(or clisp ecl lispworks)
		    (apply
		     #'nos:run-program
		     `(,path ,args
		       ,@(when env-p `(:environment ,environment))))
		   ))))
    (values (or result '(0)) result-stream)))

(defun unquoted-string-p (w expr i)
  "True if W in EXPR with index I is _not_ quoted."
  (and (stringp w) (> (length w) 0)
       (and (shell-expr-word-quoted expr)
	    (not (elt (shell-expr-word-quoted expr) i)))))

(defun do-expansions (sh expr pos)
  "Perform shell syntax expansions / subsitutions on the expression."
  (declare (ignore sh))
  (let ((new-words '()))
    (loop :with value
       :for w :in (shell-expr-words expr)
       :for i = 0 :then (1+ i)
       :do
       (if (unquoted-string-p w expr i)
	 (cond
	   ;; $ environment variable expansion
	   ((eql #\$ (aref w 0))
	    (setf value (nos:environment-variable (subseq w 1)))
	    (push (or value "") new-words))
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
	   (t (push w new-words)))
	 ;; Quoted, so just push it verbatim
	 (push w new-words)))
    (setf (shell-expr-words expr) (nreverse new-words)))
  pos)

(defun expr-to-words (expr)
  (loop
     :for w :in (shell-expr-words expr)
     :for s = (shell-expr-word-start expr) :then (cdr s)
     :for e = (shell-expr-word-end expr) :then (cdr e)
     :for q = (shell-expr-word-quoted expr) :then (cdr q)
     :for v = (shell-expr-word-eval expr) :then (cdr v)
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
     ;;(format t "shell-expr ~s~%" expr)
     (get-accepts (elt (shell-expr-words expr) 0)))
    (list
     ;;(format t "list ~s~%" expr)
     (get-accepts (if (keywordp (car expr))
		      (cdr expr)
		      (car expr))))
    (string
     ;;(format t "string ~s~%" expr)
     (let* ((cmd-name (resolve-command expr))
	    (cmd (get-command cmd-name)))
       (and cmd (command-accepts cmd))))
    (t
     ;;(format t "-T- ~s ~s~%" (type-of expr) expr)
     :unspecified)))

(defun accepts (first-type &rest other-types)
  "Return true if *ACCEPTS* matches one of the given types."
  (let ((types (cons first-type other-types)))
    (typecase *accepts*
      (sequence (some (_ (position _ *accepts*)) types))
      (keyword  (some (_ (eq       _ *accepts*)) types))
      (t        (some (_ (equal    _ *accepts*)) types)))))

(defun successful (obj)
  "Return true if the object represents a successful command result."
  (or
   ;; Zero return value from a system command?
   (and obj (and (numberp obj) (zerop obj)))
   t)) ;; Any other value from lisp code or commands.

(defun shell-eval (sh expr &key no-expansions in-pipe out-pipe)
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
  (macrolet ((eval-compound (test new-pipe)
	       "Do a compound command. TEST determines whether the next part~ 
                of the command gets done. NEW-PIPE is true to make a new pipe."
	       `(multiple-value-bind (vals out-stream show-vals)
		    (shell-eval sh (second w0)
				:in-pipe in-pipe
				:out-pipe ,new-pipe)
		  (declare (ignore show-vals) (ignorable vals))
		  (when ,test
		    (with-package *lish-user-package*
		      (shell-eval
		       sh
		       (make-shell-expr
			:words	     (cdr (shell-expr-words expr))
			:word-start  (cdr (shell-expr-word-start expr))
			:word-end    (cdr (shell-expr-word-end expr))
			:word-quoted (cdr (shell-expr-word-quoted expr))
			:word-eval   (cdr (shell-expr-word-eval expr))
			;; @@@ perhaps we should retain original,
			;; since indexes not adjusted?
			:line (format nil "~{~a ~}"
				      (cdr (shell-expr-words expr))))
		       :no-expansions no-expansions
		       :in-pipe out-stream
		       :out-pipe out-pipe))))))
    (typecase expr
      (shell-expr
       ;; Quick return when no words
       (when (= (length (shell-expr-words expr)) 0)
	 (return-from shell-eval (values nil nil nil)))
       (let ((w0 (elt (shell-expr-words expr) 0))
	     vals out-stream show-vals)
	 ;; (when (equalp w0 "opt") ;; <<<
	 ;;   (break))
	 (unless no-expansions
	   (do-expansions sh expr 0))
	 (dbug "~w~%" expr)
	 (if (listp w0)
	     ;; Compound command
	     (case (first w0)
	       (:pipe
		(let* ((*accepts*
			(get-accepts (elt (shell-expr-words expr) 1)))
		       (*input* *output*)
		       (*output* nil))
		  (dbug "*input* = ~s~%" *input*)
		  (setf (values vals out-stream show-vals)
			(eval-compound (successful vals) t))
		  (dbug "*output* = ~s~%" *output*)
		  (values vals out-stream show-vals)))
	       (:and      (eval-compound (successful vals) nil))
	       (:or       (eval-compound (not (successful vals)) nil))
	       (:sequence (eval-compound t nil)))
	     ;; Not a list, a ‘simple’ command
	     (with-package *lish-user-package*
	       (dbug "*input* = ~s~%" *input*)
	       (setf (values vals out-stream show-vals)
		     (shell-eval-command sh expr
					 :no-alias no-expansions
					 :in-pipe in-pipe :out-pipe out-pipe))
	       (dbug "*output* = ~s~%" *output*)
	       (values vals out-stream show-vals)))))
      (t ;; A full Lisp expression all by itself
       (with-package *lish-user-package*
	 (values (multiple-value-list (eval expr)) nil t))))))

(defun expand-alias (sh alias words in-pipe out-pipe)
  (let* ((expr (shell-read alias))
	 ;; XXX This could be problematic because things in expr get trashed
	 ;; or are not set, or faked like the shell-expr-line below.
	 (expr-words (expr-to-words expr))
	 (new-words (append expr-words (subseq words 1)))
	 (new-expr (words-to-expr new-words)))
    (setf (shell-expr-line new-expr)
	  (format nil "~{~a ~}" (shell-expr-words new-expr)))
    (shell-eval sh new-expr :no-expansions t
		:in-pipe in-pipe :out-pipe out-pipe)))

(defun call-command (command args &optional in-pipe out-pipe)
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
  (labels ((runky (command args)
	     (let ((lisp-args (posix-to-lisp-args command args))
		   (cmd-func (symbol-function (command-function command))))
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
	    (runky command args)))))

(defun read-parenless-args (string)
  "Read and shell-eval all the expressions possible from a string and return
them as a list."
  ;;; @@@ I think I want to change this to do a shell-read
  (loop :with start = 0 :and expr
     :while (setf (values expr start)
		  (read-from-string string nil nil :start start))
     :collect (eval expr)))

(defun shell-eval-command (sh expr &key no-alias in-pipe out-pipe)
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
    (dbug "words = ~w~%" (shell-expr-words expr))
    (dbug "expanded words = ~w~%" expanded-words)
    ;; These are in order of precedence, so:
    ;;  aliases, lisp path, commands, system path
    (flet ((sys-cmd ()
	     "Do a system command."
	     (run-hooks *pre-command-hook* cmd :system-command)
	     (setf (values result result-stream)
		   (do-system-command expanded-words in-pipe out-pipe))
	     (dbug "result = ~w~%" result)
	     (when (not result)
	       (format t "Command failed.~%"))
	     (force-output)	   ; @@@ is this really a good place for this?
	     (values result result-stream nil))
	   (rest-of-the-line (expr)
	     "Return the rest of the line after the first word."
	     (if (> (length (shell-expr-word-start expr)) 1)
		 (subseq (shell-expr-line expr)
			 (elt (shell-expr-word-start expr) 1))
		 ""))
	   (run-fun (func line)
	     "Apply the func to the line, and return the proper values."
	     (run-hooks *pre-command-hook* cmd :function)
	     (values
	      (multiple-value-list (apply func (read-parenless-args line)))
	      nil  ;; stream
	      t))) ;; show the values
      (cond
	;; Alias
	((and alias (not no-alias))
	 ;; re-read and re-eval the line with the alias expanded
	 (expand-alias sh alias expanded-words in-pipe out-pipe))
	;; Autoload
	((and (in-lisp-path cmd)
	      (setf command (load-lisp-command cmd)))
	 ;; now try it as a command
	 (run-hooks *pre-command-hook* cmd :command)
	 (call-command command (subseq expanded-words 1) in-pipe out-pipe))
	;; Lish command
	(command
	 (run-hooks *pre-command-hook* cmd :command)
	 (call-command command (subseq expanded-words 1) in-pipe out-pipe))
	((stringp cmd)
	 ;; If we can find a command in the path, try it first.
	 (if (get-command-path cmd)
	     (sys-cmd)
	     ;; Otherwise try a parenless Lisp line.
	     (multiple-value-bind (symb pos)
		 (read-from-string (shell-expr-line expr) nil nil)
	       (if (and (symbolp symb) (fboundp symb))
		   (progn
		     (run-fun (symbol-function symb)
			      (subseq (shell-expr-line expr) pos)))
		   ;; Just try a system command anyway, which will likely fail.
		   (sys-cmd)))))
	((functionp cmd)
	 (run-fun cmd (rest-of-the-line expr)))
	((and (symbolp cmd) (fboundp cmd))
	 (run-fun (symbol-function cmd) (rest-of-the-line expr)))
	(t ;; Some other type, just return it, like it's self evaluating.
	 (values (multiple-value-list (eval cmd)) nil t))))))

(defun load-file (sh file)
  "Load a lish syntax file."
  (with-open-file (stream file :direction :input)
    (with-package *lish-user-package*
      (let ((*load-pathname* (pathname file)))
	(loop :with line = nil
	   :and new-line = t
	   :and expr = nil
	   :while (and (setf line (read-line stream nil)) new-line)
	   :do
	   (loop :with i = 0
	      :while (and (eql (setf expr (shell-read line))
			       *continue-symbol*)
			  (setf new-line (read-line stream nil)))
	      :do
	      (setf line (s+ line #\newline new-line))
	      ;; (when (> i 100)
	      ;;    (break))
	      (incf i))
	   (shell-eval sh expr))))))
      ;; (loop :while (setf line (read-line stream nil))
      ;; 	 :do
      ;; 	 (if (eql line *continue-symbol*)
      ;; 	     (shell-read line

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

(defun safety-prompt (sh)
  "Return a prompt, in a manner unlikely to fail."
  (if (lish-prompt-function sh)
      (or (ignore-errors
	    (funcall (lish-prompt-function sh) sh))
	  "Your prompt function failed> ")
      (or (ignore-errors (make-prompt sh))
	  "Your prompt is broken> ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main

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

(defun confirm-quit ()
  (if (lish-suspended-jobs *shell*)
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
    (declare (special *shell*))	; XXX it's probably already special from defvar
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
