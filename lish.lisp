;;;
;;; lish.lisp - A Lisp shell.
;;;

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
	      (shadow isym *lish-user-package*))
	     (:inherited
	      (when (not (eq (symbol-package esym) (symbol-package isym)))
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

;; Get rid of this is if it's unnecessary.
(defun modified-context (context
			 &key
			   (in-pipe nil in-pipe-p)
			   (out-pipe nil out-pipe-p)
			   (environment nil environment-p)
			   (flipped-io nil flipped-io-p)
			   (pipe-plus nil pipe-plus-p)
			   (background nil background-p))
  "Return a new context based on CONTEXT, with the given slots."
  (if (not context)
      (make-context 
       :in-pipe in-pipe
       :out-pipe out-pipe
       :environment environment
       :flipped-io flipped-io
       :pipe-plus pipe-plus
       :background background)
      (let ((c (copy-structure context)))
	(when in-pipe-p     (setf (context-in-pipe     c) in-pipe))
	(when out-pipe-p    (setf (context-out-pipe    c) out-pipe))
	(when environment-p (setf (context-environment c) environment))
	(when flipped-io-p  (setf (context-flipped-io  c) flipped-io))
	(when pipe-plus-p   (setf (context-pipe-plus   c) pipe-plus))
	(when background-p  (setf (context-background  c) background))
	c)))

;; (defstruct lisp-expression
;;   "Nothing fancy. Just a wrapper for a lisp value for now."
;;   object)

(defun %find-shell-word (expr position &optional (word-num 0))
  (loop
     :for word :in (shell-expr-words expr)
     :do
     (typecase word
       (shell-word
	(when (<= position (shell-word-end word))
	  (throw 'found (list
			 (if (>= position (shell-word-start word))
			     word
			     nil)
			 word-num))))
       (cons
	(when (and (keywordp (first word))
		   (shell-expr-p (second word)))
	  (%find-shell-word (second word) position word-num))))
     (incf word-num))
  (list nil nil))

(defun find-shell-word (expr position)
  (values-list
   (catch 'found
     (%find-shell-word expr
		       position
		       ;; (min position
		       ;; 	    ;; (1- (length (shell-expr-line expr)))
		       ;; 	    (length (shell-expr-line expr))
				 ))))

(defun shell-word-num (expr pos)
  "Return the shell expression's word that position POS is in."
  (multiple-value-bind (word num) (find-shell-word expr pos)
    (if word
	num
	(and num (max 0 (1- num))))))

(defun shell-word-at (expr pos)
  "Return the shell expression's word that position POS is in."
  (first (multiple-value-list (find-shell-word expr pos))))

(defun word-word (word)
  "Word is bond."
  (typecase word
    (shell-word (shell-word-word word))
    (t word)))

(defun word-quoted (word)
  (typecase word
    (shell-word (shell-word-quoted word))
    (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Job control

(cffi:defcallback sigtstp-handler :void ((signal-number :int))
  (declare (ignore signal-number))
  ;;(format t "[Terminal Stop]~%") (finish-output)
  #+unix (setf uos::*got-tstp* :dont-suspend)
  ;; I'm scared of this.
  ;; (invoke-restart (find-restart 'abort))
  ;;(throw 'interactive-interrupt nil)
  )

(defun start-job-control ()
  #+unix
  (setf (os-unix:signal-action os-unix:+SIGTSTP+) 'sigtstp-handler
	(os-unix:signal-action os-unix:+SIGTTIN+) :ignore
	(os-unix:signal-action os-unix:+SIGTTOU+) :ignore))

(defun stop-job-control (saved-sigs)
  #-unix
  (declare (ignore saved-sigs))
  #+unix
  (let ((tstp (first saved-sigs))
	(ttin (second saved-sigs))
	(ttou (third saved-sigs)))
    (setf (os-unix:signal-action os-unix:+SIGTSTP+)
	  (if (keywordp tstp) tstp :default)
	  (os-unix:signal-action os-unix:+SIGTTIN+)
	  (if (keywordp ttin) ttin :default)
	  (os-unix:signal-action os-unix:+SIGTTOU+)
	  (if (keywordp ttou) ttou :default))))

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

(defun handle-job-change (job result status &key foreground)
  "Take appropriate action when JOB changes status."
  (case status
    (:exited
     ;; Only announce normal exits when not running in the foreground.
     (when (not foreground)
       (format t ";; Exited ~a ~a~%" (job-name job) result))
     (finish-output)
     (delete-job job)
     result)
    ((:signaled :coredump)
     (format t ";; Killed ~a ~a" (job-name job) (job-pid job))
     #+unix (progn
	      (when (and result (integerp result))
		(format t " ~a" (os-unix:signal-description result)))
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

;; (defun unquoted-string-p (w expr i)
;;   "True if W in EXPR with index I is _not_ quoted."
;;   (and (stringp w) (> (length w) 0)
;;        (and (shell-expr-word-quoted expr)
;; 	    (not (elt (shell-expr-word-quoted expr) i)))))

(defun unquoted-string-p (word)
  "True if WORD is _not_ quoted."
  ;; @@@ ??? I'm not sure why I'm requiring it to be of non-zero length.
  (or (and (stringp word) (> (length word) 0))
      (and (shell-word-p word)
	   (stringp (shell-word-word word))
	   (> (length (shell-word-word word)) 0)
	   (not (shell-word-quoted word)))))

(defstruct fake-var
  "A compatibility variable."
  name
  value
  cacheable
  cached-value
  description)

;; Please don't add to these. These are just for superficial compatibility with
;; POSIX shells.
(defparameter *fake-vars*
  `(("HOSTNAME" nil "Name of the host."		  	 *host*)
    ("HOSTTYPE" t   "Type of the host."		  	 ,#'machine-type)
    ("MACHTYPE" nil "Fully specific platform"	  	 *arch*)
    ("OLDPWD"   nil "Last working directory"
     ,#'(lambda () (lish-old-pwd *shell*)))
    #+unix
    ("PPID"     t   "Parent process ID"		  	 ,#'os-unix:getppid)
    ("PWD"      nil "Current working directory"	  	 ,#'current-directory)
    ("SHLVL"    nil "Shell level"		  	 *lish-level*)
    ("COLUMNS"  nil "Terminal character columns"
     ,#'(lambda () (terminal-window-columns
	      (rl:line-editor-terminal (lish-editor *shell*)))))
    ("ROWS"     nil "Terminal character rows"
     ,#'(lambda () (terminal-window-rows
	      (rl:line-editor-terminal (lish-editor *shell*)))))
    ("LINES"    nil "Terminal character rows"
     ,#'(lambda () (terminal-window-rows
		    (rl:line-editor-terminal (lish-editor *shell*)))))
    ;; These are readonly unlike the POSIX ones. But you can, as you may know:
    ;; (setf (lish-start-time *shell*) (get-universal-time))
    ;; and of course:
    ;; (setf *random-state* (make-random-state))
    ;; to get similar (or better) functionality.
    ("SECONDS"	nil "Seconds elapsed since some time"
		,#'(lambda () (- (get-universal-time)
				 (lish-start-time *shell*))))
    ("RANDOM"	nil "A random 16 bit number."
		,#'(lambda () (random (1- (ash 1 15)))))
    #+unix
    ("$"        nil "Current process ID"	  	 ,#'os-unix:getpid)
    ("!"        nil "Process ID of the previous command" nil)	;; @@@
    ("?"        nil "Result of the last command." 	 nil)) ;; @@@
  "List of: Name Cacheable-p Description Value")

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
(defun remove-backslashes (s)
  "Remove quoting backslashes from the string S, except don't remove doubled
backslashes."
  (with-output-to-string (str)
    (let ((start 0) (last-start 0) (len (length s)))
      (loop
	 :while (and (< start len) (setf start (position #\\ s :start start)))
	 :do
	 ;;(format t "start = ~s last-start = ~s~%" start last-start)
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
  "Return a string based on the string S with variables expanded."
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
	   (t (error "Variable parsing messed up somehow.")))
	 ;;(format t "start = ~s last-start = ~s~%" start last-start)
	 (setf last-start start)
	 )
      ;;(format t "end: start = ~s last-start = ~s~%" start last-start)
      (when (not (zerop (- (or start len) last-start)))
	(princ (subseq s last-start (or start len)) str)))))

;; @@@ Perhaps we could actually do the evaluation in here?
(defun expand-bang (word)
  "Expand a shell word starting with !. If the history-expansion shell option
is set, and the expression is an integer, do history expansion.
Otherwise, return words which will evaluate a lisp expression."
  (or
   (and
    (lish-history-expansion *shell*)
    (and word (stringp word) (char= (char word 0) #\!))
    (let (results expansion)
      (handler-case
	  (multiple-value-bind (obj pos)
	      (read-from-string (subseq word 1) nil)
	    (declare (ignore pos)) ;; @@@ wrong, should complain
	    (if (and obj (integerp obj)
		     *shell* (get-option *shell* 'history-expansion))
		;;(setf expansion (shell-read (rl:history-nth obj))
		(setf expansion (rl:history-entry-line (rl:history-nth obj))
		      results
		      (typecase expansion
			(shell-expr (shell-expr-words expansion))
			(t (list expansion))))
		;;(push (make-shell-word :word obj :eval t) results)))
		(push obj results)))
	(end-of-file ())
	(reader-error ()))
      results))
   word))

(defun expr-from-args (args)
  "Return a shell expression made up of ARGS as the words."
  (let* (words
	 (line (with-output-to-string (str)
		(loop :with pos = 0 :and as-string
		   :for a :in args :do
		   (when (not (zerop pos))
		     (princ #\space str)
		     (incf pos))
		   (setf as-string (princ-to-string a))
		   (push (make-shell-word
			  :word a
			  :start pos
			  :end (+ pos (length as-string)))
			 words)
		   (incf pos (length as-string))
		   (princ a str)))))
    (make-shell-expr :line line :words (reverse words))))

(defun lisp-exp-eval (expr)
  "Return a shell-expr with Lisp expressions expanded."
  (make-shell-expr
   :words
   (loop :with results :and first-word = t
      :for w :in (shell-expr-words expr)
      :if (and (not first-word)
	       (shell-word-p w)
	       (or (consp (shell-word-word w)) (symbolp (shell-word-word w)))
	       (shell-word-eval w))
      :do
      (setf results (eval (shell-word-word w)))
      :and :if (and (listp results) (not (shell-word-quoted w)))
	;; Spread list results into separate args
        :append (mapcar #'(lambda (x) (make-shell-word :word x))
			results)
      :else
        :collect (make-shell-word :word results)
      :else
        :collect w
      :do (setf first-word nil))
   ;; @@@ doesn't fix the line
   :line (shell-expr-line expr)))

;; @@@ should probably rename lisp-exp-eval to this
(defun expand-lisp-exp (exp)
  (lisp-exp-eval exp))

(defun number-padding (i)
  "Return how much padding we might need for an integer between zero and I."
  (if (zerop i) 0 (1+ (realpart (log i 10)))))

(defun starts-with-superfluous-zero (s)
  "Return true if there's a pointless zero at the beginning of a string
representing an integer."
  (or (and (> (length s) 1) (char= (char s 0) #\0))
      (and (> (length s) 2) (char= (char s 0) #\-) (char= (char s 1) #\0))))

(defun range-padding (start start-int end end-int)
  "How much padding might we need for the range START .. END."
  (and (or (and start-int (starts-with-superfluous-zero start))
	   (and end-int (starts-with-superfluous-zero end)))
       (truncate (max (number-padding start-int)
		      (length start)
		      (number-padding end-int)
		      (length end)))))

(defun sequence-steps (start end step is-char prefix suffix)
  "Return a list of strings in the sequence from START to END, by counting STEP.
If IS-CHAR is true, return characters in the range instead of numbers.
If IS-CHAR is not true, and START or END are not integers, return NIL."
  (let* ((start-int
	  (if is-char
	      (char-code (char start 0))
	      ;; (parse-integer start :junk-allowed t)))
	      (ignore-errors (parse-integer start))))
	 (end-int
	  (if is-char
	      (char-code (char end 0))
	      ;; (parse-integer end :junk-allowed t)))
	      (ignore-errors (parse-integer end))))
	 test)
    (when (not (and start-int end-int))
      (return-from sequence-steps nil))
    (cond
      ((zerop step) (return-from sequence-steps nil))
      ((and (> start-int end-int) (not (minusp step)))
       (setf step (- step)))
      ((and (< start-int end-int) (not (plusp step)))
       (setf step (- step))))
    (setf test
	  (if (< start-int end-int)
	      (if (plusp step) #'<= #'>=)
	      (if (minusp step) #'>= #'<=)))
    (if is-char
	(loop
	   :with i = start-int
	   :and limit = end-int
	   :while (funcall test i limit)
	     :collect (s+ prefix (code-char i) suffix)
	   :do (incf i step))
	(let ((zero-padding (range-padding start start-int end end-int)))
	  (loop
	     :with i = start-int
	     :and limit = end-int
	     :while (funcall test i limit)
	     :if zero-padding
	       :collect (format nil "~a~v,'0d~a" prefix zero-padding i suffix)
	     :else
               :collect (s+ prefix i suffix)
	     :end
	     :do (incf i step))))))

;; I don't think there's any formal specification of this. It isn't in POSIX.
;; I don't know who came up with it. Maybe it's from ksh? My implementation is
;; probably subtly different from others. I tend to agree with the way zsh
;; seems to do braces, so I think this code probably works more like zsh than
;; bash. It's just so convenient and useful, that I'm okay with sacrificing
;; curly braces to it. But I'm not really into adding character classes like
;; the zsh BRACE_CCL option, probably because it's so easy to generate arbitrary
;; sequences with just plain Lisp.

;; @@@ We should probably get rid of RECURSIVE keyword, by an flet or a
;; sub-function.

;; @@@ FIXME: "_{b{a,e}}_" expands to itself not "_{ba}_" "_{be}_"
;; It's a malformed expression, but the way that it fails doesn't make sense,
;; and isn't the same as other shells. I'm having trouble coming up with a fix
;; that doesn't involve totally re-writing the way that I do recursive
;; expressions.

(defun expand-braces (string &key (do-sequences t) recursive)
  "Expand shell braces expressions in a string. Return a list of expanded words,
which may be a list of just the STRING if no expansion was done. If DO-SEQUENCES
is true (the default), expand shell sequence expressions.

For internal purposes, we pass RECURSIVE true when we are inside a nested brace
expression, and return a second value to update how much of the string we
consumed.

Alternatives:
alt_exp : '{' brace_term ( ',' brace_term )+ '}'
brace_term : [^{},]+

E.g.:
{a,b,c}      -> a b c
x{a,b,c}y    -> xay xby xcy
{a,b{a,A},c} -> a ba bA c

Sequences:
seq_exp : '{' brace_char '..' brace_char [ '..' integer ] '}' |
          '{' integer '..' integer [ '..' integer ] '}
brace_char : [^{},]

E.g.:
{1..5}   -> 1 2 3 4 5
x{1..5}y -> x1y x2y x3y x4y x5y
{-2..2}  -> -2 -1 0 1 2
{a..g}   -> a b c d e f g
{
"
  (when (not (lish-expand-braces *shell*))
    (return-from expand-braces string))
  (let ((len (length string))
	(start 0) (i 0)
	prefix suffix alternatives output-words
	in-brace in-seq in-alts	seq-start)
    (loop
       :while (< i len)
       :do
       (case (aref string i)
	 (#\{
	  (if in-brace
	      ;; Nested braces
	      (multiple-value-bind (alts new-i)
		  (expand-braces (subseq string start) :recursive t)
		(append (or alternatives '()) alts)
		(setf i (+ start new-i)))
	      (setf prefix (subseq string start i)
		    start (1+ i)
		    seq-start (1+ i)
		    in-brace t)))
	 (#\,
	  (when in-brace
	    (when in-seq
	      ;; Pretend the sequence was just an alternative.
	      (setf start seq-start
		    in-seq nil
		    alternatives nil))
	    (push (subseq string start i) alternatives)
	    (setf start (1+ i)
		  seq-start (1+ i)
		  in-seq nil
		  in-alts t)))
	 (#\.
	  (when (and in-brace do-sequences (not in-alts)
		     (< (1+ i) len) (char= #\. (aref string (1+ i))))
	    (push (subseq string start i) alternatives)
	    (incf i)
	    (setf start (1+ i)
		  in-seq t)))
	 (#\}
	  (when in-brace
	    (push (subseq string start i) alternatives)
	    (setf suffix (if recursive
			     (subseq string (1+ i)
				     (position #\, string :start i))
			     (subseq string (1+ i)))
		  in-brace nil
		  start (1+ i))
	    (cond
	      ((and in-seq alternatives)
	       (setf alternatives (nreverse alternatives))
	       (let ((is-char (and (= 1 (length (first alternatives)))
				   (= 1 (length (second alternatives))))))
		 ;; (format t "is-char = ~s~%alts = ~s~%" is-char alternatives)
		 (case (length alternatives)
		   (2
		    (setf output-words
			  (append output-words
				  (sequence-steps (first alternatives)
						  (second alternatives)
						  1 is-char prefix suffix)))
		    ;; (format t "output-words = ~s~%" output-words)
		    )
		   (3
		    (let ((step (ignore-errors (parse-integer
						(third alternatives)))))
		      (when step
			(setf output-words
			      (append output-words
				      (sequence-steps (first alternatives)
						      (second alternatives)
						      step is-char
						      prefix suffix)))))
		    ;; (format t "output-words = ~s~%" output-words)
		    ))))
	      ((and alternatives (> (length alternatives) 1))
	       (loop :for i :in alternatives :do
		  (push (s+ prefix i suffix) output-words))))
	    ;; Expand possible remaining brace expressions in the suffix
	    (if output-words
		(progn
		  ;; (format t "derp ~s~%" output-words)
		  (setf output-words
			(flatten
			 (loop :for w :in output-words
			    :collect (expand-braces w))))
		  (loop-finish))
		(progn
		  ;; Ignore malformed things?
		  (setf start 0
			in-brace nil
			alternatives nil
			)))))
	 (#\\ (incf i)))
       (incf i))
    (values (or output-words (list string)) i)))

(defun expand-filenames (string)
  "Expand filenames with glob in STRING. Return a list of filenames or just
STRING if it's not a pattern or there's no matching files."
  (or (and (glob:pattern-p string nil t)
	   (glob:glob string :tilde t))
      string))

#|
(defun expand-word-once (word)
  "Try to exapnd the string WORD and return it's expansion. If no further
expansion can be done, return WORD itself (i.e. eq)."
  (cond
    ((position #\$ w)
     ;; $ environment variable expansion
     (let ((expansion (expand-variables w)))
       (push (if (shell-word-p word)
		 (make-shell-word
		  :word expansion
		  :start (shell-word-start word)
		  ;; @@@ this could overlap a following word
		  :end (+ (shell-word-start word) (length expansion))
		  :quoted t
		  :eval (shell-word-eval word))
		 (make-shell-word :word expansion :quoted t))
	     new-words)))
    ((glob:pattern-p w nil t)
     ;; filename globbing, with ~ expansion on
     (let ((g (glob:glob w :tilde t)))
       (if g
	   (dolist (x g) (push x new-words)) ;; @@@ fix to not dup
	   ;; There's no existing file expansions, but try just
	   ;; twiddle, and also keep the literal glob expression
	   ;; if no matches.
	   (push (glob:expand-tilde w) new-words))))
    ((eql (char w 0) #\!)
     ;; !bang expansion
     (loop :for e :in (expand-bang w) :do
	;; (format t "--> ~s~%" e)
	(push e new-words)))
    ((shell-word-p word)
     ;; quoted word without anything special to expand
     (setf (shell-word-word word) (remove-backslashes w))
     (push word new-words))
    ((stringp word)
     (push (remove-backslashes word) new-words))
    (t
     (push word new-words))))
|#

;; In POSIX shells the order of expansion is:
;;   history *
;;   brace *
;;   tilde
;;   variables (and parameters)
;;   arithmetic
;;   command substitution
;;   word splitting *
;;   filename expansion *
;;   quote removal
;; Things with * can change the number of "words".
;;
;; Even though we have differnet expansions, and I'm not always fond of the way
;; other shells do it, we should probably try not to violate peoples' general
;; expectations much.

(defparameter *expansions*
  ;; function           until-stable?  word / line
  `((expand-bang        nil            :word)
    (expand-braces      nil            :word)
    (expand-tilde       nil            :word)
    (expand-variables   t              :word)
    ;;(expand-lisp-exp    t              :line) ;; lisp-exp-eval
    ;;(expand-lisp-exp    t              :expr)
    (expand-filenames   nil            :word)
    (remove-backslashes nil            :word)
    ))

(defparameter *recursive-expansion-limit* 100000
  "Limit on how many recursive expansions to do.")

(defun do-expansions (expr)
  "Perform shell syntax expansions / subsitutions on the expression.
Remove backslash quotes."
  ;; (let ((words (map 'list #'word-word (shell-expr-words expr)))
  (let ((words (shell-expr-words expr))
	new-words func until-stable unit)
    (labels
	((push-word (w)
	   (typecase w
	     (string (push (make-shell-word :word w) new-words))
	     (t (push w new-words))))
	 (apply-func (func word until-stable unit)
	   (let ((result word) (i 0))
	     (if until-stable
		 (loop :with last-result
		    :while (not (equal last-result
				       (setf result
					     (funcall func result))))
		    :do (setf last-result result)
		    (incf i)
		    (when (and *recursive-expansion-limit*
			       (>= i *recursive-expansion-limit*))
		      (cerror "Keep expanding"
			      "Shell expansion bailed out after ~s iterations.
Set lish::*recursive-expansion-limit* higher (or to nil) and continue if you
really want to keep expanding." i)))
		 (setf result (funcall func word)))
	     (case unit
	       ((:line :expr)
		(setf new-words result))
	       (:word
		(if (listp result)
		    (mapc (_ (push-word _)) result)
		    (push-word result))))))
	 (expand-word (word)
	   (cond
	     ((not (unquoted-string-p word))
	      ;; Quoted, so just push it verbatim.
	      (push-word word))
	     ((shell-word-p word)
	      (expand-word (shell-word-word word)))
	     ((stringp word)
	      (apply-func func word until-stable unit))
	     ((consp word)
	      ;; Assume it's one of the menagerie: (e.g. :pipe :redirect-*)
	      (do-expansions (second word))
	      (push-word word))
	     (t
	      ;; It's something else? like a pre-read or pre-evaled lisp obj?
	      (push-word word)))))
      (loop :for e :in *expansions*
	 :do
	 (setf func         (first e)
	       until-stable (second e)
	       unit         (third e))
	 (ecase unit
	   ;; (:expr
	   ;;  (format t "~s expand expr ~s~%" func expr)
	   ;;  (apply-func func  until-stable unit)
	   ;;  )
	   (:line
	    ;; (format t "~s expand line ~s~%" func words)
	    (apply-func func words until-stable unit))
	   (:word
	    (loop :for word :in words
	       :do
	       (expand-word word)
	       ;; (format t "~s expand word ~s~%" func word)
	       )
	    (setf words (nreverse new-words)
		  new-words nil)))
	 (setf (shell-expr-words expr) words))))
  expr)

(defparameter *compound-expr-strings*
  '(:and           "&&"
    :or            "||"
    :sequence      "^"
    :redirect-to   ">"
    :append-to     ">>"
    :redirect-from "<"
    :pipe          "|"
    :pipe-plus     "|+")
  "For reconstructing expression strings.")

(defun compound-tag-string (keyword)
  "Return the string representation for the compound operator keyword THING."
  (getf *compound-expr-strings* keyword))

(defun %shell-words-to-string (words stream &key literal-line)
  "The internal part of shell-words-to-*-string."
  (declare (ignore literal-line))
  (let (#| start end |# skip)
    (labels ((write-thing (w)
	       (typecase w
		 (string (princ (quotify w) stream))
		 (character (print w stream))
		 (cons
		  (let ((s (compound-tag-string (car w))))
		    (if (and s (shell-expr-p (second w)))
			(format stream
			 "~a ~a~a"
			 (shell-words-to-string
			  (shell-expr-words (second w)))
			 s
			 (shell-words-to-string (rest w)))
			(write w :stream stream :readably t :case :downcase))))))
	     (write-it (w &optional space)
	       (setf skip nil)
	       ;; @@@ This whole literal-line thing is dubious because
	       ;; the start and end positions in words can be totally messed
	       ;; up, especially in regard to the line fragment we're passed.
	       #|
	       (when literal-line
		 (cond
		   ((and (shell-word-p w)
			 (shell-word-start w) (shell-word-end w))
		    ;; A word with positions, just update start and end
		    ;; and skip further processing.
		    (when (not start)
		      (setf start (shell-word-start w)))
		    (setf end (shell-word-end w)
			  skip t))
		   ((and (shell-word-p w)
			 (or (not (shell-word-start w))
			     (not (shell-word-end w)))
			 start end)
		    ;; A word without positions, write out what we got, and
		    ;; clear the start and end.
		    (when space
		      (write-char #\space stream))
		    (write-string (subseq literal-line start end)
				  stream)
		    (setf start nil end nil))))
	       |#
	       (when (not skip)
		 (when space
		   (write-char #\space stream))
		 (cond
		   ((and (shell-word-p w) (word-quoted w))
		    (write-char #\" stream)
		    (write-thing (word-word w))
		    (write-char #\" stream))
		   (t
		    (write-thing (word-word w)))))))
      (when (first words)
	(write-it (first words)))
      (loop :for w :in (rest words)
	 :do (write-it w t))
      #|
      (when (and literal-line start end)
	;; Write out the last piece.
	(write-char #\space stream)
	(write-string (subseq literal-line start end) stream))
      |#
      )))

(defun shell-words-to-string (words &key literal-line)
  "Put a list of shell words, properly quoted, into a string separated by
spaces. This of course loses some data in the words. If LITERAL-LINE is given,
try to take as much as we can from it as the original line."
  (with-output-to-string (stream)
    (%shell-words-to-string words stream :literal-line literal-line)))

(defun shell-words-to-fat-string (words &key literal-line)
  "Put a list of shell words, properly quoted, into a fat string separated by
spaces. This of course loses some data in the words. If LITERAL-LINE is given,
try to take as much as we can from it as the original line."
  (with-output-to-fat-string (stream)
    (%shell-words-to-string words stream :literal-line literal-line)))

(defun shell-words-to-list (words)
  "Return shell words as a list of strings."
  (mapcar #'word-word words))

;; This is sort of equivalent to wordexp.
(defun shell-expand (thing)
  "Expand THING and return a shell expression. If THING is a string it is turned
into a shell-expr with shell-read."
  (possibly-expand-aliases
   *shell*
     (do-expansions
	 (lisp-exp-eval
	  (etypecase thing
	    (string (shell-read thing))
	    (shell-expr thing))))))

;; Maybe we should provide a shorthand for this?
(defun shell-expand-to-list (thing)
  "Read and expand THING and return a list of words."
  (shell-words-to-list
   (shell-expr-words
    (shell-expand thing))))

(defun shell-expand-line (editor)
  "A command to expand the current line."
  ;;(format t "editor is a ~a = ~s~%" (type-of editor) editor)
  (let* ((buf (rl:get-buffer-string editor))
	 (words (shell-expr-words
		 (possibly-expand-aliases
		  *shell*
		  (do-expansions (lisp-exp-eval (shell-read buf)))))))
    (rl::use-first-context (editor)
      (rl:replace-buffer
       editor
       (shell-words-to-fat-string words)))))
#|
    (with-output-to-string (str)
       (labels ((write-thing (w)
		  (typecase w
		     (string (princ (quotify w) str))
		     (cons (write w :stream str :readably t :case :downcase))))
		;;(write w :stream str :readably t :case :downcase))
		(write-it (w)
		  (cond
		    ((and (shell-word-p w) (word-quoted w))
		     (write-char #\" str)
		     (write-thing (word-word w))
		     (write-char #\" str))
		    (t
		     (write-thing (word-word w))))))
	 (when (first words)
	   (write-it (first words)))
	 (loop :for w :in (rest words)
	    :do (write-char #\space str)
	    (write-it w)))))))
|#

;; @@@ This is very WIP at moment.
(rl:defsingle shell-help-key (editor)
  (handler-case
      (use-first-context (editor)
        (with-context ()
	  (multiple-value-bind (type word)
	      (guess-word-before (rl:get-buffer-string editor) inator::point)
	    (labels ((symbol-help ()
		       (let ((symbol (symbolify word :no-new t)))
			 (if symbol
			     (let* (result
				    (doc
				     (fatchar-io:with-output-to-fat-string (str)
				       (setf result
					     (%doc symbol :all t
						   :stream str)))))
			       (inator:message
				editor
				"~s ~s~%~/fatchar-io:print-string/"
				type symbol
				(or (and result doc)
				    (and (fboundp symbol)
					 (function-help symbol 0))))
			       (setf (rl::keep-message editor) t))
			     (inator:message
			      editor "FAIL ~s ~s ~s~%" type symbol word))))
		     (command-help ()
		       (inator:message
			editor
			"~s ~s~%~/fatchar-io:print-string/"
			type word
			(fatchar-io:with-output-to-fat-string (stream)
			  (%doc word :stream stream)))
		       (setf (rl::keep-message editor) t)))
	      (case type
		(:symbol (symbol-help))
		(:command (command-help))
		(:command-or-symbol
		 (case (command-type *shell* word)
		   ((:builtin-commandd :shell-command :command)
		    (command-help))
		   (:external-command
		    ;; @@@ if it has a man page show it
		    ;; otherwise do our fake command help
		    (command-help))
		   (:file
		    ;; @@@ show a man page if it exists
		    )
		   (:directory
		    ;; @@@ like maybe ls -l or something, but that violates
		    ;; dependencies :(
		    )
		   (t (symbol-help))))
		(otherwise
		 ;; (inator:message editor "Sorry. No help for a ~s." type)
		 (error "FUCKALL")
		 ))))))
    (condition (x)
      (inator:message editor "Help got an error: ~s" x))))

(defun nth-expr-word (n expr)
  "Return the Nth, potentially unwrapped, word of the shell-expr."
  (let ((w (nth n (shell-expr-words expr))))
    (typecase w
      (shell-word
       (shell-word-word w))
      (t w))))

(defun resolve-command (command &optional seen)
  "Try to figure out what the command really is, for testing accepts."
  (let ((alias (gethash command (lish-aliases *shell*)))
	word)
    (if alias
	(progn
	  (setf word (nth-expr-word 0 (shell-read alias)))
	  (if (not (position command seen :test #'equal)) ; don't circle
	      (progn
		(pushnew command seen :test #'equal)
		(resolve-command word seen))
	      word))
	command)))

(defun get-accepts (expr)
  (typecase expr
    (shell-expr
     (get-accepts (nth-expr-word 0 expr)))
    (list
     (get-accepts (if (keywordp (car expr))
		      (cdr expr)
		      (car expr))))
    (string
     (let* ((cmd-name (resolve-command expr))
	    (cmd (get-command cmd-name)))
       (and cmd (command-accepts cmd))))
    (t
     :unspecified)))

;; *accepts*
;;   accept-type
;; where
;;   accept-type =>
;;     keyword
;;     or

;; Fake types are keywords.
;; Fake types are for things that would be hard to specify with a type
(defun accepts (first-type &rest other-types)
  "Return true if *ACCEPTS* matches or is a subtype of one of the given types.
This should be used rather than directly testing *ACCEPTS*."
  ;; (let ((types (cons first-type other-types)))
  (let ((types (append (list first-type) other-types)))
    (labels ((is-like (x type)
	       (or (equal x type)
		   #+clisp (ignore-errors (subtypep x type))
		   #-clisp (subtypep x type)
		   )))
      (typecase *accepts*
	(cons
	 (if (keywordp (car *accepts*))
	     (some (_ (position _ *accepts* :test #'is-like)) types)
	     (some (_ (is-like *accepts* _)) types)))
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
     :while (progn
	      (setf (values expr start)
		    (read-from-string string nil *real-eof-symbol*
				      :start start))
	      (not (eq expr *real-eof-symbol*)))
     ;;:do
     ;;(format t "p-l before eval arg ~s of type ~a~%" expr (type-of expr))
     :collect (eval expr)))

(defmacro with-first-value-to-output (&body body)
  "Evaluuate BODY and set *OUTPUT* to first value."
  (with-names (vals)
    `(values-list
      (let ((,vals (multiple-value-list (progn ,@body))))
	(setf (output) (first ,vals))
	,vals))))

;; (defun expand-alias-words (alias words)
;;   "Take an alias and a shell-words array and return a shell-words array
;; with the alias expanded."
;; ;;  (let* ((alias-words (expr-to-words (shell-read alias)))
;;   (let* ((alias-words (shell-expr-words (shell-read alias)))
;; 	 (new-words (append alias-words (subseq words 1))))
;;     new-words))

(defun possibly-expand-aliases (sh expr)
  "Return a shell-expr with aliases expanded. This does lisp expression
expansion in the alias expansion."
  (if (zerop (length (shell-expr-words expr)))
      expr
      (let* ((cmd (nth-expr-word 0 expr))
	     (alias (gethash cmd (lish-aliases sh))))
	(if alias
	    (lisp-exp-eval (expand-alias alias expr))
	    expr))))

;; XXX This can be problematic because in the back and forth to words things
;; in expr can get trashed or are not set, or faked like the shell-expr-line
;; below.
(defun expand-alias (alias expr)
  "Take an alias and a shell-expr and return a shell-expr with the alias
expanded."
  (let* ((expanded-expr (do-expansions (shell-read alias)))
	 (expanded-line (shell-expr-line expanded-expr))
	 (expr-tail (cdr (shell-expr-words expr)))
	 (new-words (append (shell-expr-words expanded-expr) expr-tail))
	 (new-line (s+ expanded-line #\space (shell-words-to-string expr-tail))))
    (make-shell-expr
     :words new-words
     :line new-line)))

(defun command-type (sh command &key already-known)
  "Return a keyword representing the command type of COMMAND, or NIL.
If ALREADY-KNOWN is true, only check for already cached commands, don't bother
consulting the file system."
  ;;(declare (ignore already-known)) ;; @@@
  (let (cmd)
    ;; The order here is important and should reflect what actually happens
    ;; in shell-eval.
    (cond
      ((setf cmd (gethash command (lish-commands)))
       (typecase cmd
	 (external-command			  :external-command)
	 (builtin-command			  :builtin-command)
	 (shell-command				  :shell-command)
	 (t					  :command)))
      ((gethash command (lish-aliases sh))        :alias)
      ((gethash command (lish-global-aliases sh)) :global-alias)
      ;; @@@ A loadable system isn't really a command, rather a potential
      ;; command, so maybe it shouldn't be in here?
      ((loadable-system-p command)		  :loadable-system)
      ((get-command-path
	command :already-known already-known)	  :file)
      ((and (lish-auto-cd sh)
	    (directory-p (expand-tilde command))) :directory)
      ((and (fboundp (symbolify command)))	  :function)
      (t nil))))

(defun call-parenless (func line context)
  "Apply the function to the line, and return the proper values. If there are
not enough arguements supplied, and *INPUT* is set, i.e. it's a recipient of
a non-I/O pipeline, supply *INPUT* as the missing tail argument."
  (let ((parenless-args (read-parenless-args line))
	(function-args (argument-list
			(if (functionp func)
			    (third
			     (multiple-value-list
			      (function-lambda-expression func)))
			    func)))
	(*context* context))
    (if (and (< (length parenless-args) (length function-args))
	     *input*)
	(progn
	  (if parenless-args
	      (progn
		(with-first-value-to-output
		    (if (context-pipe-plus *context*)
			(let ((curried (_ (apply func `(,@parenless-args ,_)))))
			  ;; @@@ maybe there's faster way to do this?
			  (apply #'omap curried (list *input*)))
			(apply func `(,@parenless-args ,*input*)))))
	      (progn
		(with-first-value-to-output
		    (if (context-pipe-plus *context*)
			(omap func *input*)
			(apply func (list *input*)))))))
	;; no *input* stuffing
	(with-first-value-to-output (apply func parenless-args)))))

(defmacro maybe-do-in-background ((bg-p name args) &body body)
  (with-names (thunk)
    `(flet ((,thunk () (progn ,@body)))
       (if (and ,bg-p bt:*supports-threads-p*)
	   (progn
	     (let ((nn (prin1-to-string ,name))
		   (aa ,args))		; so we only eval once
	       (setf (lish-last-background-job *shell*)
		     (add-job nn (or (and aa (shell-words-to-string aa)) "")
			      (bt:make-thread #',thunk :name nn)))))
	   (progn
	     (funcall #',thunk))))))

(defun call-thing (thing args context &optional parenless)
  "Call a command or function with the given POSIX style arguments.
THING is a COMMAND object or a function/callable symbol.
ARGS is a list of POSIX style arguments, which are converted to Lisp arguments
by POSIX-TO-LISP-ARGS and given to the COMMAND's function.
If OUT-PIPE is true, return the values:
 a list of the values returned by COMMAND
 a input stream from which can be read the output of command
 and NIL.
If IN-PIPE is true, it should be an input stream to which *STANDARD-INPUT* is
bound during command.
If PARENLESS is set, it's the text of rest of the line to be fed to
CALL-PARENLESS."
  (with-slots (in-pipe out-pipe environement) context
    (let ((command-p (typep thing 'command))
	  (bg (context-background context)))
      (labels ((runky (thing args)
		 (cond
		   (command-p
		     (let ((lisp-args (posix-to-lisp-args thing args))
			   (cmd-func (symbol-function (command-function thing)))
			   (*context* context))
		       (if (> (length lisp-args) 0)
			   (maybe-do-in-background (bg (command-name thing) args)
			     (apply cmd-func lisp-args))
			   (maybe-do-in-background (bg (command-name thing) args)
                             (funcall cmd-func)))))
		   (parenless
		    (call-parenless thing parenless context))
		   (t
		    (maybe-do-in-background (bg thing args)
		      (let ((- thing))
			(eval thing)))))))
	(if out-pipe
	    (let ((out-str (make-stretchy-string 20)))
	      (values
	       ;; @@@ This totally stupid
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
		  (if command-p
		      (runky thing args)
		      (let ((vals (multiple-value-list (runky thing args))))
			(values vals nil t))))
		(if command-p
		    (runky thing args)
		    ;; (values (list (runky thing args)) nil t))))))))
		    (let ((vals (multiple-value-list (runky thing args))))
		      (values vals nil t)))))))))

(defun do-system-command (expr context)
  "Run a system command.
EXPR is a shell-expr.
IN-PIPE is an input stream to read from, if non-nil.
OUT-PIPE is T to return a input stream which the output of the command can be
read from."
  (let* ((command-line
	  ;; System command arguments must be strings
	  (mapcar (_ (or (and (stringp _) _)
			 (princ-to-string (shell-word-word _))))
		     (shell-expr-words expr)))
	 (program (car command-line))
	 (args    (cdr command-line))
	 (path    #| (get-command-path program) |#)
	 result result-stream pid status job)
    ;; Since run-program can't throw an error when the program is not found,
    ;; we try to do it here.
    (loop
       :while
       (not (with-simple-restart (continue "Try the command again.")
	      (setf path (get-command-path program))
	      (when (not path)
		(signal
		 'unknown-command-error
		 :name 'path
		 :command-string program :format "not found."))
	      (setf path (get-command-path program)))))

    ;; This actually should be in the child process:
    ;;(set-default-job-sigs)
    (with-slots (in-pipe out-pipe environment background) context
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
	  (let (#|(tail (last args))
		background |#)
	    ;;(format t "tail = ~s~%" tail)
	    #|
	    (when (equal (car tail) "&")
	      (setf args (nbutlast args))
	      (setf background t)
	      ;; (format t "background = ~a~%args = ~s~%" background args)
	      )
	    |#
	    (setf pid
		  (apply
		   ;; #+(or clisp ecl lispworks) #'fork-and-exec
		   ;; #-(or clisp ecl lispworks) #'nos:run-program
		   #+unix #'uos::forky
		   #-unix #'nos:run-program
		   `(,path ,args
			   ,@(when environment
				   `(:environment ,environment))
			   :background ,background))
		  job (add-job program
			       (join-by-string command-line #\space) pid))
	    ;; &&& temporarily re-get the terminal so we can debug
	    ;; (sleep .2)
	    ;; (uos::syscall (uos:tcsetpgrp 0 (uos:getpid)))
	    ;; (cerror "Keep going" "Your breakpoint, sir?")
	    (if background
		(setf (job-status job) :running
		      (lish-last-background-job *shell*) job)
		(progn
		  ;; Wait for it...
		  (multiple-value-setq (result status)
		    (nos:wait-and-chill pid))
		  (handle-job-change job result status :foreground t))))))
    (values (or result '(0)) result-stream)))

;; This ends up calling one of the following to do the actual work:
;;   do-system-command , if it's an external command
;;   call-parenless    , if it's a function
;;   call-thing        , if it's a Lish command
;;   eval	       , if it's a object
;; This also directs alias expansion, and lisp sub-expression evaluation.

(defun shell-eval-command (sh expr context &key no-alias)
  "Evaluate a shell expression that is a command.
If the first word is an alias, expand the alias and re-evaluate.
If the first word is a system that can be loaded, load it and try to call it
as a lish command. This is vaugely like autoload.
If the first word is a lish command, call it.
If the first word is an existing directory and the auto-cd option is set, try
to change to it.
If the first word is an executable file in the system path, try to execute it.
If the first word is a symbol bound to a function, call it with the arguments,
which are read like lisp code. This is like a ‘parenless’ function call.
Otherwise just try to execute it with the system command executor, which will
probably fail, but perhaps in similar way to other shells."
  (let* (;(words (shell-expr-words expr))
	 (cmd (word-word (nth-expr-word 0 expr)))
	 (command (get-command cmd))
	 (alias (gethash cmd (lish-aliases sh)))
	 (expanded-expr (lisp-exp-eval expr))
	 result result-stream)
    ;; These are in order of precedence, so:
    ;;  aliases, lisp path, commands, system path
    (flet ((sys-cmd ()
	     "Do a system command."
	     (run-hooks *pre-command-hook* cmd :system-command)
	     (setf (values result result-stream)
		   (do-system-command expanded-expr context))
	     (run-hooks *post-command-hook* cmd :system-command)
	     (when (not result)
	       (format t "Command failed.~%"))
	     (force-output)	   ; @@@ is this really a good place for this?
	     (values result result-stream nil))
	   (rest-of-the-line (expr)
	     "Return the rest of the line after the first word."
	     (if (> (length (shell-expr-words expr)) 1)
		 (shell-words-to-string (rest (shell-expr-words expr)))
		 ""))
	   (literal-rest-of-the-line (expr)
	     (if (> (length (shell-expr-words expr)) 1)
		 (shell-words-to-string (rest (shell-expr-words expr))
					:literal-line (shell-expr-line expr))
		 ""))
	   (run-fun (func line)
	     "Apply the func to the line, and return the proper values."
	     (run-hooks *pre-command-hook* cmd :function)
	     (values-list
	      (let ((vals (multiple-value-list
			   (call-thing func '() context line))))
		(run-hooks *post-command-hook* cmd :function)
		vals))))
      (cond
	;; Alias
	((and alias (not no-alias))
	 ;; re-read and re-eval the line with the alias expanded
	 (shell-eval (expand-alias alias expanded-expr)
		     :context context
		     :no-expansions t))
	;; Lish command
	((typep command 'internal-command)
	 (run-hooks *pre-command-hook* cmd :command)
	 (multiple-value-prog1
	     (call-thing command (subseq (shell-expr-words expanded-expr) 1)
			 context)
	   (run-hooks *post-command-hook* cmd :command)))
	;; external command
	((typep command 'external-command)
	 (run-hooks *pre-command-hook* cmd :command)
	 ;; (multiple-value-prog1
	 ;;     (call-thing command (subseq expanded-words 1) context)
	 ;;   (run-hooks *post-command-hook* cmd :command)))
	 (sys-cmd))
	((typep command 'autoloaded-command)
	 (setf command (load-lisp-command-from
			(command-name command)
			(command-load-from command)
			:silent (lish-autoload-quietly sh)))
	 (run-hooks *pre-command-hook* cmd :command)
	 (multiple-value-prog1
	     (call-thing command (subseq (shell-expr-words expanded-expr) 1)
			 context)
	   (run-hooks *post-command-hook* cmd :command)))
	((functionp cmd)
	 ;; (format t "CHOWZA ~s~%" (rest-of-the-line expr))
	 (run-fun cmd (rest-of-the-line expr)))
	((and (symbolp cmd) (fboundp cmd))
	 ;; (format t "FLEOOP ~s~%" (rest-of-the-line expr))
	 (run-fun (symbol-function cmd) (rest-of-the-line expr)))
	;; Autoload
	;; @@@ perhaps we should cache since it seems dumb to check each time
	;; for things we already know are a system command?
	((and (lish-autoload-from-asdf sh)
	      (in-lisp-path cmd)
	      (setf command (load-lisp-command
			     cmd :silent (lish-autoload-quietly sh))))
	 ;; now try it as a command
	 (run-hooks *pre-command-hook* cmd :command)
	 (multiple-value-prog1
	     (call-thing command (subseq (shell-expr-words expanded-expr) 1)
			 context)
	   (run-hooks *post-command-hook* cmd :command)))
	((stringp cmd)
	 ;; If we can find a command in the path, try it first.
	 (cond
	   ((get-command-path cmd)
	    (sys-cmd))
	   ((and (lish-auto-cd sh) (directory-p cmd))
	    (when (> (length (shell-expr-words expr)) 1)
	      (cerror "Ignore the rest of the line."
		      "Arguments aren't allowed after the auto-cd directory."))
	    (change-directory cmd))
	   (t ;; Otherwise try a parenless Lisp line.
	     (multiple-value-bind (symb pos)
		 (read-from-string (shell-expr-line expr) nil nil)
	       (declare (ignore pos))
	       (if (and (symbolp symb) (fboundp symb))
		   (if (macro-function symb)
		       (progn
			 (shell-eval (cons symb
					   (read-parenless-args
					    (rest-of-the-line expr)))
				     :context context))
		       (progn
			 (run-fun (symbol-function symb)
				  ;;(subseq (shell-expr-line expr) pos)
				  ;;(rest-of-the-line expr)
				  (literal-rest-of-the-line expr)
				  )))
		   ;; Just try a system command anyway, which will likely fail.
		   (sys-cmd))))))
	(t ;; Some other type, just return it, like it's self evaluating.
	 ;;(values (multiple-value-list (eval cmd)) nil t))))))
	 ;; (values (multiple-value-list
	 ;; 	  ;;(with-first-value-to-output
	 ;; 	  (call-thing (car cmd) (cdr cmd) context))
	 ;; 	 nil t)
	 ;; The first word was probably a Lisp expression, evaluating or not.
	 ;; Just treat the rest of the words as potentially Lisp expressions.
	 (let ((first-word (elt (shell-expr-words expr) 0)))
	   (if (and (shell-word-p first-word)
		    (shell-word-eval first-word))
	       (call-thing cmd nil context)
	       (values (list (eval cmd)) nil t)))
	 ;; @@@ How can we evaluate the words after the first and return all
	 ;; their possibly multiple values?
	 ;; (loop :for w :in (shell-expr-words expr)
	 ;;      (when (shell-word-eval w)
	 ;; 	(call-thing (word-word w) nil context)
	 ;; 	;; @@@@@
	 )))))

;; This does normal expansions, sets up piping and redirections and
;; eventually calls shell-eval-command.

(defun shell-eval (expr &key no-expansions (shell *shell*) (context *context*))
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
    (with-slots (in-pipe out-pipe environment flipped-io pipe-plus background)
	*context*
      (setf flipped-io nil)
      (macrolet
	  ((eval-compound (test new-pipe)
	     "Do a compound command. TEST determines whether the next~
	      part of the command gets done. NEW-PIPE is true to make a~
	      new pipe."
	     `(multiple-value-bind (vals out-stream show-vals)
		  (let ((*accepts*
			 (get-accepts (nth-expr-word 1 expr))))
		    (shell-eval (second first-word)
				:context (make-context
					  :in-pipe in-pipe
					  :out-pipe ,new-pipe
					  :environment environment
					  :flipped-io flipped-io
					  :pipe-plus pipe-plus)))
					  ;;:flipped-io (not flipped-io))))
		(declare (ignore show-vals) (ignorable vals))
		(when ,test
		  (with-package *lish-user-package*
		    (shell-eval
		     (make-shell-expr
		      :words       (cdr (shell-expr-words expr))
		      ;; @@@ perhaps we should retain original,
		      ;; since indexes not adjusted?
		      :line (format nil "~{~a ~}"
				    (cdr (shell-expr-words expr))))
		     ;; @@@ is this right with out-stream?
		     :context
		     (if out-stream
			 (modified-context *context* :in-pipe out-stream)
			 *context*)
		     :no-expansions no-expansions))))))
	;; unpack an eval-able lisp expr
	(when (and (shell-expr-p expr)
		   (= (length (shell-expr-words expr)) 1)
		   (shell-word-eval (first (shell-expr-words expr))))
	  (setf expr (shell-word-word (first (shell-expr-words expr)))))
	;; @@@ But what if there's multiple eval-able lisp exprs?
	(cond
	  ((not (shell-expr-p expr))
	   ;; A full Lisp expression all by itself
	   (cond
	     ((and (consp expr) expr
		   (and (symbolp (car expr)) (fboundp (car expr))))
	      ;; Give precedence to functions
	      ;; (with-package *lish-user-package*
	      ;; 	(values (multiple-value-list (eval expr)) nil t)))
	      (with-package *lish-user-package*
		(when (not flipped-io)
		    (setf *input* *output*
			  *output* nil
			  flipped-io t))
		(multiple-value-bind (result-values output show-p)
		    (call-thing expr nil *context*)
		  (setf (output) (car result-values))
		  (values result-values output show-p))))
	     ((consp expr)
	      (case (command-type shell (string-downcase (car expr)))
		((:command :file)
		 ;; Try to do a system command in s-exp syntax
		 (shell-eval
		  (shell-read
		   (join-by-string
		    (cons
		     (string-downcase (car expr))
		     (with-package *lish-user-package*
		       (let ((*print-case* :downcase)
			     (*print-escape* nil))
			 (mapcar (_ (if (consp _)
					(s+ #\" (prin1-to-string _) #\")
					(prin1-to-string _)))
				 (cdr expr)))))
		    #\space))
		  :context context))
		(t
		 (with-package *lish-user-package*
		   (values (multiple-value-list (eval expr)) nil t)))))
	     ((stringp expr)
	      (shell-eval (shell-read expr) :context context))
	     (t
	      (with-package *lish-user-package*
		(values (multiple-value-list (eval expr)) nil t)))))
	  ((zerop (length (shell-expr-words expr)))
	   ;; Quick return when no words
	   (return-from shell-eval (values nil nil nil)))
	  ((and (listp (setf first-word (nth-expr-word 0 expr)))
		(keywordp (first first-word)))
	   ;; First word is a list with a keyword, so it's a compound command.
	   (unless no-expansions
	     (do-expansions expr))
	   (case (first first-word)
	     ((:pipe :pipe-plus)
	      (when (not flipped-io)
		(setf *input* *output*
		      *output* nil
		      *pipe-plus* (eq (first first-word) :pipe-plus)
		      (context-pipe-plus *context*) (eq (first first-word) :pipe-plus)
		      flipped-io t))
	      (setf (values vals out-stream show-vals)
		    (eval-compound (successful vals) t))
	      (values vals out-stream show-vals))
	     (:and      (eval-compound (successful vals) nil))
	     (:or       (eval-compound (not (successful vals)) nil))
	     (:sequence (eval-compound t nil))
	     (:redirect-to
	      (run-with-output-to
	       (word-word (elt (shell-expr-words expr) 1))
	       (elt first-word 1)))
	     (:append-to
	      (run-with-output-to
	       (word-word (elt (shell-expr-words expr) 1))
	       (elt first-word 1) :append t))
	     (:redirect-from
	      (run-with-input-from
	       (word-word (elt (shell-expr-words expr) 1))
	       (elt first-word 1)))
	     (t
	      (error "Unknown compound command type."))))
	  (t
	   ;; The first word is not a list, so it's a ‘simple’ command.
	   (unless no-expansions
	     (do-expansions expr))
	   ;; A stupid way to do backgrounding:
	   (when (equal (word-word (car (last (shell-expr-words expr)))) "&")
	     (setf background t
		   (shell-expr-words expr) (nbutlast (shell-expr-words expr))))
	   (with-package *lish-user-package*
	     ;; accepts is :unspecified because we're last in the
	     ;; pipeline.
	     (when (not flipped-io)
	       (setf *input* (output)
		     *output* nil
		     flipped-io t))
	     (setf (values vals out-stream show-vals)
		   (shell-eval-command shell expr *context*
				       :no-alias no-expansions))
	     (values vals out-stream show-vals))))))))

(defun load-file (file)
  "Load a lish syntax file."
  (let ((*load-pathname* (pathname file))
	(line-number 1)
	expression-start-line
	expr)
    (with-open-file (stream file :direction :input)
      (with-package *lish-user-package*
	(labels ((read-a-line ()
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
	     (shell-eval expr))
	  (when (eql expr *continue-symbol*)
	    (error "End of file in expression. Probably starting at line ~a."
		   expression-start-line)))))))

(defun pick-an-rc-file ()
  (loop :for file :in (list *lishrc*
			    (path-append (config-dir "lish") "lishrc")
			    *default-lishrc*)
     :do
     (when file
       (let ((expanded-file (expand-variables file)))
	 (when (and expanded-file (probe-file expanded-file))
	   (return expanded-file))))))
  
(defun load-rc-file (init-file)
  "Load the users start up (a.k.a. run commands) file, if it exists."
  (when (and init-file (nos:file-exists init-file))
    (let ((*lish-user-package* (find-package :lish-user)))
      (load-file init-file))))

(defun push-directory-ring (dir)
  (if (> (length *directory-ring*) 32)
      (pop *directory-ring*))
  (setf *directory-ring* (append *directory-ring* (list dir))))

(defun find-directory-in-ring (reg)
  (let ((res (find reg *directory-ring* :test #'cl-ppcre:scan)))
    (or res (nos:current-directory))))

(defun find-id (shell)
  "Return the lowest ID that isn't in use."
  (loop :for i = 1 :then (1+ i)
     :if (not (position i (lish-jobs shell)
			:key #'job-id))
     :return i
     :if (> i 100000)
     :do (error "Something probably went wrong with finding a job ID.")))

(defun add-job (name command-line thing &key (status :running))
  "Add a job with the given NAME and COMMAND-LINE. THING is either an integer
process ID or a resume function designator. STATUS defaults to :RUNNING."
  (let (job)
    (etypecase thing
      (integer
       (setf job (make-instance 'system-job
				:id (find-id *shell*)
				:name name
				:command-line command-line
				:status status
				:pid thing
				:process-group thing)))
      (process-handle
       (setf job (make-instance 'system-job
				:id (find-id *shell*)
				:name name
				:command-line command-line
				:status status
				;; @@@ bullcrap workaround
				:pid (process-handle-value thing)
				:process-group (process-handle-value thing))))
      ((or symbol function)
       (setf job (make-instance 'lisp-job
			       :id (find-id *shell*)
			       :name name
			       :command-line command-line
			       :status status
			       :resume-function thing)))
      (bt:thread
       (setf job (make-instance 'thread-job
				:id (find-id *shell*)
				:name name
				:command-line command-line
				:status status
				:thread thing))))
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
  (setf (lish-jobs *shell*)
	(typecase job
	  (integer
	   (delete job (lish-jobs *shell*) :test #'= :key #'job-id))
	  (job
	   (delete job (lish-jobs *shell*) :test #'eq)))))

(defun check-all-job-status (sh)
  "Check the status of all jobs and perform appropriate actions."
  (mapc (_ (check-job-status sh _)) *job-types*))

(defvar *shell-non-word-chars*
  #(#\space #\tab #\newline #\linefeed #\page #\return
    #\( #\) #\[ #\] #\: #\; #\/ #\" #\' #\\ #\# #\, #\` #\| #\.
    #\- #\$ #\~ #\! #\&)
  "Characters that are not considered to be part of a word in the shell.")

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
  last-command
  (error-count 0))

(cffi:defcallback sigint-handler :void ((signal-number :int))
  (declare (ignore signal-number))
  (format t "[Interrupt]~%") (finish-output)
  ;; I'm scared of this.
  ;; (invoke-restart (find-restart 'abort))
  (throw 'interactive-interrupt t)
  )

(defun set-signals ()
  "Make sure signal handlers are set up for shell reading. Return a list of
 (signal . action) to be reset later."
  #+unix
  (let (result)
    (progn
      ;; Ignore quit
      (push (cons uos:+SIGQUIT+ (uos:signal-action uos:+SIGQUIT+)) result)
      (uos:set-signal-action uos:+SIGQUIT+ :ignore)
      ;; Ignore suspend
      (push (cons uos:+SIGTSTP+ (uos:signal-action uos:+SIGTSTP+)) result)
      ;;(uos:set-signal-action uos:+SIGTSTP+ :ignore)
      (uos:set-signal-action uos:+SIGTSTP+ 'sigtstp-handler)
      ;; Handle interrupt
      ;; #-sbcl
      ;; (progn
      ;;   (push (cons uos:+SIGINT+ (uos:signal-action uos:+SIGINT+)) result)
      ;;   (uos:set-signal-action uos:+SIGINT+ 'sigint-handler))
      )
    result)
  #-unix t)

(defun restore-signals (actions)
  "Restore (or really just set) the signal actions in ACTIONS."
  #-unix
  (declare (ignore actions))
  #+unix
  (loop :for (sig . act) :in actions
     :do (uos:set-signal-action sig act)))

(defun lish-read (sh state)
  "Read a string with the line editor and convert it shell expressions."
  (with-slots ((str string) (pre-str prefix-string) this-command last-command)
      state
    (let (saved-signals)
      (unwind-protect
	   (progn
	     ;;(break)
	     (finish-output)
	     (tt-finish-output)
	     (setf saved-signals (set-signals)
		   str (rl
			:eof-value *real-eof-symbol*
			:quit-value *quit-symbol*
			:history-context :lish
			:editor (lish-editor sh)
			:accept-does-newline nil
			:re-edit (when pre-str
				   (setf pre-str nil)
				   t)
			:prompt
			;; (if pre-str
			;;     (lish-sub-prompt sh)
			    (safety-prompt sh)
			:right-prompt (safety-prompt sh :right)
			    )))
	(when saved-signals
	  (restore-signals saved-signals))))
    (cond
      ((and (stringp str) (equal 0 (length str))) *empty-symbol*)
      ((equal str *real-eof-symbol*)		  *real-eof-symbol*)
      ((equal str *quit-symbol*)	  	  *quit-symbol*)
      (t
       (setf ! last-command
	     last-command (copy-seq this-command))
       ;; This is THE read of the REPL.
       (shell-read (setf this-command
			 (if pre-str
			     (s+ pre-str #\newline str)
			     str)))))))

(defun maybe-save-values (vals show-vals)
  "Save result values in the history if the option is on."
  (when (get-option *shell* 'history-save-values)
    (if (and show-vals vals (olength-at-least-p 1 vals))
	(setf (getf (rl:history-entry-extra
		     (dl-list:dl-content
		      (rl::history-current-get)))
		    :values)
	      (copy-list vals))
	(when *output*
	  (setf (getf (rl:history-entry-extra
		       (dl-list:dl-content
			(rl::history-current-get)))
		      :values)
		(list *output*))))))

(defun lish-print (values)
  "Print the results of an evaluation. VALUES are a list of values to print."
  (loop :with len = (length values) :and i = 0
     :for v :in values
     :do
     ;; (format t "~s" v)
     (format t "~w" v)
     (if (and (> len 1) (< i (- len 1)))
	 (format t " ;~%"))
     (incf i)
     :finally (format t "~&")))

(defun lish-eval (sh expr state)
  "Evaluate the shell expressions in EXPR."
  (declare (ignore sh))
  (with-slots ((str string) (pre-str prefix-string)) state
    (cond
      ((eq expr *continue-symbol*)
       (if (stringp pre-str)
	   (setf pre-str (format nil "~a~%~a" pre-str str))
	   (setf pre-str (format nil "~a" str))))
      ((eq expr *empty-symbol*)
       ;; do nothing
       (format t "~%"))			; <<<<
      ((eq expr *error-symbol*)
       ;; do nothing
       (break))
      (t
       (setf pre-str nil
	     *input* nil
	     *output* nil)
       (format t "~%")			; <<<<
       (force-output)
       (when (catch 'interactive-interrupt
	       (multiple-value-bind (vals stream show-vals)
		   #| @@@ This really fails.
		   #+(and unix (not sbcl))
		   ;; (uos:with-signal-handlers ((uos:+sigint+ . sigint-handler))
		   ;;   (format t "Howdy pardner.~%")
		   ;;   (shell-eval expr :context nil))
		   (progn
		   (uos:set-signal-action uos:+sigint+ 'sigint-handler)
		   (shell-eval expr :context nil)
		   (uos:set-signal-action uos:+sigint+ :default))
		   #-(and unix (not sbcl))
		   |#
		   (shell-eval expr :context nil)
		 (declare (ignore stream))
		 (let ((vals-list (if (listp vals) vals (list vals))))
		   (setf /// //
			 // /
			 / vals-list
			 *** **
			 ** *
			 * (car vals-list))
		   (maybe-save-values vals show-vals)
		   (when show-vals
		     ;; Maybe save expression values in history.
		     (lish-print vals)))
		 nil))
	 ;; Got an intterrupt, so stop reading this multi-line expression.
	 (format t ">>>> Control-C <<<<~%") ;; does this ever happen?
	 (set pre-str nil))))))

(defun confirm-quit ()
  (if (lish-jobs *shell*)
      (progn
	(format t "There are stopped jobs. ")
	(confirm "quit the shell"))
      t))

(defun ensure-theme ()
  "Make sure a theme is set."
  (when (not theme:*theme*)
    (setf theme:*theme* (theme:default-theme))))

(defun save-history (sh &key update)
  "Save the history."
  (handler-bind
      ((serious-condition
	#'(lambda (c)
	    (if (lish-debug *shell*)
		(invoke-debugger c)
		(format t "Saving history failed. ~
                           Turn on debug if you want.~%~a~%" c)))))
    (history-store-save (lish-history-store sh) (history-style sh)
			:update update)))


(defun load-history (sh &key update)
  (with-simple-restart (continue
			"Keep going even though history failed to load.")
    (handler-bind
	((serious-condition
	  #'(lambda (c)
	      (if (lish-debug *shell*)
		  (invoke-debugger c)
		  (progn
		    (format t "Loading history failed. Turn on debug if you ~
                             want.~%~a~%" c)
		    (continue))))))
      (history-store-load (lish-history-store sh) (history-style sh)
			  :update update))))

(defun init-history (sh)
  "Create the history store and load the history from it."
  (rl::history-init)
  (setf (lish-history-store sh)
	(make-instance (ecase (history-format sh)
			 (:text-file 'text-history-store)
			 (:database 'db-history-store))))
  (history-store-start (lish-history-store sh) (history-style sh)))

;; @@@ Since there's a shared history for all shells in the same image, there
;; should be a shared history store for the image too, which probably should
;; be saved at least when a shell exits, but for sure when the last shell
;; exits.
;;
;; For now we don't do any saving for shells other than the first.

(defun start-history (sh)
  (when (not (get-history)) ;; @@@
    (init-history sh)
    (load-history sh)))

(defun finish-history (sh)
  "Save the history finish using the history store."
  (when (lish-history-store sh) ;; @@@ 
    (save-history sh)
    (history-store-done (lish-history-store sh) (history-style sh))))

(defmacro with-error-handling ((state) &body body)
  (with-names (results just-print-the-error condition)
    `(let* (,condition
	    (,results
	     ;; We have to be careful to preserve the values
	     (multiple-value-list
	      (catch ',just-print-the-error
		(handler-bind
		    (#+sbcl (sb-ext::step-condition 'repple-stepper)
		     #+sbcl ;; So we can do something on ^C
		     (sb-sys:interactive-interrupt
		      #'(lambda (c)
			  ;; Stop reading this multi-line expression
			  (setf (read-state-prefix-string ,state) nil)
			  (if (lish-debug *shell*)
			      (invoke-debugger c)
			      (progn
				(format t "~%") (finish-output)
				(invoke-restart (find-restart 'abort))))))
		     (unknown-command-error
		      #'(lambda (c)
			  (cond
			    ((lish-debug *shell*)
			     (if *unknown-command-hook*
				 (progn
				   (with-simple-restart
				       (continue
					"~a"
					(or
					 (documentation *unknown-command-hook*
							'function)
					 "Continue the *unknown-command-hook*"))
				     (invoke-debugger c))
				   (funcall *unknown-command-hook*
					    *shell* c))
				 (invoke-debugger c)))
			    (*unknown-command-hook*
			     (if (funcall *unknown-command-hook* *shell* c)
				 (continue)
				 (signal c)))
			    (t
			     (setf ,condition c)
			     (throw ',just-print-the-error
			       ',just-print-the-error)))))
		     ;; Normal error handling
		     (serious-condition
		      #'(lambda (c)
			  (if (lish-debug *shell*)
			      (invoke-debugger c)
			      ;; We can't just print the error here because the
			      ;; terminal could be screwed up, in a way which
			      ;; can make the conditional newline fail, so we
			      ;; throw out, so that hopefully the terminal will
			      ;; be reset by unwind code and then print and
			      ;; abort.
			      (progn
				(setf ,condition c)
				(throw ',just-print-the-error
				  ',just-print-the-error))))))
		  ,@body)))))
       (if (eq (car ,results) ',just-print-the-error)
	   (progn
	     ;; We can't leave this error counting to the debugger, since
	     ;; we never get into it when debugging is off.
	     (incf (read-state-error-count ,state))
	     (when (> (read-state-error-count ,state) 10)
	       (format t "Too many errors!~%")
	       ;; This will mostly happen if we get an error in the shell or
	       ;; line editor code. Errors from commands probably won't recur.
	       (break))
	     (format t "~&~a~&" ,condition)
	     (finish-output)			; semi-dubious
	     (invoke-restart 'abort))
	   (values-list ,results)))))

;; These with-* macros break shell initialization up into pieces so we can
;; do non-interactive commands, especially the various ! functions, without
;; repeating ourselves, and so we can call the ! functions when not already in
; an interactive shell.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-new-shell ((&rest args) &body body)
    "Evaluate the BODY with *SHELL* bound to anew shell instance."
    `(let* ((*shell* (make-instance 'shell ,@args))
	    (*history-context* :lish)
	    (*lish-level* (if *lish-level*
			      (funcall #'1+ (symbol-value '*lish-level*))
			      0))
	    (saved-sigs (job-control-signals)))
       ,@body))

  (defmacro with-shell-command (() &body body)
    "Evaluate the BODY as a non-interactive shell command."
    (with-names (result)
    `(let (,result)
       (catch 'interactive-interrupt
	 (ensure-theme)
	 (unwind-protect
	      (progn
		(start-job-control)
		(run-hooks *enter-shell-hook*)
		(setf ,result (progn ,@body)))
	   (stop-job-control saved-sigs)
	   (run-hooks *exit-shell-hook*)))
       (if (lish-exit-values *shell*)
	   (values-list (lish-exit-values *shell*))
	   ,result))))

  (defmacro with-shell (() &body body)
    "Evaluate the body as a non-interactive shell command, making a new shell
if we aren't already inside one."
    `(if *shell*
	 ;; @@@ How can we avoid the code duplication? I don't think the thunk
	 ;; trick will work here?
	 (progn ,@body)
	 (with-new-shell ()
	   (with-shell-command ()
	     ,@body)))))

(defun lish (&key debug terminal-name
	       ;;(terminal-type (pick-a-terminal-type))
	       terminal-type
	       ;;(init-file (or *lishrc* *default-lishrc*))
	       (init-file (pick-an-rc-file))
	       command)
  "Lish is a LIsp SHell.
Type the “help” command for more documentation.
Arguments:
  DEBUG         - True to turn on entering the debugger on errors.
  TERMINAL-NAME - Device name of the terminal.
  TERMINAL-TYPE - Type of terminal to read from. Defaults from
                   pick-a-terminal-type and so *default-terminal-type*.
  INIT-FILE     - File to load on startup or *default-lishrc* if not given.
  COMMAND       - A command to evaluate and exit."
  ;; (let* ((*shell* (make-instance 'shell :debug debug))
  ;; 	 (sh *shell*)		; shorthand
  ;; 	 (state (make-read-state))
  ;; 	 (*history-context* :lish)
  ;; 	 (*lish-level* (if *lish-level*
  ;; 			   (funcall #'1+ (symbol-value '*lish-level*))
  ;; 			   0))
  ;; 	 (*lishrc* init-file) ;; So it's inherited by sub-shells.
  ;; 	 ! ;-) !
  ;; 	 (saved-sigs (job-control-signals))
  ;; 	 ;; (old-terminal *terminal*)
  ;; 	 )
  (with-new-shell (:debug debug)
    (let* ((sh *shell*)		; shorthand
	   (state (make-read-state))
	   (*lishrc* init-file) ;; So it's inherited by sub-shells.
	   !)
      (setf (lish-debug *shell*) debug)	; @@@ the arg to make-instance doesn't

    ;; Make the user package if it doesn't exist.
    (when (not (find-package :lish-user))
      (setf *lish-user-package* (make-user-package)))

    ;; Don't do the wacky package updating in other packages.
    (when (eq *lish-user-package* (find-package :lish-user))
      (update-user-package))

    ;; Set the recursion level for system processes.
    (setf (nos:environment-variable "LISH_LEVEL")
	  (format nil "~d" lish::*lish-level*))

    ;; Load the startup file.
    (load-rc-file init-file)

    ;; Perhaps do a single command and exit.
    (when command
      ;; (let (result)
      ;; 	(catch 'interactive-interrupt
      ;; 	  (ensure-theme)
      ;; 	  (start-job-control)
      ;; 	  (run-hooks *enter-shell-hook*)
      ;; 	  (setf result (lish-eval sh
      ;; 				  (typecase command
      ;; 				    (list command)
      ;; 				    (t (shell-read command)))
      ;; 				  (make-read-state)))
      ;; 	  (stop-job-control saved-sigs)
      ;; 	  (run-hooks *exit-shell-hook*))
      ;; 	(return-from lish (if (lish-exit-values sh)
      ;; 			      (values-list (lish-exit-values sh))
      ;; 			      result))))
      (return-from lish
	(with-shell-command ()
	  (lish-eval sh (typecase command
			  (list command)
			  (t (shell-read command)))
		     (make-read-state)))))

    ;; Figure out the terminal type.
    (setf terminal-type (or terminal-type
			    (and *terminal*
				 (find-terminal-type-for-class
				  (type-of *terminal*)))
			    (pick-a-terminal-type)))
    (with-terminal (terminal-type *terminal* :device-name terminal-name
				  :start-at-current-line t)
      (let (#|(*standard-output* *terminal*) not ready for this |#)
	(setf (tt-input-mode) :line)

	(ensure-theme)

	;; Make a customized line editor
	(setf (lish-editor sh)
	      (make-instance 'rl:line-editor
			     :non-word-chars *shell-non-word-chars*
			     :completion-func #'shell-complete
			     :history-context *history-context*
			     :terminal-device-name terminal-name
			     :auto-suggest-p (lish-auto-suggest sh)
			     :local-keymap (lish-keymap sh)
			     :prompt-func nil
			     :filter-hook `(colorize)
			     :partial-line-indicator
			     (lish-partial-line-indicator *shell*)))

	(start-history sh)

	(unwind-protect
	  (progn
	    (start-job-control)
	    (run-hooks *enter-shell-hook*)
	    (when (not (eq :lish-quick-exit (catch :lish-quick-exit
	      (loop
	       :named pippy
	       :with expr = nil
	       :and lvl = *lish-level*
	       :and eof-count = 0
	       :and retry = nil
	       :if (lish-exit-flag sh)
		 :if (confirm-quit)
		   :return (values-list (lish-exit-values sh))
		 :else
		   :do (setf (lish-exit-flag sh) nil)
		 :end
	       :end
	       :do
	       (restart-case
		 (with-error-handling (state)
		   (check-all-job-status sh)
		   (if retry
		       (setf retry nil)
		       (setf expr (lish-read sh state)))
		   (when (and (eq expr *real-eof-symbol*) (confirm-quit))
		     (return-from pippy expr))
		   (if (eq expr *quit-symbol*)
		       (if (and (not (lish-ignore-eof sh)) (confirm-quit))
			   (return-from pippy expr)
			   (progn
			     (when (numberp (lish-ignore-eof sh))
			       (if (< eof-count (lish-ignore-eof sh))
				   (incf eof-count)
				   (if (confirm-quit)
				       (return-from pippy expr)
				       (setf eof-count 0))))
			     (let ((remain
				    (1+ (- (lish-ignore-eof sh) eof-count))))
			       (format t "Type 'exit'~:[ or ~a ~d more time~p~
					  ~;~^~] to exit the shell.~%"
				       (zerop eof-count)
				       (char-util:nice-char
					(rl::last-event (lish-editor sh))
					:caret t)
				       remain remain))))
		       (progn
			 (setf eof-count 0)
			 (lish-eval sh expr state)))
		   (setf (read-state-error-count state) 0))
		 (abort ()
		   :report
		   (lambda (stream)
		     (format stream
			     "Return to Lish ~:[~;TOP ~]level~:[~; ~d~]."
			     (= lvl 0) (/= lvl 0) lvl))
		   nil)
		 (retry ()
		   :report
		   (lambda (stream)
		     (format stream
			     "Retry Lish command ~:[~;TOP ~]level~:[~; ~d~]."
			     (= lvl 0) (/= lvl 0) lvl))
		   (setf retry t)
		   nil))))))))
	  (stop-job-control saved-sigs))
	;;(save-command-stats)
	(run-hooks *exit-shell-hook*)))

    (finish-history sh)

    (when (lish-exit-flag sh)
      (return-from lish (when (lish-exit-values sh)
			  (values-list (lish-exit-values sh)))))
    (format t "*EOF*~%")
    ;; Well, let's hope that this will clear the EOF on *standard-input*
    (clear-input *standard-input*))))

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

(defcommand lish
  ((command string :short-arg #\c :help "Command to execute.")
   (init-file pathname :short-arg #\i
    :default *default-lishrc* :use-supplied-flag t
    :help "File to execute on startup.")
   (no-init-file boolean :short-arg #\n :help "Don't load any startup file.")
   (greeting boolean :short-arg #\g :default t
    :help "True to print a greeting.")
   (terminal-type choice :short-arg #\t
    :choice-func (lambda () (mapcar #'string-downcase (terminal-types)))
    :help "Terminal type.")
   (debug boolean :short-arg #\d :help "True to turn on debugging."))
  :keys-as args
  "Lisp Shell"
  (when (and greeting (not command))
    (format t "Welcome to ~a ~a~%" *shell-name* *version*))
  (remf args :greeting)
  (when (not init-file-supplied-p)
    (setf init-file (when (not no-init-file) (pick-an-rc-file))))
  (when (getf args :no-init-file)
    (remf args :no-init-file))
  (apply #'lish args))

(defun wordify-list (word-list)
  "Return a list of shell words that is like the strings in word-list separated
by spaces."
  (loop :with pos = 0
     :for w :in word-list
     :collect (make-shell-word :word w :start pos :end (+ pos (length w)))
     :do (incf pos (1+ (length w)))))

(defvar *saved-default-external-format* nil)

(defun shell-toplevel ()
  "For being invoked as a standalone shell."
  (setf *standalone* t)
  ;;(format t "Welcome to ~a ~a~%" *shell-name* *version*)
  ;;(format t "Yo yo! ~s~%" (nos:lisp-args)) (finish-output)
  ;;(trace lish)
  ;;(trace !lish)
  (let* ((level-string (nos:environment-variable "LISH_LEVEL"))
	 ;;(args-expr (when (cdr (nos:lisp-args))
	 ;;  (shell-read (join-by-string (cdr (nos:lisp-args)) #\space
	 ;;(args-expr (wordify-list (nos:lisp-args)))
	 )
    (when level-string
      (setf *lish-level* (parse-integer level-string)))

    ;; @@@ Of course this is wrong. We really get it from LANG or LC_CYTPE, or
    ;; whatever it is on Windows or whatever O/S.
    #+sbcl (setf sb-impl::*default-external-format*
		 (or *saved-default-external-format* :utf-8))
    ;; (with-new-terminal ()
      (if (cdr (nos:lisp-args))
	  (apply #'!lish
		 `(,@(posix-to-lisp-args (get-command "lish")
					 (wordify-list (cdr (nos:lisp-args))))
		     :greeting t :terminal-type :crunch))
	  (!lish :greeting t :terminal-type :crunch))
      ;; )
    (nos:exit-lisp)))

(defun make-standalone (&key (name "lish") (smaller t))
  "Make a lish executable."
  ;; (update-version)
  (let (options)
    #+sbcl
    (progn
      ;; So that the saved image can start with a fresh external format.
      (setf *saved-default-external-format*
	    sb-impl::*default-external-format*)
      ;; In case we were built with the debugger turned off.
      ;; @@@ Maybe this should be in the top level function??
      (sb-ext:enable-debugger)
      ;; Turn on core compression if it's available
      (when (and smaller (has-feature :sb-core-compression))
	(setf options '(:compression t))))
    ;; Make sure ASDF is cleared.
    (asdf:clear-configuration)
    (apply #'save-image-and-exit name :initial-function #'lish:shell-toplevel
	   options)))

;; So we can conditionalize adding of lish commands in other packages.
(d-add-feature :lish)

;; EOF
