;;;
;;; lish.lisp - A Lisp shell.
;;;

;; This file contains the basic REPL and dispatch, and some other odds and ends.

(in-package :lish)

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
			   (in-pipe     nil in-pipe-p)
			   (out-pipe    nil out-pipe-p)
			   (environment nil environment-p)
			   (flipped-io  nil flipped-io-p)
			   (pipe-plus   nil pipe-plus-p)
			   (pipe-dot    nil pipe-dot-p)
			   (background  nil background-p))
  "Return a new context based on CONTEXT, with the given slots."
  (if (not context)
      (make-context 
       :in-pipe     in-pipe
       :out-pipe    out-pipe
       :environment environment
       :flipped-io  flipped-io
       :pipe-plus   pipe-plus
       :pipe-dot    pipe-dot
       :background  background)
      (let ((c (copy-structure context)))
	(when in-pipe-p     (setf (context-in-pipe     c) in-pipe))
	(when out-pipe-p    (setf (context-out-pipe    c) out-pipe))
	(when environment-p (setf (context-environment c) environment))
	(when flipped-io-p  (setf (context-flipped-io  c) flipped-io))
	(when pipe-plus-p   (setf (context-pipe-plus   c) pipe-plus))
	(when pipe-dot-p    (setf (context-pipe-dot    c) pipe-dot))
	(when background-p  (setf (context-background  c) background))
	c)))

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

(defun in-word (word position)
  "Return true if ‘position’ is in shell-word ‘word’."
  (and (>= position (shell-word-start word))
       (<= position (shell-word-end word))))

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


(defparameter *compound-expr-strings*
  '(:and           "&&"
    :or            "||"
    :sequence      "^"
    :redirect-to   ">"
    :append-to     ">>"
    :redirect-from "<"
    :pipe          "|"
    :pipe-plus     "|+"
    :pipe-dot      "|.")
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
		 ((or string fat-string) (princ (quotify w) stream))
		 ((or character fatchar) (princ w stream))
		 (cons
		  (let ((s (compound-tag-string (car w))))
		    (if (and s (shell-expr-p (second w)))
			(format stream
			 "~a ~a~a"
			 (shell-words-to-string
			  (shell-expr-words (second w)))
			 s
			 (shell-words-to-string (rest w)))
			(write w :stream stream :readably t :case :downcase))))
		 (t ;; @@@ is this reasonable?
		  (princ w stream))))
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
  (let ((alias (gethash command (shell-aliases *shell*)))
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
	       ;; (format t "is-like ~s ~s~%" x type)
	       (or (equal x type)
		   #+clisp (ignore-errors (subtypep x type))
		   ;; #-clisp (subtypep x type)
		   #-clisp (ignore-errors (subtypep x type))
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
      ((gethash command (shell-aliases sh))       :alias)
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

;; @@@ Maybe this would be better done by typep, but then we'd have to somehow
;; put aliaes, systems, and files, into the type system?
(defun maybe-a-command-p (expr)
  "Return true if ‘expr’ has a first symbol which might be a command."
  (and expr
       (symbolp (car expr))
       (not (member (command-type *shell* (string-downcase (car expr))
				  :already-known t)
		    '(:directory :function)))))

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
			(let ((curried
				(_ (with-input (_)
				     (apply func `(,@parenless-args ,_))))))
			  ;; @@@ maybe there's faster way to do this?
			  (apply #'omap curried (list *input*)))
			(apply func `(,@parenless-args ,*input*)))))
	      (progn
		(with-first-value-to-output
		    (if (context-pipe-plus *context*)
			(let ((wrapper
				(_ (with-input (_)
				     (funcall func _)))))
			  (omap wrapper *input*))
			(apply func (list *input*)))))))
	;; no *input* stuffing
	(with-first-value-to-output (apply func parenless-args)))))

(defmacro maybe-do-in-background ((bg-p name args) &body body)
  (with-names (thunk string-name args-val)
    `(flet ((,thunk () (progn ,@body)))
       (if (and ,bg-p bt:*supports-threads-p*)
	   (progn
	     (let ((,string-name (prin1-to-string ,name))
		   (,args-val ,args))
	       (setf (lish-last-background-job *shell*)
		     (add-job ,string-name
			      (or (and ,args-val
				       (shell-words-to-string ,args-val)) "")
			      (bt:make-thread #',thunk :name ,string-name)))))
	   (progn
	     (funcall #',thunk))))))

(defun eval-lisp-expr (expr)
  "A wrapper to eval that does the right thing for an invidual expression as an
element in a pipeline."
  (let ((- expr))
    (if (context-pipe-plus *context*)
      (let ((wrapper
	      (_ (with-input (_)
		   (eval expr)))))
	(omap wrapper *input*))
      (eval expr))))

(defun post-command (name type)
  "Things to do after a command."
  (run-hooks *post-command-hook* name type)
  (finish-output))

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
    (let ((command-p (typep thing 'base-command))
	  (bg (context-background context)))
      (labels ((runky (thing args)
		 (cond
		   ((typep thing 'autoloaded-command)
		    (let ((*context* context))
		      ;; The args can't be converted yet.
		      (maybe-do-in-background (bg (command-name thing) args)
		        (invoke-command thing args))))
		   (command-p
		    (let ((lisp-args (posix-to-lisp-args thing args))
			  (*context* context))
		      (maybe-do-in-background (bg (command-name thing) args)
		        (invoke-command thing lisp-args))))
		   (parenless
		    (call-parenless thing parenless context))
		   (t
		    (maybe-do-in-background (bg thing args)
		      (let ((- thing))
			(eval-lisp-expr thing)))))))
	(if out-pipe
	    (let ((out-str (make-stretchy-string 20)))
	      (values
	       ;; @@@ This is totally stupid
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
	 (alias (gethash cmd (shell-aliases sh)))
	 (expanded-expr (lisp-exp-eval expr))
	 result result-stream)
    ;; These are in order of precedence, so:
    ;;  aliases, lisp path, commands, system path
    (flet ((sys-cmd ()
	     "Do a system command."
	     (run-hooks *pre-command-hook* cmd :system-command)
	     (setf (values result result-stream)
		   (do-system-command expanded-expr context))
	     (post-command cmd :system-command)
	     (when (not result)
	       (format t "Command failed.~%"))
	     ;; (finish-output)	   ; @@@ is this really a good place for this?
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
		(post-command cmd :function)
		;;(finish-output)   ; @@@ is this really a good place for this?
		vals))))
      (cond
	;; Alias
	((and alias (not no-alias))
	 ;; re-read and re-eval the line with the alias expanded
	 (shell-eval (expand-alias alias expanded-expr)
		     :context context
		     :no-expansions t))
	;; Lish command
	((typep command '(or internal-command autoloaded-command))
	 (call-thing command (subseq (shell-expr-words expanded-expr) 1)
		     context))
	;; external command
	((typep command 'external-command)
	 (run-hooks *pre-command-hook* cmd :command)
	 (sys-cmd))
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
	 (call-thing command (subseq (shell-expr-words expanded-expr) 1)
	  	     context))
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
		 (values (list (eval-lisp-expr cmd) nil t))))
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
    (with-slots (in-pipe out-pipe environment flipped-io pipe-plus pipe-dot
		 background)
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
					  :pipe-plus pipe-plus
					  :pipe-dot pipe-dot)))
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
		    (setf *input* (if pipe-dot nil *output*)
			  *output* nil
			  flipped-io t))
		(multiple-value-bind (result-values output show-p)
		    (call-thing expr nil *context*)
		  (post-command expr :expression)
		  (setf (output) (car result-values))
		  (values result-values output show-p))))
	     ((consp expr)
	      (if (maybe-a-command-p expr)
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
		   :context context)
		  (with-package *lish-user-package*
		    (values (multiple-value-list (eval-lisp-expr expr)) nil t))))
	     ((stringp expr)
	      (shell-eval (shell-read expr) :context context))
	     (t
	      (with-package *lish-user-package*
		(values (multiple-value-list (eval-lisp-expr expr)) nil t)))))
	  ((zerop (length (shell-expr-words expr)))
	   ;; Quick return when no words
	   (return-from shell-eval (values nil nil nil)))
	  ((and (listp (setf first-word (nth-expr-word 0 expr)))
		(keywordp (first first-word)))
	   ;; First word is a list with a keyword, so it's a compound command.
	   (unless no-expansions
	     (do-expansions expr))
	   (case (first first-word)
	     ((:pipe :pipe-plus :pipe-dot)
	      (when (not flipped-io)
		(setf *input* (if pipe-dot nil *output*)
		      *output* nil
		      *pipe-plus* (eq (first first-word) :pipe-plus)
		      (context-pipe-plus *context*)
		      (eq (first first-word) :pipe-plus)
		      (context-pipe-dot *context*)
		      (eq (first first-word) :pipe-dot)
		      flipped-io t))
	      (setf (values vals out-stream show-vals)
		    (eval-compound (successful vals) t))
	      (values vals out-stream show-vals))
	     (:and      (eval-compound (successful vals) nil))
	     (:or       (eval-compound (not (successful vals)) nil))
	     (:sequence
	      (setf pipe-dot t) ;; don't pass *output* for sequences
	      (eval-compound t nil))
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
	       (setf *input* (if pipe-dot nil (output))
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
		(setf line (s+ line +newline-string+ new-line)))
	     (when (and expr (not (eql expr *continue-symbol*)))
	       (setf expression-start-line nil))
	     (shell-eval expr))
	  (when (eql expr *continue-symbol*)
	    (error "End of file in expression. Probably starting at line ~a."
		   expression-start-line)))))))

(defun expand-load-file-name (file-name)
  (expand-tilde (expand-variables file-name)))

(defun pick-an-rc-file ()
  (loop :for file :in (list *lishrc*
			    (path-append (config-dir "lish") "lishrc")
			    *default-lishrc*)
     :do
     (when file
       (let ((expanded-file (expand-load-file-name file)))
	 (when (and expanded-file (probe-file expanded-file))
	   (return expanded-file))))))
  
(defun load-rc-file (init-file)
  "Load the users start up (a.k.a. run commands) file, if it exists."
  (when init-file
    (let ((file-name (expand-load-file-name init-file)))
      (when (nos:file-exists file-name)
	(let ((*lish-user-package* (find-package :lish-user)))
	  (load-file file-name))))))

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
    (typecase thing
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
      (t
       (if (and (refer-to :bt '*supports-threads-p*)
		(symbol-call :bt :threadp thing))
	   (setf job (make-instance 'thread-job
				    :id (find-id *shell*)
				    :name name
				    :command-line command-line
				    :status status
				    :thread thing))
	 (error "Tried to a job of an unknown type ~s" (type-of thing)))))
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
      ;; (uos:set-signal-action uos:+SIGTSTP+ :ignore)
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
    (finish-output)
    (tt-finish-output)
    (setf str (rl :eof-value *real-eof-symbol*
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
		  :right-prompt (safety-prompt sh :right)))
    (cond
      ((and (stringp str) (equal 0 (length str))) *empty-symbol*)
      ((equal str *real-eof-symbol*)		  *real-eof-symbol*)
      ((equal str *quit-symbol*)	  	  *quit-symbol*)
      (t
       (setf ! last-command
	     last-command (copy-seq this-command))
       ;; This is the read of the REPL.
       (shell-read (setf this-command
			 (if pre-str
			     (s+ pre-str +newline-string+ str)
			     str)))))))

#+(or)
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
  (with-package *lish-user-package*
    (loop :with len = (length values) :and i = 0
       :for v :in values
       :do
       ;; (format t "~s" v)
       (format t "~w" v)
       (if (and (> len 1) (< i (- len 1)))
	   (format t " ;~%"))
       (incf i)
       :finally (format t "~&"))))

(defparameter *fuxor* nil)

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
       ;; @@@ This one is the trouble
       (when (shell-interactive-p sh)
	 (format t "~%"))		; <<<<
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
		   ;; (maybe-save-values vals show-vals)
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
		(progn
		  (format t "Saving history failed. ~
                             Turn on debug if you want.~%~a~%" c)
		  (abort))))))
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
    (with-simple-restart (abort "Abort finishing lish history.")
      (save-history sh)
      (history-store-done (lish-history-store sh) (history-style sh)))))

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
;; an interactive shell.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-job-signals (() &body body)
    `(unwind-protect
	  (progn
	    (start-job-control)
	    ,@body)
       (stop-job-control (shell-saved-signals *shell*))))

  (defmacro with-shell-signals (() &body body)
    "Evaluate the BODY with the system signal handlers set the way they should
be for normal interactive execution in the shell. This usually means ignoring
suspend ^Z and quit ^\, and setting sub-process jobs control signals."
    (with-names (saved-signals)
      `(let (,saved-signals)
	 (unwind-protect
	      (progn
		(setf ,saved-signals (set-signals))
		(with-job-signals ()
		  ,@body))
	   (when ,saved-signals
	     (restore-signals ,saved-signals))))))

  (defmacro with-new-shell ((&rest args) &body body)
    "Evaluate the BODY with *SHELL* bound to anew shell instance."
    `(let* ((*shell* (make-instance 'shell ,@args))
	    (*history-context* :lish)
	    (*lish-level* (if *lish-level*
			      (funcall #'1+ (symbol-value '*lish-level*))
			      0)))
       ,@body))

  (defmacro with-shell-command (() &body body)
    "Evaluate the BODY as a non-interactive shell command."
    (with-names (result)
      `(let (,result)
	 (catch 'interactive-interrupt
	   (ensure-theme)
	   (unwind-protect
		(with-job-signals ()
		  (run-hooks *enter-shell-hook*)
		  (setf ,result (progn ,@body)))
	     (run-hooks *exit-shell-hook*)))
	 (if (shell-exit-values *shell*)
	     (values-list (shell-exit-values *shell*))
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
	       terminal-type
	       (terminal *terminal*)
	       ;;(init-file (or *lishrc* *default-lishrc*))
	       (init-file (pick-an-rc-file))
	       prompt
	       command)
  "Lish is a LIsp SHell.
Type the “help” command for more documentation.
Arguments:
  debug         - True to turn on entering the debugger on errors.
  terminal-name - Device name of the terminal.
  terminal-type - Type of terminal to read from. Defaults from
                   pick-a-terminal-type and so *default-terminal-type*.
  terminal      - A terminal to use. Ignores terminal-name and terminal-type.
  init-file     - File to load on startup or *default-lishrc* if not given.
  command       - A command to evaluate and exit."
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
	   (*terminal* (or terminal *terminal*))
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
      ;; 	(return-from lish (if (shell-exit-values sh)
      ;; 			      (values-list (shell-exit-values sh))
      ;; 			      result))))

      ;; Load the startup file.
      (load-rc-file init-file)

      (when prompt
	(setf (lish-prompt sh) prompt))

      (setf (shell-interactive-p sh) nil)
      (return-from lish
	(with-shell-command ()
	  (lish-eval sh (typecase command
			  ((or list shell-expr) command)
			  (t (shell-read command)))
		     (make-read-state)))))

    ;; Use our debugger unless we're told not to or it's not loaded.
    (when (and (not *dont-deblarg*) (find-package :deblarg))
      (symbol-call :deblarg :activate :quietly t))

    ;; Figure out the terminal type.
    (setf terminal-type
	  (if terminal
	      (type-of terminal)
	      (or terminal-type
		  (and *terminal*
		       (find-terminal-type-for-class
			(type-of *terminal*)))
		  (pick-a-terminal-type))))
    (with-terminal (terminal-type *terminal*
				  :device-name terminal-name
				  :start-at-current-line t)
      ;; We can't just use a terminal-crunch as *standard-output* because
      ;; it won't know about output from other programs or other streams,
      ;; but since the lower level terminals are are something like
      ;; "immediate mode" maybe they should work? The point is to get
      ;; color output for fat-strings.
      (let ((*standard-output*
	      ;; We can't use typecase because the terminal types don't
	      ;; have to be loaded.
	      (case (keywordify (type-of *terminal*))
		(:terminal-crunch (terminal-wrapped-terminal *terminal*))
		(:null *standard-output*)
		(t *terminal*))))
	(setf (tt-input-mode) :line)

	(ensure-theme)

	;; Load the startup file.
	(load-rc-file init-file)

	(when prompt
	  (setf (lish-prompt sh) prompt))

	(when *use-bracketed-paste*
	  (dlib-i:setup-bracketed-paste))

	;; Make a customized line editor
	(setf (lish-editor sh)
	      (make-instance 'lish-line-editor ;; 'rl:line-editor
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

	(with-shell-signals ()
	  (run-hooks *enter-shell-hook*)
	  (when (not (eq :lish-quick-exit (catch :lish-quick-exit
	    (loop
	      :named pippy
	      :with expr = nil
	      :and lvl = *lish-level*
	      :and eof-count = 0
	      :and retry = nil
	      :if (shell-exit-flag sh)
	        :if (confirm-quit)
		  :return (values-list (shell-exit-values sh))
		:else
		   :do (setf (shell-exit-flag sh) nil)
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
		  nil)))))))
	  ;; (stop-job-control saved-sigs)
	  )
	;;(save-command-stats)
	(run-hooks *exit-shell-hook*)))

    (finish-history sh)

    (when (shell-exit-flag sh)
      (return-from lish (when (shell-exit-values sh)
			  (values-list (shell-exit-values sh)))))
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
   (debug boolean :short-arg #\d :help "True to turn on debugging.")
   ;; We have to do non-auto-help because so it works from shell-toplevel.
   (version boolean :long-arg "version" :help "Print the version and exit.")
   (help boolean :long-arg "help" :help "Show the help."))
  :args-as args
  "Lisp Shell"
  (cond
    (help
     (print-command-help (get-command "lish"))
     (return-from !lish 0))
    (version
     (format t "~a~%" *version*)
     (return-from !lish 0)))
  (remf args :help)
  (remf args :version)
  (when (and greeting (not command))
    (format t "Welcome to ~a ~a~%" *shell-name* *version*))
  (remf args :greeting)
  (when (not init-file-supplied-p)
    (setf init-file (when (not no-init-file) (pick-an-rc-file))))
  (remf args :no-init-file)
  (when no-init-file
    (setf (getf args :init-file) nil))
  (apply #'lish args))

(defun wordify-list (word-list)
  "Return a list of shell words that is like the strings in word-list separated
by spaces."
  (loop :with pos = 0
     :for w :in word-list
     :collect (make-shell-word :word w :start pos :end (+ pos (length w)))
     :do (incf pos (1+ (length w)))))

(defun is-this-shell (command)
  "Return true if command probably names this shell."
  (let ((start 0))
    ;; Called as a login shell on unix, with starting dash.
    #+unix (when (char= (char command 0) #\-)
	     (incf start))
    (and command (stringp command)
	 (command-test (lambda (cmd path)
			 (string-equal path cmd :start1 start))
		       *shell-name* command))))

(defvar *saved-default-external-format* nil)

;; If you want to invoke lish commands from another shell or another program,
;; you can make a "link farm", which is a directory in your path which has
;; links to the shell as the command names you want to invoke. Since lish/los
;; commands are intentionally not very compatible with POSIX, it's advisable
;; that unless you're in a setup that won't be using any POSIX shell scripts,
;; to link commands that are also POSIX commands, as "lish-<command>", and then
;; make aliases in the other shell to invoke them.

(defun shell-toplevel ()
  "Invoked the standalone shell. If we're not invoked as our known shell name,
try to run the command that we're invoked as, or the command we're
invoked as with a (s+ *shell-name* “-”) stripped off."
  (setf *standalone* t
	*default-lishrc* (default-lishrc))
  ;; No, this doesn't work:
  ;; (asdf:initialize-output-translations)
  (uiop:restore-image)

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
    (let* ((args (nos:lisp-args))
	   (command
	     (remove-prefix (nos:basename (car args))
			    (s+ (string-downcase *shell-name*) #\-))))
      (cond
	;; When not invoked as *shell-name*, try to run it as a command.
	;; So you can do the link farm thing.
	((and args (not (is-this-shell command)))
	 (cond
	   ;; @@@ This seems expensive, but we have to check that we won't
	   ;; invoke it as an external command to prevent unwanted recursion.
	   ((ignore-errors (with-new-shell ()
			     ;; (not (member (command-type *shell* (first args))
			     (not (member (command-type *shell* command)
					  '(:external-command :file)))))
	    (if (and *lish-level* (> *lish-level* 7))
		(format t "We're ~s levels deep. Something probably went ~
                           wrong, so I'm giving up.~%~
                           The command was ~s.~%" *lish-level* args)
		(with-new-shell (:debug nil)
		  (setf (shell-interactive-p *shell*) nil)
		  (let ((state (make-read-state)))
		    (with-simple-restart (abort "Abort the command.")
		      (with-error-handling (state)
			(with-shell-command ()
			  (lish-eval *shell*
				     (expr-from-args (cons command (cdr args)))
				     state))))))))
	   (t (format t "Sorry, this ~a doesn't have a ~s command in it.~%"
		      *shell-name* command))))
	((cdr args) ;; Some args
	 ;; Invoke as if we were invoking the lish command from in the shell.
	 (apply #'!lish
		`(,@(posix-to-lisp-args (get-command "lish")
					(wordify-list (cdr args)))
		  :greeting t :terminal-type :crunch)))
	(t ;; No args
	 ;; Interactive
	 (!lish :greeting t :terminal-type :crunch))))
    (nos:exit-lisp)))

(defun make-standalone (&key (name #-windows "lish" #+windows "lish.exe")
			     (smaller t))
  "Make a lish executable."
  #-sbcl (declare (ignore smaller))
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
	(setf options (nconc options '(:compression t))))

      ;; So toplevel doesn't get any interference with the command line.
      ;; Unfortuneately that means we can't change the damn "dynamic" space size.
      (setf options (nconc options '(:save-runtime-options t))))

    ;; Clear the user packages
    (delete-package *lish-user-package*)
    (delete-package *junk-package*)
    (setf *lish-user-package* nil
	  *lish-user-package* (make-user-package)
	  *junk-package* (make-package :lish-junk))

    ;; So the invoking user doesn't get the saving user's.
    (setf *lishrc* nil
	  *default-lishrc* nil)

    ;; Save the build locale data in the image so we don't need it around at
    ;; runtime. @@@ Later it should be a option in build system, which locale
    ;; if any, or all, should be saved in the image.
    (locale:ensure-locale)

    ;; Make sure ASDF is cleared.
    (asdf:clear-configuration)
    (setf asdf:*central-registry* nil)
    (apply #'save-image-and-exit name :initial-function #'lish:shell-toplevel
	   options)))

;; So we can conditionalize adding of lish commands in other packages.
(d-add-feature :lish)

;; EOF
