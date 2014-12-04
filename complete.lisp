;;
;; complete.lisp - Completion for Lish
;;

;; $Revision$

#|

If we can't do at least a good as Tops-20 COMND% JSYS, then we suck.

I dream and aspire to be as good as or better than CP, "The Command Processor
Reader" on Symbolics Genera, but since I never really used it, I won't know,
will I. Anyway, we're ostensibly driving Unix underneath, which is a bit of a
different beasty.

I designed this thing without really knowing about the Symbolics stuff but it
turns out I did something similar. It seems like having something like
"defcommand" and a reader which does the right thing based on it, and which
can basicly prompt for lambda expressions, is an inherently Lispy way to do
it. But the part with translating to Unix style is really quite crufty.

|#

(in-package :lish)

; (defun quoted-start (str pos)
;   "Check if we are inside a shell quoted string and return it's starting
;  position."
;   (

(defun complete-env-var (str all)
  ;; (complete-string-sequence
  ;;  str all (mapcar #'(lambda (x) (string (car x))) (nos:environ))))
  (complete-list str (length str) all
		 (mapcar #'(lambda (x) (string (car x))) (nos:environ))))

(defun complete-user-name (str all)
  (prog2
      (nos:setpwent)
      (complete-list str (length str) all
		     (loop :with p = nil
			:while (setf p (nos:getpwent))
			:collect (nos:passwd-name p)))
    (nos:endpwent)))

;; @@@ Consider caching this.
;; @@@ In fact we should probably require a "rehash", like other shells.
(defparameter *verb-list* nil
  "List of current lish commands. Includes aliases, built-in commands, and ~
exectuables in the path. Use the \"rehash\" command to update after new ~
commands are added.")

(defun probe-file-or-dir (p)
  (or (probe-directory p) (probe-file p)))

(defun verb-list (shell)
  (declare (type shell shell))
  "Return the command list for the current shell: *shell*."
  (if (not *verb-list*)
      (setf *verb-list*
	    (remove-duplicates
	     (append
	      (loop :for k :being :the :hash-keys :of (lish-aliases shell)
		 :collect k)
	      (loop :for k :being :the :hash-keys :of (lish-commands)
		 :collect k)
	      (loop :for dir :in (split-sequence #\: (nos:getenv "PATH"))
		 :if (probe-directory dir)
		 :append (loop :for f :in (nos:read-directory :dir dir :full t)
			    :if (eql (nos:dir-entry-type f) :regular)
			    :collect (nos:dir-entry-name f))))
	     :test #'equal))
      *verb-list*))

(defun complete-command (str all)
;  (complete-string-sequence str all (verb-list *shell*)
  (complete-list str (length str) all (verb-list *shell*)))

;; This is mostly like complete-symbol but it handles the ! at the beginning.
;; XXX Uses completion internals.
(defun complete-bang-symbol (context pos all)
  "Completion function for symbols (preceded by ! in the shell)."
  (let* ((word-start (completion::scan-over-str
		      context pos :backward
		      :not-in completion::*lisp-non-word-chars*))
	 (word (subseq context word-start pos))
	 (pack nil)
	 (external nil))
;    (format t "Howdy: word-start ~s word ~s~%" word-start word)
    (when (eql #\! (aref word 0))
      (setf word (subseq word 1)
	    word-start (1+ word-start)))
    (multiple-value-setq (pack external)
      (completion::find-back-pack context word-start))
    (if all
	(completion::symbol-completion-list
	 word :package pack :external external)
	(values (completion::symbol-completion
		 word :package pack :external external) word-start))))

(defun quotify (string)
  "Put a backslash in front of any character that might not be intrepreted
literally in shell syntax."
  (let ((result string))
    (flet ((possibly-quote (c)
	     (when (position c result)
	       (setf result (join (split-sequence c result) (s+ #\\ c))))))
      (loop :for c :across " !$|;[]*?()" :do ;
  (possibly-quote c))
      result)))

(defvar *junk-package*
  (progn
    (when (find-package :lish-junk)
      (delete-package :lish-junk))
    (make-package :lish-junk)))

;; Remember, a completion functions returns:
;;   One completion: completion and replacement starting position
;;   List:           sequence and sequence length

(defun shell-complete (context pos all)
  (declare (type string context))
  "Analyze the context and try figure out what kind of thing we want to ~
complete, and call the appropriate completion function."
  (let ((exp (ignore-errors (shell-read context :partial t
					:package *junk-package*))))
    (typecase exp
      (shell-expr
       (let* ((word-num (shell-word-number exp pos))
	      (word     (if word-num
			    (elt (shell-expr-words exp) word-num))))
	 (dbug "~%word-num = ~w word = ~w~%exp ~w~%" word-num word exp)
	 (flet ((simple-complete (func word wpos)
		  (if all
		      (let ((list (funcall func word all)))
			(values list (length list)))
		      (values (funcall func word all) wpos))))
	   (cond
	     ((not word-num)		; no words
	      (simple-complete #'complete-command "" 0))
	     ((not word)		; probably ()
	      (complete-symbol context pos all))
	     ((symbolp word)
	      (complete-bang-symbol context pos all))
	     ((consp word)		; (foo)
	      (complete-symbol context pos all))
	     ((eql (aref word 0) #\()	; (foo
	      (complete-symbol context pos all))
	     ((eql (aref word 0) #\!)	; !foo
	      (complete-bang-symbol context pos all))
	     ((eql (aref word 0) #\$)	; $foo
	      (simple-complete #'complete-env-var
			       (subseq word 1)
			       (1+ (elt (shell-expr-word-start exp)
					word-num))))
	     ((and (eql (aref word 0) #\~) ; ~foo
		   (valid-user-name (subseq word 1)))
;;;	      (format t "CHING! ~a~%" (valid-user-name word))
	      (simple-complete #'complete-user-name
			       (subseq word 1)
			       (1+ (elt (shell-expr-word-start exp)
					word-num))))
	     ;; first word, when not starting with directory chars
	     ((and (= word-num 0) (not (position (aref word 0) "/.~")))
	      ;; try commands
	      (multiple-value-bind (v1 v2)
		  (simple-complete #'complete-command context
				   (elt (shell-expr-word-start exp) 0))
;		(format t "CMD v1=~s v2=~s~%~%" v1 v2)
		;; then symbols
		(when (not v1)
		  (setf (values v1 v2)
			(complete-symbol context pos all))
;		  (format t "SYM v1=~s v2=~s~%~%" v1 v2)
		  )
		(values v1 v2)))
	     (t
	      (let ((from-end (- (length context) pos)))
		(multiple-value-bind (result new-pos)
		    (complete-filename word (- (length word) from-end) all)
		  (declare (ignore new-pos))
		  (values (if (not all) (quotify result) result)
			  (elt (shell-expr-word-start exp) word-num)))))))))
      (cons
       (complete-symbol context pos all)))))

;; EOF
