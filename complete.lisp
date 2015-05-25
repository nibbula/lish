;;
;; complete.lisp - Completion for Lish
;;

;; $Revision$

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

(defun words-past (expr pos)
  "Return how many words the position POS is past in EXPR."
  (let ((past 0))
    (loop :for i :from 0 :below (length (shell-expr-words expr))
       :do (when (> pos (elt (shell-expr-word-end expr) i))
	     (setf past (1+ i))))
    past))

;; Note that this takes different args than a normal completion function.
(defun complete-command-arg (command expr pos all &optional word-num word)
  "Complete a command argument."
  (let* ((past (words-past expr pos))
	 (fake-word (or word ""))
	 (arg (nth (1- past) (command-arglist command)))
	 (doc (and arg (documentation (type-of arg) 'type)))
	 (choices (and arg (argument-choices arg))))
    (if all
	(progn
	  #| (print-values* (command expr pos all word-num word)) |#
;;;	  (format t "ummm...~a~%" past)
	  (if (and (= past 1) (not word-num))
	      (progn
;		(format t "snoo ~a? words-past ~a~%" command past)
		(let* ((cols
			(ansiterm:terminal-window-columns
			 (tiny-rl::line-editor-terminal (lish-editor *shell*))))
		       (out-str (s+ (posix-synopsis command) #\newline
				    (or doc "") #\newline
				    (or (and choices
					     (with-output-to-string (str)
					       (print-columns choices
							      :stream str
							      :columns cols)))
					""))))
		  (when (eql (char out-str (1- (length out-str))) #\newline)
		    (setf out-str (subseq out-str 0 (1- (length out-str)))))
		  (list out-str)))
	      (progn
		(complete-filename fake-word pos all))))
	(if choices
	    (complete-list fake-word (length fake-word) all choices)
	    (complete-filename fake-word pos all)))))

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
					:package *junk-package*)))
	cmd)
    (typecase exp
      (cons
;;;       (format t "Hellow I am janky!~%")
       (complete-symbol context pos all))
      (shell-expr
       (let* ((word-num (shell-word-number exp pos))
	      (word (if word-num
			(elt (shell-expr-words exp) word-num)
			nil)))
;;;	 (format t "~%word-num = ~w word = ~w~%exp ~w~%" word-num word exp)
	 (flet ((simple-complete (func word wpos)
		  (if all
		      (let ((list (funcall func word all)))
			(values list (length list)))
		      (values (funcall func word all) wpos))))
	   (cond
	     ((and (not word-num) (= pos 0))
	      ;; no words
;;;	      (format t "none~%")
	      (simple-complete #'complete-command "" 0))
	     ((not word)
	      (if (= 0 (length (shell-expr-words exp)))
		  (progn
		    ;; probably ()
;;;		    (format t "bogo~%")
		    (complete-symbol context pos all))
		  (let ((from-end (- (length context) pos))
			(first-word (first (shell-expr-words exp))))
;;;		    (format t "heyba~%")
		    (multiple-value-bind (result new-pos)
			(if (setf cmd (get-command first-word))
			    (progn
;;;			      (format t "Baaa~%")
			      (complete-command-arg cmd exp pos all))
			    (complete-filename word (- (length word) from-end) all))
		      (declare (ignore new-pos))
		      (values (if (not all) (quotify result) result)
			      (or (and word-num
				       (elt (shell-expr-word-start exp) word-num))
				  pos))))))
	     ((symbolp word)
;;;	      (format t "janky~%")
	      (complete-bang-symbol context pos all))
	     ((consp word)		; (foo)
;;;	      (format t "junky~%")
	      (complete-symbol context pos all))
	     ((eql (aref word 0) #\()	; (foo
;;;	      (format t "half baka~%")
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
	      (simple-complete #'complete-user-name
			       (subseq word 1)
			       (1+ (elt (shell-expr-word-start exp)
					word-num))))
	     ;; first word, when not starting with directory chars
	     ((and (= word-num 0) (not (position (aref word 0) "/.~")))
;;;	      (format t "jinky~%")
	      ;; try commands
	      (multiple-value-bind (v1 v2)
		  (simple-complete #'complete-command context
				   (elt (shell-expr-word-start exp) 0))
		;; then symbols
		;; XXX Symbols won't come up in the list.
		(when (not v1)
		  (setf (values v1 v2)
			(complete-symbol context pos all))
		  )
		(values v1 v2)))
	     (t
	      (let ((from-end (- (length context) pos)))
;;;		(format t "hello ~a~%" word)
		(multiple-value-bind (result new-pos)
		    (if (setf cmd (get-command
				   (elt (shell-expr-words exp) 0)))
			(progn
;;;			  (format t "blurgg~%")
			  (complete-command-arg
			   cmd exp pos #| (- (length word) from-end) |#
			   all word-num word))
			(progn
;;;			  (format t "jorky~%")
			  (complete-filename word (- (length word) from-end)
					     all)))
		  (declare (ignore new-pos))
		  (values (if (not all) (quotify result) result)
			  (elt (shell-expr-word-start exp) word-num))))))))))))

;; EOF
