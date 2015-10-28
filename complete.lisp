;;
;; complete.lisp - Completion for Lish
;;

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

;; Perhaps, this should be in OPSYS, but of course there's also access(2).
(defun is-executable (pathname)
  "Given a pathname, return true if it's likely to be executable, which
probably means it's a regular file and we have execute permission on it."
  (let* ((stat (stat pathname))
	 (mode (file-status-mode stat)))
    (and (is-regular-file mode)
	 (or (is-other-executable mode)
	     (and (is-user-executable mode)
		  (eql (file-status-uid stat) (geteuid)))
	     (and (is-group-executable mode)
		  (or (eql (file-status-gid stat) (getegid)))
		  (position (file-status-gid stat) (get-groups)))))))

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
		 :append (loop :for f :in (nos:read-directory :dir dir :full t
							      :omit-hidden t)
			    :if (without-access-errors
				  (is-executable (s+ dir *directory-separator*
						     (nos:dir-entry-name f))))
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

(defun list-arg-choices (command doc choices)
  (let* ((cols
	  (terminal-window-columns
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

(defun show-dash-arglist (arglist)
  (list
   (with-output-to-string (str)
     (loop :with print-newline = nil
	:for a :in arglist
	:when (and (arg-short-arg a)
		   (not (arg-hidden a)))
	:do
#|	(format str "~:[~;~%~]-~a ~:[~;[T] ~]~25a~@[ ~a~]"
		print-newline
		(arg-short-arg a) (arg-default a) (arg-name a)
		(and (slot-boundp a 'help) (arg-help a))) |#
	(format str "~:[~;~%~] -~a ~@[ ~a~] ~:[~;~1:*[~a] ~]"
		print-newline
		(arg-short-arg a)
		(or (and (slot-boundp a 'help) (arg-help a))
		    (arg-name a))
		(arg-default a))
	(when (not print-newline)
	  (setf print-newline t))))))

(defvar *long-double-dash-help* nil
  "True to show longer help for double dash arguments.")

(defun show-double-dash-arglist (arglist)
  (let ((result (make-stretchy-string 200)))
    (with-output-to-string (str result)
      (table:nice-print-table
       (loop :for a :in arglist
	  :when (and (arg-long-arg a)
		     (not (arg-hidden a)))
	  :collect
	  (if *long-double-dash-help*
	      (list (s+ "--" (arg-long-arg a))
		    (arg-default a)
		    (string-downcase (arg-type a))
		    (or (and (slot-boundp a 'help) (arg-help a))
			(arg-name a)))
	      (list (s+ "--" (arg-long-arg a))
		    (or (and (slot-boundp a 'help) (arg-help a))
			(format nil "~s ~(~a~)"
				(arg-default a) (arg-type a))))))
       nil :stream str :trailing-spaces nil))
    ;; Get rid of the final newline
    (when (char= #\newline (aref result (- (length result) 1)))
      (setf (fill-pointer result) (- (length result) 2)))
    (list result)))

(defun complete-double-dash-arglist (word pos arglist)
  (dbug "word = ~s pos = ~s~%" word pos)
  (complete-list
   ;; (subseq word 2) (- pos 2) nil
   word pos nil
   (loop :for a :in arglist
      :if (arg-long-arg a)
      :collect (s+ "--" (arg-long-arg a)))))

(defun first-mandatory-or-non-flag-arg (past arglist)
  (or (loop :with i = 0
	 :for a :in arglist :do
;;;	 (format t "~a ~a ~s~%" i (>= i (1- past)) a)
	 (when (and (>= i (1- past))
		    (not (arg-optional a)))
	   (return-from first-mandatory-or-non-flag-arg a))
	 (incf i))
      (loop :with i = 0
	 :for a :in arglist :do
;;;	 (format t "~a ~a ~s~%" i (>= i (1- past)) a)
	 (when (and (>= i (1- past))
		    (not (and (or (arg-short-arg a)
				  (arg-long-arg a)
				  (arg-old-long-arg a))
			      (eq (arg-type a) 'boolean))))
	   (return-from first-mandatory-or-non-flag-arg a))
	 (incf i))
      (nth (1- past) arglist)))

;; Note that this takes different args than a normal completion function.
(defun complete-command-arg (context command expr pos all
			     &optional word-num word word-pos)
  "Complete a command argument."
  (let* ((past (words-past expr pos))
	 (fake-word (or word ""))
;;;	 (arg (nth (1- past) (command-arglist command)))
	 (arg (first-mandatory-or-non-flag-arg past (command-arglist command)))
	 (func (and arg (arg-completion-function arg))))
    (dbug "cmd arg ~s ~s ~s ~s ~s ~s~%"
	  context pos fake-word word-pos arg func)
    (cond
      ((and word-pos (> word-pos 1)
;;;	    (char= (char word (1- word-pos)) #\-)
;;;	    (char= (char word (- word-pos 2)) #\-))
	    (char= (char word 0) #\-)
	    (char= (char word 1) #\-))
       ;; double dash args
       (if all
	   (show-double-dash-arglist (command-arglist command))
	   (progn
	     (complete-double-dash-arglist word word-pos
					   (command-arglist command)))))
      ((and all word-pos
	    (> word-pos 0)
	    (char= (char word (1- (min word-pos (length word)))) #\-))
       ;; dash arg enumeration
       (show-dash-arglist (command-arglist command)))
      (func
       (dbug "---> (~a ~s ~s ~s )~%" func fake-word (length fake-word) all)
       (funcall func fake-word (length fake-word) all :parsed-exp expr))
      (t
       (let ((doc (and arg (documentation (type-of arg) 'type)))
	     (choices (and arg (argument-choices arg))))
	 (let ((*print-lines* 20))
	   (dbug "wazzup? ~s choices ~w ~%" fake-word choices))
	 (if all
	     (progn
	       #| (print-values* (command expr pos all word-num word)) |#
	       (dbug "ummm...~a~%" past)
	       (if (and (= past 1) (not word-num))
		   (progn
		     (dbug "snoo ~a? words-past ~a~%" command past)
		     (list-arg-choices command doc choices))
		   (progn
		     (if (and fake-word choices)
			 (complete-list fake-word
					(length fake-word) all choices)
			 (complete-filename fake-word pos all)))))
	     (progn
	       (dbug "cmd arg fake-word ~s" fake-word)
	       (if choices
		   (complete-list fake-word (length fake-word) all choices)
		   (complete-filename fake-word pos all)))))))))

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
       (dbug "Hellow I am janky!~%")
       (complete-symbol context pos all))
      (shell-expr
       (let* ((word-num (shell-word-number exp pos))
	      (first-word (first (shell-expr-words exp)))
	      word word-pos)
	 ;; word-num is the index of the word in the shell expr
	 ;; word is the text of the word
	 ;; word-pos is the relative position in the word
	 (when word-num
	   (setf word (elt (shell-expr-words exp) word-num)
		 word-pos (- pos (elt (shell-expr-word-start exp) word-num))))
	 (dbug "~%word-num = ~w word = ~w word-pos = ~w~%exp ~w~%"
	       word-num word word-pos exp)
	 (flet ((simple-complete (func word wpos)
		  (if all
		      (let ((list (funcall func word all)))
			(values list (length list)))
		      (values (funcall func word all) wpos))))
	   (cond
	     ((and (not word-num) (= pos 0))
	      ;; no words
	      (dbug "none~%")
	      (simple-complete #'complete-command "" 0))
	     ((not word)
	      (if (= 0 (length (shell-expr-words exp)))
		  (progn
		    ;; probably ()
		    (dbug "bogo~%")
		    (complete-symbol context pos all))
		  (let ((from-end (- (length context) pos)))
		    (dbug "heyba~%")
		    (multiple-value-bind (result new-pos)
			(if (setf cmd (get-command first-word))
			    (progn
			      (dbug "Baaa~%")
			      (complete-command-arg context cmd exp pos all))
			    (complete-filename word
					       (- (length word) from-end) all))
		      (declare (ignore new-pos))
		      (values (if (not all) (quotify result) result)
			      (or (and word-num
				       (elt (shell-expr-word-start exp)
					    word-num))
				  pos))))))
	     ((symbolp word)
	      (dbug "janky~%")
	      (complete-bang-symbol context pos all))
	     ((consp word)		; (foo)
	      (dbug "junky~%")
	      (complete-symbol context pos all))
	     ((eql (aref word 0) #\()	; (foo
	      (dbug "half baka~%")
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
	      (dbug "jinky~%")
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
		(dbug "hello ~a~%" word)
		(multiple-value-bind (result new-pos)
		    (if (setf cmd (get-command first-word))
			(progn
			  (dbug "blurgg~%")
			  (complete-command-arg
			   context cmd exp pos #| (- (length word) from-end) |#
			   all word-num word word-pos))
			(progn
			  (dbug "jorky~%")
			  ;; But it could be a command which isn't loaded yet.
			  (if (load-lisp-command first-word)
			      (complete-command-arg
			       context (get-command first-word) exp pos
			       all word-num word word-pos)
			      (complete-filename word
						 (- (length word) from-end)
						 all))))
		  (declare (ignore new-pos))
		  (dbug "result = ~s~%" result)
		  (if all
		      (values result (length result))
		      (values
		       (quotify result)
		       (elt (shell-expr-word-start exp) word-num)))))))))))))

;; EOF
