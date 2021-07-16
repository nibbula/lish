;;;
;;; complete.lisp - Completion for Lish
;;;

(in-package :lish)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

;; (defun quoted-start (str pos)
;;   "Check if we are inside a shell quoted string and return it's starting
;;  position."
;;   (

(defun complete-env-var (str all)
  ;; (complete-string-sequence
  ;;  str all (mapcar #'(lambda (x) (string (car x))) (nos:environment))))
  (complete-list str (length str) all
		 (append
		  (fake-var-list)
		  (mapcar #'(lambda (x) (string (car x))) (nos:environment)))))

(defun complete-user-name (str all)
  (let ((result
	  (complete-list str (length str) all
			 (mapcar (_ (nos:user-info-name _)) (nos:user-list)))))
    ;; Put a trailing directory separator on it, it it exists.
    (when (and (completion-result-unique result)
	       (file-exists
		(expand-tilde (s+ #\~ (completion-result-completion result)))))
      (setf (completion-result-completion result)
	    (s+ (completion-result-completion result)
		nos:*directory-separator*)))
    result))

(defvar *verb-list* nil
  "List of current lish commands. Includes aliases, built-in commands, and ~
exectuables in the path. Use the \"rehash\" command to update after new ~
commands are added.")

#+nil (progn ;; Unused code for better verb list caching
(defun probe-file-or-dir (p)
  (or (probe-directory p) (probe-file p)))

(defvar *last-update* nil
  "An alist of (:<facility> . <time-code>) for storing last time we updated the
*VERB-LIST* for various facilites. The structure of <time-code> depends on the
facility. Usually it's a universal-time, or an alist of (<thing> . <time>).")

(defun verb-list-needs-upadating-p ()
  (with-spin ()
    (loop :for dir :in (split-sequence
			nos:*path-separator*
			(nos:environment-variable *path-variable*))
       :do (spin)
       :if (probe-directory dir)
       :append (loop :for f :in (nos:read-directory
				 :dir dir :full t
				 :omit-hidden t)
		  :if (without-access-errors
			  (is-executable
			   (s+ dir *directory-separator*
			       (nos:dir-entry-name f))))
		  :collect (nos:dir-entry-name f)))))

(defun update-verb-thing (thing)
  (ecase thing
    (:aliases)
    (:commands)
    (:path))
  )
)

(defun verb-list (shell)
  "Return the command list for the current shell: *shell*."
  (if (not *verb-list*)
      (setf *verb-list*
	    (with-spin ()
	      (locally
		#+sbcl (declare
			(sb-ext:muffle-conditions sb-ext:compiler-note))
		(sort
		 (remove-duplicates
		  (append
		   (loop :for k :being :the :hash-keys :of (lish-aliases shell)
		      :collect k)
		   (loop :for k :being :the :hash-keys :of (lish-commands)
		      :do (spin)
		      :collect k)
		   (loop :for dir :in (split-sequence
				       nos:*path-separator*
				       (nos:environment-variable
					*path-variable*))
		      :do (spin)
		      :if (probe-directory dir)
		      :append (loop :for f :in (nos:read-directory
						:dir dir :full t
						:omit-hidden t)
				 :if (without-access-errors
					 (is-executable
					  (s+ dir *directory-separator*
					      (nos:dir-entry-name f))))
				 :collect (nos:dir-entry-name f))))
		  :test #'equal)
		 #'string<))))
      *verb-list*))

(defun complete-command (str all)
;  (complete-string-sequence str all (verb-list *shell*)
  (complete-list str (length str) all (verb-list *shell*)))

;; This is mostly like complete-symbol but it handles the ! at the beginning.
;; XXX Uses completion internals.
(defun complete-bang-symbol (context pos all)
  "Completion function for symbols (preceded by ! in the shell)."
  (let* ((word-start (scan-over-string
		      context pos :backward
		      :not-in completion::*lisp-non-word-chars*))
	 (word (subseq context word-start pos))
	 (pack nil)
	 (external nil))
    (dbugf 'completion "bang completion: word-start ~s word ~s~%"
	   word-start word)
    (when (and (plusp (length word)) (eql #\! (aref word 0)))
      (setf word (subseq word 1)
	    word-start (1+ word-start)))
    (multiple-value-setq (pack external)
      (completion::find-back-pack context word-start))
    (if all
	(completion::symbol-completion-list
	 word :package pack :external external)
	(let ((result
	       (completion::symbol-completion
		word :package pack :external external)))
	  (setf (completion-result-insert-position result)
		word-start)
	  result))))

(defun shell-complete-symbol (context pos all &optional bang-p)
  "Complete symbols in the *lish-user-package*, optionally with a
preceding exclamation point '!' ."
  (with-package *lish-user-package*
    (if bang-p
	(complete-bang-symbol context pos all)
	(complete-symbol context pos all))))

;; Things in here:
;;   quote char:                      \
;;   word separation:                 #\space
;;   history / variable expansion:    !
;;   lisp expression expansion:       ,
;;   environment variable expansion:  $
;;   piping:                          |  (maybe should also '&' and '^' ?)
;;   comment char:                    ;
;;   glob chars:                      []*?
;;   parens (evaluation):             ()

;; Note that backslash '\' must be first in this list, since otherwise you will
;; get doubled backslashes.
(defparameter *quote-needing-chars* "\\ !,$|;[]*?()"
  "Characters that may need quoting in shell syntax, like if they are in a file
name, so they don't get interpreted by the shell.")

(defun quotify (string)
  "Put a backslash in front of any character that might not be intrepreted
literally in shell syntax."
  (let ((result string))
    (flet ((possibly-quote (c)
	     (when (position c result)
	       (setf result (join-by-string (split-sequence c result)
					    (s+ #\\ c))))))
      (loop :for c :across *quote-needing-chars* :do ;
	 (possibly-quote c))
      result)))

;; perhaps would be faster with position and subseq?
(defun de-quotify (string)
  "Convert double backslashes into a single backslash."
  (with-output-to-string (str)
    (let* ((len (length string)) (i 0))
      (loop :while (< i (1- len))
	 :do
	 (when (and (char= (char string i) #\\)
		    (char= (char string (1+ i)) #\\))
	   (incf i))
	 (write-char (char string i) str)
	 ;;(setf (char output o) (char string i))
	 (incf i))
      (when (< i len)
	(write-char (char string i) str)
	))))

;; (defun words-past (expr pos)
;;   "Return how many words the position POS is past in EXPR."
;;   (let ((past 0))
;;     (loop
;;        :for i = 0 :then (1+ i)
;;        :for w :in (shell-expr-words expr)
;;        :do (when (> pos (shell-word-end w))
;;     	     (setf past (1+ i))))
;;     past))

(defun words-past (expr pos)
  (multiple-value-bind (word num) (find-shell-word expr pos)
    (declare (ignore word))
    (or num -1)))
  ;; (let ((num (shell-word-num expr
  ;; 		      ;; (min pos
  ;; 		      ;; 	(length (shell-expr-line expr)))
  ;; 			     pos)))
  ;;   (or (and num (1- num)) 0)))

(defun first-word-in-expr (expr pos)
  "Find the first word of pipeline where POS is in a shell expr."
  (let ((w (first (shell-expr-words expr)))
	(w2 (second (shell-expr-words expr))))
    (cond
      ((stringp w) w)
      ((shell-word-p w)
       (if (>= pos (shell-word-start w))
	   w
	   nil)) ; We couldn't find it?
      ((and (consp w) (member (car w) '(:pipe :pipe-plus :pipe-dot)))
       (if (and w2 (shell-word-p w2)
		(>= pos (shell-word-start w2)))
	   w2
	   (first-word-in-expr (second w) pos))))))

(defun term-cols ()
  "Return the terminal columns."
  (terminal-window-columns
   (rl:line-editor-terminal (lish-editor *shell*))))

(defun list-arg-choices (command doc choices)
  (let* (;; (cols (term-cols))
	 (prefix-str (s+ (if (>= *completion-count* 1)
			     (s+ (documentation command 'function) #\newline)
			     (s+ (posix-synopsis command) #\newline))
			 (or doc "") #\newline))
	 (comp-list (or choices (list ""))))
			  ;;      (with-output-to-string (str)
			  ;; 	 (print-columns choices
			  ;; 			:stream str
			  ;; 			:columns cols)))
			  ;; ""))))
    ;; Trim possible trailing newline?
    ;; (when (eql (char out-str (1- (length out-str))) #\newline)
    ;;   (setf out-str (subseq out-str 0 (1- (length out-str)))))
    ;;(make-completion-result :completion (list out-str) :count 1)))
    (make-completion-result :completion comp-list
			    :count (length comp-list)
			    :prefix prefix-str)))

(defun show-dash-arglist (arglist)
  (let ((result (make-stretchy-string 200)))
    (with-output-to-string (str result)
      (output-table
       (make-table-from
       (loop :for a :in arglist
	  :when (and (arg-short-arg a)
		     (not (arg-hidden a)))
	  :collect
	  (list (s+ " -" (arg-short-arg a))
		(s+ (or (and (slot-boundp a 'help)
			     (substitute #\space #\newline (arg-help a)))
			(arg-name a))
		    (if (arg-default a)
			(s+ " [" (arg-default a) "]")
			"")))
	  :when (and (arg-old-long-arg a)
		     (not (arg-hidden a)))
	  :collect
	  (list (s+ " -" (arg-old-long-arg a))
		(s+ (or (and (slot-boundp a 'help)
			     (substitute #\space #\newline (arg-help a)))
			(arg-name a))
		    (if (arg-default a)
			(s+ " [" (arg-default a) "]")
			""))))
       :columns '((:name "Arg") (:name "desc" :align :wrap)))
       (make-instance 'text-table-renderer)
       str
       :trailing-spaces nil
       :print-titles nil :max-width (term-cols)))
    (setf result (rtrim result))
    ;; Get rid of the final newline
    (when (and (> (length result) 0)
	       (char= #\newline (aref result (- (length result) 1))))
      ;; (setf (fill-pointer result) (- (length result) 2))
      (setf (fill-pointer result) (- (length result) 1)))
    (make-completion-result :completion (list result) :count 1)))

(defvar *long-double-dash-help* nil
  "True to show longer help for double dash arguments.")

(defun show-double-dash-arglist (arglist)
  (let ((result (make-stretchy-string 200)))
    (with-output-to-string (str result)
      (output-table
       (make-table-from
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
		    (or (and (slot-boundp a 'help)
			     (substitute #\space #\newline (arg-help a)))
			(format nil "~s ~(~a~)"
				(arg-default a) (arg-type a))))))
       :columns '((:name "Arg" :align :overflow)
		  (:name "desc" :align :wrap)))
       (make-instance 'text-table-renderer) str
       :trailing-spaces nil :print-titles nil :max-width (term-cols)))
    (setf result (trim result))
    ;; Get rid of the final newline
    (when (and (> (length result) 0)
	       (char= #\newline (aref result (- (length result) 1))))
      (setf (fill-pointer result) (- (length result) 2)))
    (make-completion-result :completion (list result) :count 1)))

(defun complete-double-dash-arglist (word pos arglist)
  (dbugf 'completion "word = ~s pos = ~s~%" word pos)
  (complete-list
   ;; (subseq word 2) (- pos 2) nil
   word pos nil
   (loop :for a :in arglist
      :if (arg-long-arg a)
      :collect (s+ "--" (arg-long-arg a)))))

(defun complete-dash-arglist (word pos arglist)
  (dbugf 'completion "word = ~s pos = ~s~%" word pos)
  (complete-list
   ;; (subseq word 2) (- pos 2) nil
   word pos nil
   (loop :for a :in arglist
      :if (arg-old-long-arg a)
      :collect (s+ "-" (arg-old-long-arg a)))))

(defun first-mandatory-or-non-flag-arg (past arglist)
  (or (loop :with i = 0
	 :for a :in arglist :do
	 ;;(format t "~a ~a ~s~%" i (>= i (1- past)) a)
	 (when (and (>= i (1- past))
		    (not (arg-optional a)))
	   (return-from first-mandatory-or-non-flag-arg a))
	 (incf i))
      ;; @@@ Unfortunately this makes the wrong choice for non-boolean
      ;; args. It should only pick a non-boolean arg which has a flag if we
      ;; are past the flag, but the simple numeric PAST count can't indicate
      ;; that. I'm not really sure what a workable way to patch this is. This
      ;; really shows how I need to totally redesign the argument code to do
      ;; proper parsing. Then we can reliably show what are the choices from a
      ;; given parse state.
      (loop :with i = 0
	 :for a :in arglist :do
	 ;;(format t "~a ~a ~s~%" i (>= i (1- past)) a)
	 (when (and (>= i (1- past))
		    (not (and (or (arg-short-arg a)
				  (arg-long-arg a)
				  (arg-old-long-arg a))
			      (eq (arg-type a) 'boolean))))
	   (return-from first-mandatory-or-non-flag-arg a))
	 (incf i))
      (nth (max 0 (1- past)) arglist)))

(defun flag-arg-p (arg)
  (and (or (arg-short-arg arg)
	   (arg-long-arg arg)
	   (arg-old-long-arg arg))
       (not (eq (arg-type arg) 'boolean))))

#|
(defun what-next (command expr pos)
  (let ((unused (copy-seq (command-arglist command)))
	used previous-flaged-arg)
    (loop :for word :in (cdr (mapcar #'word-word (shell-expr-words expr)))
       :do
       (loop :for a :in (command-arglist command)
	  :do
	  (cond
	    ((and (flag-arg-p a)
		  (or (string-equal word (s+ "-" (arg-short-arg a)))
		      (string-equal word (s+ "--" (arg-long-arg a)))))
	     (setf previous-flaged-arg a)
	     (return))
	    ((and (arg-optional
|#

(defun shell-complete-quoted (func word position all &key parsed-exp)
  "Complete a thing that needs quoting, using the completion function FUNC."
  (declare (ignore parsed-exp))
  (let ((result (funcall func (de-quotify word) position all)))
    (when (and result (completion-result-completion result))
      (setf (completion-result-completion result)
	    (quotify (completion-result-completion result))))
    result))

(defun shell-complete-filename (word position all &key parsed-exp)
  "A version of complete-filename which does the right thing for the shell."
  (declare (ignore parsed-exp))
  (shell-complete-quoted #'complete-filename word position all))

(defun shell-complete-directory (word position all &key parsed-exp)
  "A version of complete-directory which does the right thing for the shell."
  (declare (ignore parsed-exp))
  (shell-complete-quoted #'complete-directory word position all))

;; Note that this takes different args than a normal completion function.
(defun complete-command-arg (context command expr pos all
			     &optional word-num shell-word word-pos)
  "Complete a command argument."
  (let* ((past (words-past expr pos))
	 ;; (past (or (shell-word-num expr
	 ;; 			   ;; (min pos
	 ;; 			   ;; 	(length (shell-expr-line expr)))
	 ;; 			   pos) 0))
	 (word (word-word shell-word))
	 (fake-word (or word ""))
;;;	 (arg (nth (1- past) (command-arglist command)))
	 (arg (first-mandatory-or-non-flag-arg past (command-arglist command)))
	 (func (and arg (arg-completion-function arg))))
    (dbugf 'completion "command arg : context = ~s pos = ~s fake-word = ~s~%~
                        shell-word = ~s word-num = ~s word-pos = ~s arg = ~s ~
                        func = ~s~%"
	   context pos fake-word shell-word word-num word-pos arg func)
    (cond
      ((and shell-word (> word-pos 1)
;;;	    (char= (char word (1- word-pos)) #\-)
;;;	    (char= (char word (- word-pos 2)) #\-))
	    (is-flag-char (char word 0))
	    (is-flag-char (char word 1)))
       ;; double dash args
       (if all
	   (show-double-dash-arglist (command-arglist command))
	   (progn
	     (complete-double-dash-arglist word word-pos
					   (command-arglist command)))))
      ((and all word-pos
	    (> word-pos 0)
	    ;;(is-flag-char (char word (1- (min word-pos (length word)))))
	    (is-flag-char (char word 0)))
       ;; dash arg enumeration
       (show-dash-arglist (command-arglist command)))
      ((and (not all) word-pos
	    (> word-pos 0)
	    (is-flag-char (char word 0)))
       (complete-dash-arglist word word-pos (command-arglist command)))
      (func
       (dbugf 'completion
	      "---> (~a ~s ~s ~s )~%" func fake-word (length fake-word) all)
       ;; I don't want to make all the arg completion functions have to use
       ;; completion-result, but will this suffice? Or will it lose something?
       ;; @@@
       (let ((result
	      (funcall func fake-word (length fake-word) all :parsed-exp expr)))
	 (if (completion-result-p result)
	     result
	     (make-completion-result
	      :completion result
	      :count (length result) ;; @@@ redundant?
	      ))))
      (t
       (let ((doc (and arg (documentation (type-of arg) 'type)))
	     (choices (and arg (argument-choices arg))))
	 (let ((*print-lines* 10))
	   (dbugf 'completion "not dash or func : wazzup?~%~
                               fake-word = ~s choices = ~w ~%"
		  fake-word choices))
	 (if all
	     (progn
	       #| (print-values* (command expr pos all word-num word)) |#
	       (dbugf 'completion "show all : ummm... past = ~a~%" past)
	       (if (and (= past 1) (not word-num))
		   (progn
		     (dbugf 'completion "snoo ~a? words-past ~a~%" command past)
		     (list-arg-choices command doc choices))
		   (progn
		     (if (and fake-word choices)
			 (complete-list fake-word
					(length fake-word) all choices)
			 (shell-complete-filename fake-word pos all)))))
	     (progn
	       (dbugf 'completion "not all : fake-word ~s" fake-word)
	       (if choices
		   (complete-list fake-word (length fake-word) all choices)
		   (shell-complete-filename fake-word pos all)))))))))

(defun start-of-a-compound-p (expr pos)
  "Return true if we are at the start of the last compound command."
  (and (= (length (shell-expr-words expr)) 1)
       (consp (first (shell-expr-words expr)))
       (keywordp (first (first (shell-expr-words expr))))
       (>= pos (length (shell-expr-line expr)))))

(defun in-command-position-p (expr word-num)
  (or
   ;; first word in a line
   (= word-num 0)
   ;; first word after a compound command
   (and (= (length (shell-expr-words expr)) 2)
	(consp (first (shell-expr-words expr)))
	(keywordp (first (first (shell-expr-words expr))))
	(= word-num 1))))

(defun try-command (command-name)
  "See if we can dig up the dirt on a command named COMMAND.
Uses the first available of:
  - an already loaded command
  - a command which we load by the normal command path mechanism
  - a pre-defined external command, from an external command cache
  - a mined external command"
  (assert command-name)			; Don't be calling this with NIL.
  (let ((command (get-command command-name)))
    (typecase command
      (autoloaded-command
       (load-lisp-command-from command-name
			       (command-load-from command)
			       :silent (lish-autoload-quietly *shell*))
       (get-command command-name))
      (command command)
      (null
       (or
	(and (or (not *shell*) (lish-autoload-from-asdf *shell*))
	     (in-lisp-path command-name)
	     (load-lisp-command command-name
				:silent (lish-autoload-quietly *shell*))
	     (get-command command-name))
	;;(and (load-external-command command) (get-command command))
	(and (mine-command command-name :quietly t)
	     (get-command command-name)))))))

(defun first-command-of (word)
  (cond
    ((and (consp word)
	  (member (car word) '(:redirect-from :redirect-to)))
     (first-command-of (cadr word)))
    ((typep word 'shell-word)
     (first-command-of (shell-word-word word)))
    ((typep word 'shell-expr)
     (first-command-of (first (shell-expr-words word))))
    ;; ((stringp word)
    ;;  word)
    (t ;; some other thing??
     word)))

(defun get-backward-word-symbol (type string position &key bang-p)
  "Get a symbol prior to POSITION in STRING. If BANG-P is true, ignore a leading
exclamation point '!'. Return the TYPE given, the symbol as a string, the
package, and boolean indicating if the package was with the external notation."
  (let* ((word-start (scan-over-string
		      string position :backward
		      :not-in completion::*lisp-non-word-chars*))
	 (word (subseq string word-start position))
	 (package nil)
	 (external nil))
    (when (and bang-p (plusp (length word)) (eql #\! (aref word 0)))
      (setf word (subseq word 1)
	    word-start (1+ word-start)))
    (multiple-value-setq (package external)
      (completion::find-back-pack string word-start))
    (values type word package external)))

;; @@@ consider consolidation of this and what's in colorize-expr
;; @@@ or just wait until parse

(defun guess-word-before (string position)
  "Try to guess what the word before POSTION in STRING is.
First value is a keyword indicating the type, which is one of:
  :symbol :bang-symbol :string :command :filename :command-or-symbol :unknown
  :environment-variable :user-name
Second value is the word as a string.
Third and fourth values depend on the type.
Sorry you'll have to figure it out yourself."
  (let (exp explanation)
    (multiple-value-setq (exp explanation)
      (ignore-errors (shell-read (subseq string 0 position) :partial t
				 :package *junk-package*)))
      (etypecase exp
	(keyword
	 (cond
	   ;; Couldn't read a whole expression.
	   ((eq exp *continue-symbol*)
	    ;; If it's not something we know about, it's probably a bug.
	    (ecase (car explanation)
	      (lisp-expr	      ; an incomplete lisp expression
	       ;; (cdr explanation) should be the expr?
	       (get-backward-word-symbol :symbol string position))
	      (bang-expr	      ; an incomplete !lisp expression
	       ;; (cdr explanation) should be the expr?
	       (get-backward-word-symbol :bang-symbol string position :bang-p t))
	      (string	       ; an unclosed string
	       (let ((word (de-quotify (second explanation)))
		     (position (third explanation)))
		 (values :string word position)))
	      (compound		  ; a compound connector with nothing after it
	       (case (cadr explanation)
		 ((:pipe :and :or :sequence)
		  (values :command ""))
		 ((:redirect-to :redirect-from :append-to)
		  (values :filename ""))))))
	   (t ;; This is probably a bug.
	    (error "Unknown keyword returned from shell-read."))))
	(cons
	 (get-backward-word-symbol :symbol string position))
	(shell-expr
	 (let* ((shell-word (shell-word-at exp position))
		(word (and shell-word (word-word shell-word)))
		(word-num (word-word (shell-word-num exp position)))
		(first-word (word-word (first-word-in-expr exp position)))
		#| word-pos |#)
	   ;; word is the text of the word
	   ;; word-pos is the relative position in the word
	   ;; (when shell-word
	   ;;   (setf word-pos (- position (shell-word-start shell-word))))
	   (cond
	     ((or (and (not word) (zerop position))
		  (start-of-a-compound-p exp position))
	      ;; no words
	      (values :command ""))
	     ((not word)
	      (if (= 0 (length (shell-expr-words exp)))
		  ;; probably ()
		  (progn
		    (dbugf 'completion "not in a word, empty list : bogo~%")
		    (get-backward-word-symbol :symbol string position))
		  ;; a blank spot somewhere in the line
		  (values :command-or-symbol
			  first-word
			  ;; (shell-word-start shell-word)
			  position
			  )))
	     ((symbolp word)
	      (dbugf 'completion "symbol word : janky~%")
	      (get-backward-word-symbol :symbol string position))
	     ((consp word)		; (foo)
	      (dbugf 'completion "junky~%")
	      (get-backward-word-symbol :symbol string position))
	     ((not (stringp word))
	      ;; We don't know how to complete it.
	      ;; Of course we could implement completion for random typed
	      ;; objects, like with a method, but what would that be like?
	      ;; We could error here, that's probably just annoying.
	      (dbugf 'completion "word of weird type ~s ~s~%"
		     word (type-of word))
	      (values :unknown (type-of word) word))
	     ((zerop (length word))
	      (dbugf 'completion "empty word~%")
	      ;; It's probably !() or something so just list the symbols.
	      (get-backward-word-symbol :symbol string position))
	     ((eql (aref word 0) #\()	; (foo
	      (dbugf 'completion "half baka~%")
	      (get-backward-word-symbol :symbol string position))
	     ((eql (aref word 0) #\!)	; !foo
	      (get-backward-word-symbol :symbol string position :bang-p t))
	     ((eql (aref word 0) #\$)	; $foo
	      (values :environment-variable (subseq word 1)
		      (1+ (shell-word-start shell-word))))
	     ((and (eql (aref word 0) #\~) ; ~foo
		   (valid-user-name (subseq word 1)))
	      (values :user-name (subseq word 1)
		      (1+ (shell-word-start shell-word))))
	     ;; first word, when not starting with directory chars
	     ((and
	       (in-command-position-p exp word-num)
	       (not (position (aref word 0) "/.~")))
	      (dbugf 'completion "first word, non path : jinky~%")
	      ;; (get-backward-word-symbol :symbol string position))
	      ;; try commands
	      (values :command-or-symbol
		      first-word (shell-word-start shell-word)))
	     (t
	      (values :command-or-symbol first-word position))))))))

(defun shell-complete (context pos all)
  (declare (type string context))
  "Analyze the context and try figure out what kind of thing we want to ~
complete, and call the appropriate completion function."
  (dbugf 'completion "shell-complete ~s ~s ~s~%" context pos all)
  (let (exp explanation cmd)
    (multiple-value-setq (exp explanation)
      (ignore-errors (shell-read context :partial t
				 :package *junk-package*)))
    (dbugf 'completion "exp ~s is a ~a~%" exp (type-of exp))
    (flet ((simple-complete (func word wpos)
	     (let ((result (funcall func word all)))
	       (if all
		   (setf (completion-result-count result)
			 (length (completion-result-completion result)))
		   (setf (completion-result-insert-position result)
			 wpos))
	       result)))
      (typecase exp
	(keyword
	 (cond
	   ;; Couldn't read a whole expression.
	   ((eq exp *continue-symbol*)
	    ;; If it's not something we know about, it's probably a bug.
	    (ecase (car explanation)
	      (lisp-expr	      ; an incomplete lisp expression
	       ;; (cdr explanation) should be the expr?
	       (dbugf 'completion "partial lisp-expr ~s ~s~%" context pos)
	       (shell-complete-symbol context pos all))
	      (bang-expr	      ; an incomplete !lisp expression
	       ;; (cdr explanation) should be the expr?
	       (dbugf 'completion "partial bang-expr ~s ~s~%" context pos)
	       (shell-complete-symbol context pos all))
	      (string	       ; an unclosed string
	       ;; This is lame. Anything could be in a string.
	       (dbugf 'completion "partial string ~s ~s~%" (second explanation)
		      (third explanation))
	       (simple-complete #'shell-complete-filename (second explanation)
				(third explanation)))
	      (compound		  ; a compound connector with nothing after it
	       (dbugf 'completion "partial compound ~s~%" (cdr explanation))
	       (case (cadr explanation)
		 ((:pipe :and :or :sequence)
		  (dbugf 'completion "whut?~%")
		  (prog1 (simple-complete #'complete-command "" pos)
		    (dbugf 'completion "why?~%")))
		 ((:redirect-to :redirect-from :append-to)
		  (shell-complete-filename "" 0 all))))))
	   (t ;; This is probably a bug.
	    (error "Unknown keyword returned from shell-read."))))
	(cons
	 (dbugf 'completion "Hellow I am janky! : symbol completion~%")
	 (shell-complete-symbol context pos all))
	(shell-expr
	 (let* ((shell-word (shell-word-at exp pos))
		(word (and shell-word (word-word shell-word)))
		(word-num (word-word (shell-word-num exp pos)))
		(first-word (word-word (first-word-in-expr exp pos)))
		 word-pos)
	   ;; word is the text of the word
	   ;; word-pos is the relative position in the word
	   (when shell-word
	     (setf word-pos (- pos (shell-word-start shell-word))))
	   (dbugf 'completion
		  "~%expr completion~%word = ~w word-pos = ~w shell-word ~w~%"
		  word word-pos shell-word)
	   (cond
	     ((or (and (not word) (zerop pos))
		  (start-of-a-compound-p exp pos))
	      ;; no words
	      (dbugf 'completion "no words~%")
	      (simple-complete #'complete-command "" 0))
	     ((not word)
	      (if (= 0 (length (shell-expr-words exp)))
		  ;; probably ()
		  (progn
		    (dbugf 'completion "not in a word, empty list : bogo~%")
		    (shell-complete-symbol context pos all))
		  ;; a blank spot somewhere in the line
		  (let ((from-end (- (length context) pos)))
		    (dbugf 'completion "blank spot : heyba ~a~%" first-word)
		    (let ((result
			   (if (and first-word
				    (setf cmd
					  (try-command
					   (first-command-of first-word))))
			       (progn
				 (dbugf 'completion "in a command : Baaa~%")
				 (complete-command-arg context cmd exp pos all))
			       (shell-complete-filename
				word
				(- (length word) from-end)
				all))))
		      (setf (completion-result-insert-position result)
			    (or (and shell-word
				     (shell-word-start shell-word))
				pos))
		      result))))
	     ((symbolp word)
	      (dbugf 'completion "symbol word : janky~%")
	      (shell-complete-symbol context pos all t))
	     ((consp word)		; (foo)
	      (dbugf 'completion "junky~%")
	      (shell-complete-symbol context pos all))
	     ((characterp word)
	      (dbugf 'completion "real char~%")
	      (simple-complete #'complete-char-name
			       (char-name word)
			       (+ (shell-word-start shell-word) 2)))
	     ((not (stringp word))
	      ;; We don't know how to complete it.
	      ;; Of course we could implement completion for random typed
	      ;; objects, like with a method, but what would that be like?
	      ;; We could error here, that's probably just annoying.
	      (dbugf 'completion "word of weird type ~s ~s~%"
		     word (type-of word))
	      (make-completion-result))
	     ((zerop (length word))
	      (dbugf 'completion "empty word~%")
	      ;; It's probably !() or something so just list the symbols.
	      (shell-complete-symbol context pos all))
	     ((eql (aref word 0) #\()	; (foo
	      (dbugf 'completion "half baka~%")
	      (shell-complete-symbol context pos all))
	     ((eql (aref word 0) #\!)	; !foo
	      (shell-complete-symbol context pos all t))
	     ((eql (aref word 0) #\$)	; $foo
	      (simple-complete #'complete-env-var
			       (subseq word 1)
			       (1+ (shell-word-start shell-word))))
	     ((and (eql (aref word 0) #\~) ; ~foo
		   (valid-user-name (subseq word 1)))
	      (simple-complete #'complete-user-name
			       (subseq word 1)
			       (1+ (shell-word-start shell-word))))
	     ((and (eql (aref word 0) #\#) ; #\
		   (> (length word) 1)
		   (eql (aref word 1) #\\))
	      (dbugf 'completion "bogo char~%")
	      (simple-complete #'complete-char-name
			       (subseq word 2)
			       (+ (shell-word-start shell-word) 2)))
	     ;; first word, when not starting with directory chars
	     ((and
	       (in-command-position-p exp word-num)
	       (not (position (aref word 0) "/.~")))
	      (dbugf 'completion "first word, non path : jinky~%")
	      ;; try commands
	      (let ((result 
		     (simple-complete #'complete-command
				      first-word ;; was: context
				      (shell-word-start shell-word))))
		;; then symbols
		;; XXX Symbols won't come up in the list.
		(when (not (completion-result-completion result))
		  (setf result
			(shell-complete-symbol context pos all)))
		result))
	     (t
	      (dbugf 'completion "nothing special : hello ~a ~a~%"
		     word first-word)
	      (let* ((from-end (- (length context) pos))
		     (result
		      (if (and first-word
			       (setf cmd (try-command
					  (first-command-of first-word))))
			  (progn
			    (dbugf 'completion "command : blurgg~%")
			    (complete-command-arg
			     context cmd exp pos
			     #| (- (length word) from-end) |#
			     all word-num word
			     ;; @@@ band-aid : this code's days are numbered
			     ;; bug shows up e.g. : foo "+<tab>
			     (min word-pos (length word))
			     ))
			  (progn
			    (dbugf 'completion "default to filename : jorky~%")
			    (shell-complete-filename
			     word (- (length word) from-end) all)))))
		(dbugf 'completion "result = ~s~%" result)
		(if all
		    (setf (completion-result-count result)
			  (length (completion-result-completion result)))
		    (setf (completion-result-insert-position result)
			  (let ((ss (shell-word-start shell-word)))
			    (if (shell-word-quoted shell-word)
				(1+ ss) ss))))
		result)))))))))

;; EOF
