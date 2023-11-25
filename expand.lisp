;;;
;;; expand.lisp - Shell expansions.
;;;

(in-package :lish)

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
		;; (push obj results)
		(push word results)
		))
	(end-of-file ())
	(reader-error ()))
      results))
   word))

(defun expr-from-args (args)
  "Return a shell expression made up of ‘args’ as the words."
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
	   (let ((expansion (glob:glob string :tilde t)))
	     ;; If expansion fails we have to just return the pattern string.
	     (if expansion
		 (make-file-expansion :files expansion)
		 string)))
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
	     (alias (gethash cmd (shell-aliases sh))))
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
	     (t (push w new-words))
	     ;; (t (push (make-shell-word :word w) new-words))
	     ))
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

;; End
