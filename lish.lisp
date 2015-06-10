;;
;; lish.lisp - Unix Shell & Lisp somehow smushed together
;;

;; $Revision: 1.15 $

(in-package :lish)

;(declaim (optimize (debug 3)))
(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0) (compilation-speed 0)))
;(declaim (optimize (speed 3) (safety 3) (debug 3) (space 0) (compilation-speed 0)))

#| OLD Way of doing options:

;; This should be where anything that is designed to be settable by the
;; operator should be put to separate it out from internal things in the
;; shell object.
(defclass shell-options ()
  ((prompt-char
    :initarg :prompt-char
    :accessor lish-prompt-char
    :documentation "Normal prompt character.")
   (prompt-string
    :initarg :prompt-string
    :accessor lish-prompt-string
    :documentation "Normal prompt string.")
   (prompt-function
    :initarg :prompt-function
    :accessor lish-prompt-function
    :documentation "Function returning the prompt string.")
   (sub-prompt
    :initarg :sub-prompt
    :accessor lish-sub-prompt
    :documentation "Prompt for continuation lines.")
   (ignore-eof
    :initarg :ignore-eof
    :accessor lish-ignore-eof :initform nil
    :documentation
    "True to ignore when the operator presses ^D. If a number, the count of how~
     many times to ignore ^D before exiting.")
   (debug
    :initarg :debug
    :accessor lish-debug
    :documentation "True to enter the debugger on errors in lish."))
  (:default-initargs
   :prompt-char #\@
   :prompt-string nil
   :prompt-function #'make-prompt
   :sub-prompt "- "	; @@@ maybe we need sub-prompt-char & sub-prompt-func?
   :debug nil)
  (:documentation "User options for the shell."))

(defmethod initialize-instance :after
    ((o shell-options) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  )
|#

(defclass shell ()
  ((exit-flag
    :initarg :exit-flag
    :accessor lish-exit-flag
    :documentation "Set to true to exit the shell.")
   (exit-values
    :initarg :exit-values
    :accessor lish-exit-values
    :documentation "List of values to return to the caller.")
   (aliases
    :accessor lish-aliases
    :documentation "Hash table of aliases.")
   (global-aliases
    :accessor lish-global-aliases
    :documentation "Hash table of global aliases.")
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
   (options
    :initarg :options :accessor lish-options :initform nil
    :documentation "Operator configurable options.")
   )
  (:default-initargs
   :exit-flag nil
   :exit-values '())
  (:documentation "A lispy system command shell."))

(defparameter *options* nil
  "List of options defined.")

(defmethod initialize-instance :after
    ((sh shell) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
;  (setf (slot-value sh 'commands) (make-hash-table :test #'equal))
  (setf (slot-value sh 'aliases) (make-hash-table :test #'equal))
  (setf (slot-value sh 'global-aliases) (make-hash-table :test #'equal))
  #| OLD style
  (setf (slot-value sh 'options)
	(apply #'make-instance 'shell-options initargs)) |#
  ;; Copy the objecs from the defined option list, and set the default values.
  (loop :with o :for opt :in *options* :do
     (setf o (shallow-copy-object opt)
	   (arg-value o) (arg-default o))
     (push o (lish-options sh)))
  (init-commands))

;; We think of options like they are arguments for the shell, and use
;; the argument class to store them. That way we can use the same completion
;; and conversion.

(defun find-option (sh name)
  "Find the option of the shell SH, named NAME. Error if there is none."
  (or (find name (lish-options sh) :key #'arg-name :test #'equalp)
      (error "No such option ~w" name)))

(defun set-option (sh name value)
  "Set the option named NAME, for shell SH, to VALUE."
  (setf (arg-value (find-option sh name)) value))

(defun get-option (sh name)
  "Get the option named NAME, for shell SH."
  (arg-value (find-option sh name)))

(defmacro defoption (name &rest arg)
  "Define a shell option named NAME, with the properties in arg. The syntax
is like Lish arguments, e.g.:
  (defoption \"foo\" type :help \"Make sure to foo.\" :short-arg #\\f)"
  (let ((sym (symbolify (s+ "LISH-" name)))
	(name-string (string-downcase name)))
    `(progn
       ;; Access options as if they were in the shell object.
       (defmethod ,sym ((sh shell)) (get-option sh ,name-string))
       (defmethod (setf ,sym) (value (sh shell))
	 (set-option sh ,name-string value))
       (push (make-argument ',(cons name-string arg))
	     *options*))))

(defoption prompt-char character
  :help "Normal prompt character. Output if there is no prompt string."
  :default #\@)
(defoption prompt-string string
  :help "Normal prompt string. Output if there is no prompt function. Output
with FORMAT-PROMPT, which see."
  :default nil)
(defoption prompt-function function
  :help "Function which takes a SHELL and returns a string to output as the
prompt."
  :default #'make-prompt)
(defoption sub-prompt string
  :help "String to print when prompting for more input."
  :default "- ")	; @@@ maybe we need sub-prompt-char & sub-prompt-func?
(defoption ignore-eof integer
  :help "If true, prevent the EOF (^D) character from exiting the shell. If a 
number ignore it that many times before exiting."
  :default nil)
(defoption debug boolean
  :help "True to enter the debugger when there is an error."
  :default nil)

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
%i      The lisp implementation nickname.
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
	       (#\$ (write-char (if (= (nos:geteuid) 0) #\# #\$) str))
	       (#\i (write-string *lisp-implementation-nickname* str))
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
  (or (and ;(slot-boundp (lish-options sh) 'prompt-string)
       (lish-prompt-string sh)
       (format-prompt sh (lish-prompt-string sh)))
      (format nil "~a " (make-string (+ 1 *lish-level*)
				     :initial-element (lish-prompt-char sh)))))

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

#|
 
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@@@@@@@ DREADFUL

;;;;;;;;
    (labels ((finish-word ()
	       "Finish the current word."
	       (when in-word
		 (push (copy-seq w) args)
		 (push i word-end)
		 (push did-quote word-quoted)
		 (setf (fill-pointer w) 0
		       in-word nil
		       did-quote nil)))
	     (return-partial ()
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
	     (do-continue ()
	       "Handle when the expression is incomplete."
	       (if partial
		   (return-partial)
		   (return-from shell-read *continue-symbol*)))
	     (do-reader-error (c)
	       "Handle when the expression has an error."
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
		     words       (nreverse args)))
	     (make-the-expr ()
	       "Make an expression, with it's own copy of the lists."
	       (make-shell-expr
		:line line
		:words (copy-seq words)
		:word-start (copy-seq word-start)
		:word-end (copy-seq word-end)
		:word-quoted (copy-seq word-quoted)))
	     (make-compound (key &optional (inc 2))
	       "Make a compound expression with type KEY."
	       (finish-word)
	       (reverse-things)
	       (let ((e (list key (make-the-expr))))
		 (setf args (list e)))
	       (setf word-start (list i))
	       (incf i inc)
	       (setf word-end (list i)
		     word-quoted (list nil))))

;;;;;;;;

(defmacro with-words ((line) &body body)
  "Do something for each word in the line."
  (with-unique-names (word in-word do-quote did-quote i len)
    `(let ((,word (make-stretchy-string 12))
	 (,i 0)	(,len (length line)) ; index in line
	 ,in-word ,do-quote ,did-quote ,c
	 word)
      (loop :while (< ,i ,len) :do
	 (setf ,c (aref ,line ,i))
	 (cond
	   ;; quoted char
	   (do-quote
	       (vector-push-extend c ,word)
	     (when (not in-word)
	       (push (1- i) word-start))
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
	      (end-of-file () (do-continue))
	      (reader-error (c) (do-reader-error c))
;	      (error () (do-continue))
;	      (condition (c) (signal c))
	      ))
	   ;; a lisp expr
	   ((eql c #\!)
	    (finish-word)
	    ;; read a form as a separate word
	    (handler-case
		(multiple-value-bind (obj pos)
		    (with-package package
		      (read-from-string line nil nil :start (+ i 1)))
		  (push i word-start)
		  (setf i pos)
		  (push i word-end)
		  (push nil word-quoted)
		  (push obj args))
	      (end-of-file () (do-continue))
;	      (error () (do-continue))
;	      (end-of-file () (do-continue))
;	      (condition (c) (signal c))
	      ))
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
		  word-quoted (list nil)))
	   ((and (eql c #\&) (eql (next-char) #\&))
	    (make-compound :and))
	   ((and (eql c #\|) (eql (next-char) #\|))
	    (make-compound :or))
	   ((eql c #\^)
	    (make-compound :sequence 1))
	   ;; any other character: add to word
	   (t
	    (when (not in-word)
	      (push i word-start))
	    (setf in-word t)
	    (vector-push-extend c w)
	    (incf i)))

(defun expand-global-aliases (line)
  "Expand global aliases in the line."
  line)

@@@@@@@@@ DREADFUL
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

|#

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
;  (setf line (expand-global-aliases line))
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
	     (return-partial ()
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
	     (do-continue ()
	       "Handle when the expression is incomplete."
	       (if partial
		   (return-partial)
		   (return-from shell-read *continue-symbol*)))
	     (do-reader-error (c)
	       "Handle when the expression has an error."
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
		     words       (nreverse args)))
	     (make-the-expr ()
	       "Make an expression, with it's own copy of the lists."
	       (make-shell-expr
		:line line
		:words (copy-seq words)
		:word-start (copy-seq word-start)
		:word-end (copy-seq word-end)
		:word-quoted (copy-seq word-quoted)))
	     (make-compound (key &optional (inc 2))
	       "Make a compound expression with type KEY."
	       (finish-word)
	       (reverse-things)
	       (let ((e (list key (make-the-expr))))
		 (setf args (list e)))
	       (setf word-start (list i))
	       (incf i inc)
	       (setf word-end (list i)
		     word-quoted (list nil))))
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
;	     (setf did-quote t)
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
	      (end-of-file () (do-continue))
	      (reader-error (c) (do-reader-error c))
;	      (error () (do-continue))
;	      (condition (c) (signal c))
	      ))
	   ;; a lisp expr
	   ((eql c #\!)
	    (finish-word)
	    ;; read a form as a separate word
	    (handler-case
		(multiple-value-bind (obj pos)
		    (with-package package
		      (read-from-string line nil nil :start (+ i 1)))
		  (push i word-start)
		  (setf i pos)
		  (push i word-end)
		  (push nil word-quoted)
		  (push obj args))
	      (end-of-file () (do-continue))
;	      (error () (do-continue))
;	      (end-of-file () (do-continue))
;	      (condition (c) (signal c))
	      ))
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
		  word-quoted (list nil)))
	   ((and (eql c #\&) (eql (next-char) #\&))
	    (make-compound :and))
	   ((and (eql c #\|) (eql (next-char) #\|))
	    (make-compound :or))
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
  "Return true if a command can be found by ASDF."
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

(defun do-system-command (command-line
			  &optional in-pipe out-pipe
			    (environment nil env-p))
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
	  (if (or in-pipe out-pipe)
	      (progn
		(setf result-stream
		      (apply #'nos:popen
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
    (loop :with v
       :for w :in (shell-expr-words expr)
       :for i = 0 :then (1+ i)
       :do
       (if (unquoted-string-p w expr i)
	 (cond
	   ;; $ environment variable expansion
	   ((eql #\$ (aref w 0))
	    (setf v (nos:getenv (subseq w 1)))
	    (push (or v "") new-words))
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
	   ;; @@@ This is probably horribly inefficient.
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
  (let* ((cmd (elt (shell-expr-words expr) 0))
	 #| (args (subseq (shell-expr-words expr) 1)) |#
	 ;; (command (gethash cmd (lish-commands)))
	 (command (get-command cmd))
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
       (record-command-stats cmd :command)
       (call-command command (subseq expanded-words 1) in-pipe out-pipe))
      ;; Lish command
      (command			
       (record-command-stats cmd :command)
       (call-command command (subseq expanded-words 1) in-pipe out-pipe))
      (t
       (flet ((sys-cmd ()
		"Do a system command."
		(record-command-stats (first expanded-words) :system-command)
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
		   (progn
		     (record-command-stats (first expanded-words) :function)
		     (values
		      (multiple-value-list
		       (apply (symbol-function symb)
			      (read-parenless-args
			       (subseq (shell-expr-line expr) pos))))
		      nil ;; stream
		      t))  ;; show the values
		   ;; Just try a system command anyway, which will likely fail.
		   (sys-cmd)))))))))

(defun successful (obj)
  "Return true if the object represents a successful command result."
  (or
   ;; Zero return value from a system command?
   (and obj (and (numberp obj) (zerop obj)))
   t)) ;; Any other value from lisp code or commands/

(defun shell-eval (sh expr &key no-alias in-pipe out-pipe)
  "Evaluate the shell expression EXPR. If NO-ALIAS is true, don't expand
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
			;; @@@ perhaps we should retain original,
			;; since indexes not adjusted?
			:line (format nil "~{~a ~}"
				      (cdr (shell-expr-words expr))))
		       :no-alias no-alias
		       :in-pipe out-stream
		       :out-pipe out-pipe))))))
    (typecase expr
      (shell-expr
       ;; Quick return when no words
       (when (= (length (shell-expr-words expr)) 0)
	 (return-from shell-eval (values nil nil nil)))
       (let ((w0 (elt (shell-expr-words expr) 0)))
	 (do-expansions sh expr 0)
	 (dbug "~w~%" expr)
	 (if (listp w0)
	     ;; Compound command
	     (case (first w0)
	       (:pipe     (eval-compound (successful vals) t))
	       (:and      (eval-compound (successful vals) nil))
	       (:or       (eval-compound (not (successful vals)) nil))
	       (:sequence (eval-compound t nil)))
	     ;; Not a list, a ‘simple’ command
	     (with-package *lish-user-package*
	       (shell-eval-command sh expr
				   :no-alias no-alias
				   :in-pipe in-pipe :out-pipe out-pipe)))))
      (t ;; A full Lisp expression all by itself
       (with-package *lish-user-package*
	 (values (multiple-value-list (eval expr)) nil t))))))

(defun load-file (sh file)
  "Load a lish syntax file."
  (with-open-file (stream file :direction :input)
    (with-package *lish-user-package*
      (loop :with line = nil :and new-line = t :and expr = nil
	 :while (and (setf line (read-line stream nil))
		     new-line)
	 :do
	 (loop :while (and (eql (setf expr (shell-read line))
				*continue-symbol*)
			   (setf new-line (read-line stream nil)))
	    :do
	    (setf line (format nil "~a~%~a" line new-line)))
	 (shell-eval sh expr)))))
      ;; (loop :while (setf line (read-line stream nil))
      ;; 	 :do
      ;; 	 (if (eql line *continue-symbol*)
      ;; 	     (shell-read line

(defun load-rc-file (sh)
  "Load the users start up (a.k.a. run commands) file, if it exists."
;;;  (without-warning
  (let ((file (merge-pathnames (user-homedir-pathname)
			       (make-pathname :name ".lishrc"))))
    (when (probe-file file)
      (load-file sh file))))

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
  "Read a string with the line editor and convert it shell expressions,
handling errors."
  (with-slots ((str string) (pre-str prefix-string)) state
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
	     #+sbcl (sb-ext::step-condition 'tiny-rl::repple-stepper)
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
			   (if (lish-prompt-function sh)
			       (funcall (lish-prompt-function sh) sh)
			       (make-prompt sh))))))
	  (cond
	    ((and (stringp str) (equal 0 (length str))) *empty-symbol*)
	    ((equal str *real-eof-symbol*)		*real-eof-symbol*)
	    ((equal str *quit-symbol*)	  		*quit-symbol*)
	    (t (shell-read (if pre-str
			       (s+ pre-str #\newline str)
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
		:with result = nil
		:and lvl = *lish-level*
		:and eof-count = 0
		:if (lish-exit-flag sh)
		  :return (values-list (lish-exit-values sh))
		:end
		:do
		(restart-case
		    (progn
		      (setf result (lish-read sh state))
		      (when (eq result *real-eof-symbol*)
			(return-from pippy result))
		      (if (eq result *quit-symbol*)
			  (if (not (lish-ignore-eof sh))
			      (return-from pippy result)
			      (progn
				(when (numberp (lish-ignore-eof sh))
				  (if (< eof-count (lish-ignore-eof sh))
				      (incf eof-count)
				      (return-from pippy result)))
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
    (save-command-stats)
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

(defun make-standalone (&optional (name "lish"))
  "FUFKFUFUFUFUFF"
  (update-version)
  (save-image-and-exit name #'lish:shell-toplevel))

;; So we can conditionalize adding of lish commands in other packages.
(d-add-feature :lish)

;; EOF
