;;
;; commands.lisp - How Lish does commands
;;

;; This defines structure of commands and command arguments for Lish.
;; Lish commands have thier own style of arguments, which are different than
;; Lisp function arguments and Unix command arguments, but can be converted
;; between them. This allows us to keep useful information about command
;; arguments, but still treat Unix programs and Lisp functions as commands.
;; The extra infomation about arguments is most useful for completion and
;; input help.
;;
;; The point is to give the user a lot of assistance entering commands.

(in-package :lish)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(define-condition shell-error (error)
  ((format
    :accessor shell-error-format
    :initarg :format
    :type string
    :documentation "Format control for error reporting.")
   (arguments
    :accessor shell-error-arguments
    :initarg :arguments :initform nil
    :type list
    :documentation "Format arguments for error reporting."))
  (:report (lambda (c s)
	     (when (shell-error-format c)
	       (format s "~?"
		       (shell-error-format c)
		       (shell-error-arguments c)))))
  (:documentation "An error ocurring in the shell."))

(define-condition unknown-command-error (shell-error)
  ((command-string
    :accessor unknown-command-error-command-string
    :initarg :command-string
    :type string
    :documentation "Name of the unknown command."))
  (:report (lambda (c s)
	     (if (shell-error-format c)
		 (format s "~a ~?"
			 (unknown-command-error-command-string c)
			 (shell-error-format c)
			 (shell-error-arguments c))
		 (format s "~a not found"
			 (unknown-command-error-command-string c)))))
  (:documentation "Tried to execute an unknown command."))

(deftype function-designator ()
  "Something that denotes a function."
  `(or function symbol null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands

(defclass command ()
  ((name
    :accessor command-name        :initarg :name
   :documentation "The string word that invokes the command.")
   (function
    :accessor command-function    :initarg :function
    :documentation "The function that performs the command.")
   (arglist
    :accessor command-arglist     :initarg :arglist
    :documentation "A list of arguments.")
   (handle-unrecognized
    :initarg :handle-unrecognized :accessor command-handle-unrecognized
    :initform nil :type boolean
    :documentation "True to pass unrecogized arguments to the command.")
   (pass-keys-as
    :initarg :pass-keys-as :accessor command-pass-keys-as
    :initform nil :type symbol
    :documentation
    "Argument name to pass all keyword arguments as. NIL if we don't need it.")
   (accepts
    :initarg :accepts :accessor command-accepts :initform :unspecified
    :documentation
    "Sequence of things the command accepts. Probably one of:
     :STREAM		An output stream.
     :GROTTY-STREAM	An output stream which can have terminal decorations.
     :SEQUENCE		A sequnce of objects.
     :UNSPECIFIED	We don't know. This is the default.
     <non-keyword>	A lisp type.
     NIL		Doesn't accept anything.")
   (built-in-p
    :accessor command-built-in-p  :initarg :built-in-p :initform nil
    :documentation "True if the command is considered ‘built in’.")
   (loaded-from
    :accessor command-loaded-from :initarg :loaded-from :initform nil
    :documentation "Where the command was loaded from."))
  (:documentation "A command defined internally in the shell."))

(defmethod initialize-instance :after ((b command) &rest initargs)
  (declare (ignore initargs))
  ;; Make the default function binding from the name
  (if (not (slot-boundp b 'function))
      (setf (slot-value b 'function)
	    (command-function-name (slot-value b 'name)))))

(defmethod print-object ((o command) stream)
  "Print a lish command in an unreadable way."
  (print-unreadable-object (o stream :identity nil :type t)
    ;; (if (slot-boundp o 'synopsis)
    ;; 	(format stream "~s" (command-synopsis o))
    ;; 	(format stream "~s" (if (slot-boundp o 'name)
    ;; 				(command-name o)
    ;; 				(format stream "<unnamed>"))))))
    (format stream "~a" (posix-synopsis o))))

;; (defparameter *initial-commands* nil
;;   "List of initial commands.")

(defvar *command-list* nil
  "List of command names.")

(defvar *lish-commands* nil
  "Hash table of commands. This means that commands are shared by all shell
instances.")

(defun lish-commands ()
  (when (not *lish-commands*)
    (setf *lish-commands* (make-hash-table :test #'equal)))
  *lish-commands*)

(defun set-command (name obj)
  (setf (gethash name (lish-commands)) obj))

(defun unset-command (name)
  (remhash name (lish-commands)))

(defun get-command (name)
  (gethash name (lish-commands)))

(defun init-commands ()
  "This doesn't really do anything anymore, we're just keeping it in case
we want to use it for something in the future."
  (values))
;;   "Set up the *LISH-COMMANDS* hash table, and load it with the commands from
;; *INITIAL-COMMANDS*, which is likely whatever commands were defined with
;; DEFBUILTIN."
;;   (loop :for (k v) :in *initial-commands*
;;      :do (set-command k v)))

(eval-when (:compile-toplevel :load-toplevel) ; needed by defcommand macro
  (defun command-function-name (n)
    "Return the normal command function symbol for the given command name."
    (intern (concatenate 'string "!" (string-upcase n)))))

#|
(defmacro squirrel-def (package &body body)
  (let ((squirrel-package (s+ package "-DEFS"))
	(defs-sym (gensym "SQUIRREL-DEF")))
    `(if (find-package ,package)
	 (progn ,@body)
	 (progn
	   (when (not (find-package ,squirrel-package))
	     (make-package ,squirrel-package)
	     (let ((,defs-sym (intern "*DEFS*"
				      (find-package ,squirrel-package))))
	       (eval `(defvar ,,defs-sym '()))))
	   (let ((,defs-sym (intern "*DEFS*" (find-package ,squirrel-package))))
	     (set ,defs-sym (cons ',body (symbol-value ,defs-sym))))))))
|#

#|
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun eval-defaults (arglist)
    "Evaluate default arguments."
    (loop :for a :in arglist :do
       (loop :for k = (cddr a) :then (cdr
	  (
    ))
|#

(defun ignorable-filter (args)
  "Given a lambda list return a list of variable names to ignore."
  (lambda-list-vars args :all-p t))

;; There should be little distinction between a user defined command and
;; "built-in" command, except perhaps for a warning if you redefine a
;; pre-defined command, and the fact that things defined in the shell are
;; considered "built-in" and listed in help that way.

;;
;; I would wish that defcommand could provide enough documentation to make a
;; man page, or an info page, or a CLHS like page.
;;
;; We should make it easy to indicate man page sections and info links.
;; We should accept some kind of markdown for doc strings.
;; We should automatically generate any sections possible.
;;
;; Man pages have:
;;   NAME		name - Less than one line summary
;;   SYNOPSIS		(should be automatically generated, as in help)
;;   DESCRIPTION	Pretty much the normal docstring.
;;   OPTIONS		List options and describe in detail. Generate from
;;			option docstrings?
;;   EXAMPLES		very useful
;;   FILES		<perhaps not so useful / optional>
;;   ENVIRONMENT	<perhaps not so useful / optional>
;;   DIAGNOSTICS	populated by gleaning errors?
;;   BUGS		Of course we don't need this ;)
;;   AUTHOR		Gleaned from package author
;;   SEE ALSO		this is useful in CLHS, so...
;;
;; Info pages have:
;;   Up, Prev, Next links
;;   Menu sub links
;; 
;; CLHS pages have:
;;   Summary  (type, name, args, and return values)
;;   Arguments and Values
;;   Description
;;   Examples
;;   Exceptional Situations
;;   Side Effects
;;   Affected By
;;   See Also
;;   Notes

;; Don't export stuff because it causes package variance on reloading.
;; @@@ Perhaps we should define commands in the LISH-USER package
;; instead, so they can be exported and used by other packages?

(defparameter *special-body-tags* #(:accepts :keys-as)
  "Keywords with special meanings appearing first in the command body.")

(defmacro %defcommand (name built-in-p (&rest arglist) &body body)
  "See the documentation for DEFCOMMAND."
  (let ((func-name (command-function-name name))
	(name-string (string-downcase name))
	(accepts :unspecified)
	(fixed-body body)
	pass-keys-as params ignorables)
    ;; Pull out special body tags:
    (loop :with tag
       :while (setf tag (find (car fixed-body) *special-body-tags*)) :do
       (case tag
	 (:accepts
	  (setf accepts (cadr fixed-body)
		fixed-body (cddr fixed-body)))
	 (:keys-as
	  (setf pass-keys-as (cadr fixed-body)
		fixed-body (cddr fixed-body)))))
    (setf params (command-to-lisp-args (make-argument-list arglist t)
				       :pass-keys-as pass-keys-as)
	  ignorables
	  (when (and params pass-keys-as)
	    `((declare (ignorable ,@(ignorable-filter params))))))
    `(progn
       (defun ,func-name ,params
	 ,@ignorables
	 ,@fixed-body)
       (pushnew ,name-string lish::*command-list* :test #'equal)
       (set-command ,name-string
		    (make-instance (find-symbol "COMMAND" :lish)
				   :name ,name-string
				   :loaded-from *load-pathname*
				   :built-in-p ,built-in-p
				   :accepts ',accepts
				   :pass-keys-as
				   ,(and pass-keys-as `(quote ,pass-keys-as))
				   :arglist (make-argument-list ',arglist))))))

(defmacro defcommand (name (&rest arglist) &body body)
  "Define a command for the shell.
NAME is the name it is invoked by.
ARGLIST is a shell argument list.
BODY is the body of the function it calls.
BODY recognizes some special keywords:
  :ACCEPTS followed by a single or list of accept keywords to indicate what
           kind of things the command accepts from a pipeline.
  :KEYS-AS followed by a symbol which will be a list of the keywords and values
           given to the command function. "
  `(%defcommand ,name nil (,@arglist) ,@body))

(defmacro defbuiltin (name (&rest arglist) &body body)
  "Just like DEFCOMMAND, except it sets BUILT-IN-P to T."
  `(%defcommand ,name t (,@arglist) ,@body))

(defmacro defexternal (name (&rest arglist))
  "Define an external command for the shell. NAME is the name it is invoked by
and the name of the external program. ARGLIST is a shell argument list."
  `(%defcommand ,name t (,@arglist)
     (! (string-downcase ,name))))

(defun undefine-command (name)
  (unset-command name)
  (setf *command-list* (delete name *command-list*)))

#|
  (defclass arg-list ()
(arg-list-list)
(:documentation "A list of arguments."))

  (defgeneric get-args ()
(:documentation "Return a string to prompt with."))
  (defmethod get-args (arg-list))

  (defgeneric args-help ()
(:documentation "Return a string to prompt with."))
  (defmethod args-help (arg-list))
  |#

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#
#|| 									||#
#|| Let's try something different.					||#
#|| 									||#
#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

;; [ ]     optional
;; ( )     mandatory (not optional)
;; ...     repeating
;; a | b   or
;; foo     literal string "foo", not optional
;; --foo   option "foo"
;; <foo>   type foo : arg-<foo>
;; --      start of positional only

;; We already have :optional and :repeating on individual items.
;; We don't have grouping, which we need for mandatory ( ), and "or".
;; We should implement "--".

;; (:or a b c)                          a | b | c
;; (:opt a b c)                         [ a b c ]
;; (:and a b c)                         ( a b c )
;; (:repeat a b c)                      ( a b c )...
;; (:repeat a)                          a...
;; (:opt (:repeat a))                   [a...]
;; --a :positional b c                  --a -- --a
;; (:or ("foo" (:opt x y z))            foo [x] [y] [z]
;;      ("bar" (:opt a b c)))           bar [a] [b] [c]
;; (:case <foo-cmd>			So we can have type based completion.
;;  (foo (x y z))		        foo [x] [y] [z]
;;  (bar (a b c)))		        bar [a] [b] [c]

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#


#|
  @@@ Perhaps we could make this whole thing simpler by just specifying
  @@@ that all command functions take only keyword arguments, and if you
  @@@ want a lispy-er interface, make it call a different function.
  @@@ In other words, how useful is it to have simple/nice command functions,
  @@@ conviently usable from Lisp, since a Lisp API to the command's
  @@@ functionality will likely be more complicated and/or better served by a
  @@@ separate function. ???

  The rules for converting POSIX arguments to lambda lists are fairly
  complicated. Here we try to examine some of the possiblities. We use a
  letter to denote the category of argument:
  m = manditory  o = optional  r = repeating  f = flagged

  When only manditory and optional are present, we don't need keywords.
  The order of manditories vs optionals doesn't matter to lambda lists,
  but does to POSIX.
  Non-flagged optionals must come after manditories.

  m1 m2		(m1 m2)				m1 m2
  m1 m2 o1 o2	(m1 m2 &optional o1 o2)		m1 m2 [o1] [o2]
  o1 o2 m1 m2 	(m1 m2 &optional o1 o2)		[o1] [o2] m1 m2  problematic???
  o1 o2		(&optional o1 o1)		[o1] [o2]

  We can't have more than one non-flagged repeating:
  ro1		(&rest r1)			[ro1...]
  rm1 rm2	ERROR
  ro1 ro2	ERROR
  m1 m2 ro1	(m1 m2 &rest r1)		m1 m2 [ro2...]
  m1 m2 r1 r2	ERROR
  o1 o2 r1	ERROR (o1 and o2 must be flagged)

  rm1		(&rest r1)			r1[...]
  r1 r2		ERROR
  m1 m2 rm1	(m1 m2 &rest r1)		m1 m2 rm1[...]
  m1 m2 ro1	(m1 m2 &rest r1)		m1 m2 [ro1...]
  m1 m2 r1 r2	ERROR
  of1 of2 rm1	(&key of1 of2 r1)		[-12] r1[...]
  of1 of2 ro1	(&key of1 of2 r1)		[-12] [r1...]

  Flagged optional must be done as keywords:
  m1 m2 of1 o2	(m1 m2 &key of1 o2)		[-1] m1 m2 [o2]
  m1 m2 o1 of2	(m1 m2 &key o1 of2)		[-2] m1 m2 [o1]
  m1 m2 of1 of2	(m1 m2 &key o1 of2)		[-2] m1 m2 [o1]
  of1 o2 m1 m2 	(m1 m2 &key of1 o2)		[-1] m1 m2 [o2]
  of1 of2 m1 m2 (m1 m2 &key of1 of2)		[-12] m1 m2
  of1 of2	(&key of1 of2)			[-12]
  o1 of2	(&key of1 of2)			[-2] [o1]
  of1 o2	(&key of1 of2)			[-1] [o2]

  Flagged manditory must be done as keywords, which DOES'T make other
  manditories keywords.
  Manditory flagged treated as optional flagged, except error afterward if
  not present.

  mf1 m2 of1 o2		(m2 &key mf1 of1 o2)	[-of1] [-mf1] [o2] m2
  m1 mf2 o1 of2		(m1 &key mf2 o1 of2)	[-mf2] [-of2] [o1] m1
  mf1 mf2 of1 of2	(&key mf1 mf2 o1 of2)	[-mf1] [-mf2] [-of2] [o1]
  of1 o2 mf1 m2 	(m2 &key of1 o2 mf1)	[-of1] [-mf1] [o2] m2
  of1 of2 m1 mf2 	(m1 &key of1 of2 mf2)	[-of1] [-of2] [-mf2] m1
  mf1 mf2		(&key mf1 mf2)		[-mf1] [-mf2]

  Repeating flagged: can have more than one, but values can't start with
  dashes!
  Repeating flagged manditory and optional are treated the same.
  rf1		(&rest rf1)			[-rf1 foo] [...]
  (*stupid but legal)
  rf1 rf2	(&key rf1 rf2)			[-rf1 foo...] [-rf2 bar...]
  (*can be given in any order, e.g.: -rf2 foo bar -rf1 foo bar baz)

  Flagged arguments can appear in POSIX in multiple ways:
  (:short-arg x :type boolean)		[-x]
  ((:short-arg x :type boolean)   	[-xy]
(:short-arg y :type boolean))
  (:short-arg x :type (not boolean))	[-x arg]
  ((:short-arg x :type (not boolean))	[-x arg] [-y arg]
(:short-arg y :type (not boolean)))
  (:long-arg foo :type boolean)   	[--foo]
  (:long-arg foo :type (not boolean))   [--foo bar]

  |#

;;
;; If there are any optional non-positional args (i.e. optional args with
;; short-arg or long-arg specified, then all the lisp args must be keyworded.
;;
;; @@@ Make argument type classes work, with specific type
;; validation and completion methods.

(defmacro move-arg (old new i arg)
  "Move the I'th item from the OLD to the NEW list, and return both."
  `(progn
     (setf ,new (push (convert-arg ,arg
				   (shell-word-word (nth ,i ,old))
				   (shell-word-quoted (nth ,i ,old)))
		      ,new)
	   ,old (delete-nth ,i ,old))))

(defun arg-key (arg)
  (intern (string-upcase (arg-name arg)) :keyword))

(defmacro move-key (old new i arg keyworded)
  "Move the I'th item from the OLD to the NEW list, and return both."
  `(progn
     (when ,keyworded
       (setf ,new (push (arg-key ,arg) ,new)))
     (setf ,new (push (convert-arg ,arg
				   (shell-word-word (nth ,i ,old))
				   (shell-word-quoted (nth ,i ,old)))
		      ,new))
     (setf ,old (delete-nth ,i ,old))))

(defmacro push-key (new arg value keyworded)
  "Push a possibly keyworded arg VALUE to the NEW list."
  `(progn
     (when ,keyworded
       (setf ,new (push (arg-key ,arg) ,new)))
     (setf ,new (push (convert-arg ,arg
				   (shell-word-word ,value)
				   (shell-word-quoted ,value)))
	   ,new)))

(defmacro move-flag (old new i arg)
  `(progn
     (setf ,new (push (arg-key ,arg) ,new))
;;;     (format t "(nth (1+ ~s) ~s) = ~s~%" ,i ,old (nth ,i ,old))
;;;     (format t "(convert-arg ~s ~s) = ~s~%" ,arg (nth ,i ,old)
;;;	     (convert-arg ,arg (nth ,i ,old)))
;;;     (setf ,new (push (convert-arg ,arg (nth (1+ ,i) ,old)) ,new))
     (dbugf 'lish-arg "before i=~s old=~s~%" ,i ,old)
     (dbugf 'lish-arg "~s -> ~s~%" (nth (1+ ,i) ,old)
	   (convert-arg ,arg (nth (1+ ,i) ,old)))
     (setf ,new (push (convert-arg ,arg
				   (shell-word-word (nth (1+ ,i) ,old))
				   (shell-word-quoted (nth (1+ ,i) ,old)))
		      ,new))
     (setf ,old (delete-nth ,i ,old)) ; flag
     (setf ,old (delete-nth ,i ,old)) ; arg
     (dbugf 'lish-arg "after i=~s old=~s~%" ,i ,old)
     (setf possible-flags (delete ,arg possible-flags))))

(defmacro move-boolean-2 (old new i arg)
  `(progn
     (setf ,new (push (arg-key ,arg) ,new))
     (setf ,new (push t ,new))
     (setf ,old (delete-nth ,i ,old))))

(defmacro move-boolean (old new i arg val)
  (declare (ignore old i))
  `(progn
     (setf ,new (push (arg-key ,arg) ,new))
     (setf ,new (push ,val ,new))
     (setf possible-flags (delete ,arg possible-flags))))

(defmacro move-repeating (old new start arg keyworded &optional until)
  (let ((e (gensym "move-repeating-e"))
	(did-one (gensym "move-repeating-did-one")))
    `(progn
       (let (,did-one)
	 (if ,until
	     (error "can't do until yet") ;; @@@
	     (progn
	       (if ,keyworded
		   (progn
		     (setf ,new (push (arg-key ,arg) ,new))
		     (setf ,new (push (mapcar
				       #'(lambda (x)
					   (convert-arg
					    ,arg
					    (shell-word-word x)
					    (shell-word-quoted x)))
					      (nthcdr ,start ,old)) ,new))
		     (when (length (nthcdr ,start ,old))
		       (setf ,did-one t)))
		   (loop :for ,e :in (nthcdr ,start ,old) :do
		      (setf ,new
			    (push (convert-arg
				   ,arg
				   (shell-word-word ,e)
				   (shell-word-quoted ,e)) ,new)
			    ,did-one t)))
	       (setf ,old (subseq ,old 0 ,start))
	       ;; Push default if we have one and didn't get any values.
#|	       (when (and (not ,did-one) (arg-default ,arg))
		 (when ,keyworded
		   (setf ,new (push (arg-key ,arg) ,new)))
		 (setf ,new (push (convert-arg
				   ,arg (arg-default ,arg)) ,new))) |#
	       ))))))

;; I used to handle default values to arguments here, but they were not getting
;; evaluated properly, so it's probably best to let the command function handle
;; defaulting arguments. It could be confusing to have two different defaulting
;; mechanisms anyway.
;;
;; @@@ But the problem is that when the command function defun gets constructed
;; at compile time, default arguments from the argument class may not be
;; availible yet if the argument class is defined in the same file, since the
;; class may not be fully defined until the end of the compilation phase?

(defun extract-short-boolean-options (command posix-args)
  "Returns POSIX-ARGS with boolean options removed and as the second value,
a list of the converted boolean options."
  (let* (option-list
	 new-args)
    (loop :with boolean-taken :and boolean-value #| :and flag-taken |#
       :for a :in posix-args
       :when (and (>= (length a) 2)
		  (is-flag-char (char a 0))
		  (not (is-flag-char (char a 1))))
       :do
       (setf boolean-value (is-normal-flag-char (char a 0))
	     #| flag-taken nil |#)
       (loop :for cc :from 1 :below (length a) :do
	  (setf boolean-taken nil)
	  (loop :for arg :in (command-arglist command) :do
	     (if (and (eql (arg-short-arg arg) (char a cc))
		      (eq (arg-type arg) 'boolean))
		 (progn
		   (dbugf 'lish-arg "short boolean arg ~s~%" arg)
		   (push (arg-key arg) option-list)
		   (push boolean-value option-list)
		   (setf boolean-taken t))))
	  (when (not boolean-taken)
	    ;; @@@ and some other option not to warn?
	    (warn "Unrecogized boolean flag ~a" (char a cc))))
       :else
       :do (push a new-args))
    (values (nreverse new-args) (nreverse option-list))))

(defun extract-long-boolean-options (command posix-args)
  "Returns POSIX-ARGS with long boolean options removed and as the second value,
a list of the converted boolean options."
  (let* (option-list
	 new-args)
    (loop :with flag-taken :and boolean-value = t
       :for a :in posix-args
       :when (and (>= (length a) 2)
		  (is-flag-char (char a 0))
		  (is-flag-char (char a 1)))
       :do
       (setf flag-taken nil)
       (loop :for arg :in (command-arglist command) :do
	  (if (and (string-equal (arg-long-arg arg) a :start2 2)
		   (eq (arg-type arg) 'boolean))
	      (progn
		(dbugf 'lish-arg "long boolean arg ~s~%" arg)
		(push (arg-key arg) option-list)
		(push boolean-value option-list)
		(setf flag-taken t))))
       (when (not flag-taken)
	 ;; @@@ and some other option not to warn?
	 (warn "Unrecogized long boolean flag ~a" a))
       :else
       :do (push a new-args))
    (values (nreverse new-args) (nreverse option-list))))

(defun extract-non-flagged (command posix-args)
  "Returns POSIX-ARGS with non-flagged arguments removed and as the second
value, a list of the converted arguments."
  (declare (ignore command posix-args))
  )

(defun extract-non-flagged-optional (command posix-args)
  "Returns POSIX-ARGS with non-flagged optional arguments removed and as the
second value, a list of the converted arguments."
  (declare (ignore command posix-args))
  )

#| @@@@
(defun extract-repeating (command posix-args)
  "Returns POSIX-ARGS with non flagged arguments removed and as the second
value, a list of the converted arguments."
  (let (new-args)
    (loop :for arg :in (command-arglist command) :do
       (if (arg-repeating arg)
	   (cond
	     ((and (zerop (length posix-args)) (not (arg-optional arg)))
	      (error "Missing mandatory argument: ~a." (arg-name arg)))
;	     ((setf end-flag (arg-end-flag arg command))
;	      ;; collect until end flag
;	      (move-repeating (old-list new-list 0 arg keyworded end-flag)))
;	     (check-for-multipe-repeats
;	      ;; error
;	      )
	     (t
	      ;; collect
	      ;; (move-repeating old-list new-repeating 0 arg keyworded)
	      ))))))
|#

(defun NEW-posix-to-lisp-args (command p-args)
  "Convert POSIX style arguments to lisp arguments. This makes flags like '-t'
become keyword arguments, in a way specified in the command's arglist."
  ;; (when (= (length p-args) 0)
  ;;   (return-from new-posix-to-lisp-args nil))
  (when (equal (command-name command) "env")
    (break))
  (let ((i 0)            ; Where we are in the old list, so effectively
	                 ; a count of how many posix args we've skipped.
;;;	(new-list        '())
	(old-list        (copy-list p-args)) ; so we don't modify it
;;;	(old-list        (mapcar #'shell-word-word p-args))
	(new-flags       '())
	(new-mandatories '())
	(new-optionals   '())
	(new-repeating   '())
	(keyworded (args-keyworded (command-arglist command)))
	(flag-taken      nil)
	(boolean-taken   nil)
	(boolean-value   t)
	(possible-flags  (loop :for a :in (command-arglist command)
			    :if (or (arg-short-arg a)
				    (arg-long-arg a))
			    :collect a))
	#| (optionals '()) |#)
    ;; Flagged arguments (optional or manditory)
    (dbugf 'lish-arg "considering flagged: ~s~%" old-list)
    (loop :with a
       :while (< i (length old-list)) :do
       #| (setf a (car old-list)) |#
       (setf a (shell-word-word (nth i old-list)))
       (if (and (stringp a) (> (length a) 0)
		(is-flag-char (char a 0)))
	   (if (and (> (length a) 1)
		    (is-flag-char (char a 0))
		    (is-flag-char (char a 1))) ; two dash arg
	       ;; --long-arg
	       (progn
		 (setf flag-taken nil boolean-taken nil)
		 (loop :for arg :in (command-arglist command) :do
		    ;; @@@ have to deal with repeating?
		    (when (equalp (subseq a 2) (arg-long-arg arg))
		      (if (eq (arg-type arg) 'boolean)
			  (progn
			    (dbugf 'lish-arg "boolean long arg ~s~%" arg)
			    (move-boolean-2 old-list new-flags i arg))
			  (progn
			    (dbugf 'lish-arg "long arg ~s~%" arg)
			    (move-flag old-list new-flags i arg)))
		      (setf flag-taken t)))
		 (when (not flag-taken)
		   (warn "Unrecognized long option ~a" a)
		   (incf i)))
	       ;; -abcxyz (short args)
	       (progn
		 (setf boolean-taken nil
		       boolean-value (is-normal-flag-char (char a 0)))
		 (loop :for cc :from 1 :below (length a) :do
		    (setf flag-taken nil)
		    (loop :for arg :in (command-arglist command) :do
		       (when (eql (arg-short-arg arg) (char a cc))
			 (setf flag-taken t)
			 ;; @@@ have to deal with repeating?
			 (if (eq (arg-type arg) 'boolean)
			     (progn
			       (dbugf 'lish-arg "short boolean arg ~s~%" arg)
			       (move-boolean old-list new-flags i arg
					     boolean-value)
			       (setf boolean-taken t))
			     (if (/= cc (1- (length a)))
				 (error "Unrecognized flag ~a." a)
				 (progn
				   (dbugf 'lish-arg "short arg ~s~%" arg)
				   (move-flag old-list new-flags i arg))))))
		    (when (not flag-taken)
		      (warn "Unrecognized option ~a" (char a cc))))
		  (if boolean-taken
		      (setf old-list (delete-nth i old-list))
		      (progn
			;;(incf i)
			(dbugf 'lish-arg "skipping flag value ~a ~w~%"
			       i (nth i old-list))
			;;(setf old-list (delete-nth i old-list))
			))))
	   ;; Arg doesn't start with a dash, so skip it
	   (progn
	     (dbugf 'lish-arg "skipping arg ~a ~w~%" i a)
	     (incf i))))
#|    ;; Default any left over defaultable flags
    (loop :for a :in possible-flags :do
       (when (arg-default a)
	 (push (arg-key a) new-flags)
	 (push (arg-default a) new-flags))) |#
    (setf new-flags (nreverse new-flags))
    ;; Non-flagged mandatories.
    (setf i 0)
    (dbugf 'lish-arg "considering non-flagged: ~s~%" old-list)
    (loop
       :for arg :in (command-arglist command) :do
       (if (not (or (arg-optional arg)
		    (arg-has-flag arg)
		    (arg-repeating arg)))
	   (if (> (length old-list) 0)
	       (progn
		 (dbugf 'lish-arg "found mandatory arg ~a~%" arg)
		 (move-arg old-list new-mandatories 0 arg))
	       (error "Missing mandatory argument: ~a." (arg-name arg)))
	   ;; skip
	   (incf i)))
    (setf new-mandatories (nreverse new-mandatories))
    ;; Non-flagged optionals
    (dbugf 'lish-arg "considering non-flagged optionals: ~s~%" old-list)
    (loop
       :for arg :in (command-arglist command) :do
       (if (and (arg-optional arg) (not (arg-repeating arg))
		(not (arg-has-flag arg)))
	   (if (> (length old-list) 0)
	       (move-key old-list new-optionals 0 arg keyworded)
	       #| (if (arg-default arg)
	           (push-key new-optionals arg (arg-default arg) keyworded) |#
	       ;; skip
	       (incf i))))
    (setf new-optionals (nreverse new-optionals))
    ;; Repeating
    (loop #| :with i = 0 :and did-one = nil :and end-flag |#
       :for arg :in (command-arglist command) :do
       (if (arg-repeating arg)
	   (cond
	     ((and (>= i (length old-list)) (not (arg-optional arg)))
	      (error "Missing mandatory argument: ~a." (arg-name arg)))
;	     ((setf end-flag (arg-end-flag arg command))
;	      ;; collect until end flag
;	      (move-repeating (old-list new-list 0 arg keyworded end-flag)))
;	     (check-for-multipe-repeats
;	      ;; error
;	      )
	     (t
	      ;; collect
	      (move-repeating old-list new-repeating 0 arg keyworded)))))
    (setf new-repeating (nreverse new-repeating))
    (when (> (length old-list) 0)
      (warn "Extra arguments: ~w" old-list))
    (concatenate
     'list new-mandatories new-optionals new-repeating new-flags)))

(defun posix-to-lisp-args (command p-args)
  "Convert POSIX style arguments to lisp arguments. This makes flags like '-t'
become keyword arguments, in a way specified in the command's arglist."
  ;; (when (= (length p-args) 0)
  ;;   (return-from new-posix-to-lisp-args nil))
  ;; (when (equal (command-name command) "env")
  ;;   (break))
  (let ((i 0)            ; Where we are in the old list, so effectively
	                 ; a count of how many posix args we've skipped.
;;;	(new-list        '())
	(old-list        (copy-list p-args)) ; so we don't modify it
;;;	(old-list        (mapcar #'shell-word-word p-args))
	(new-flags       '())
	(new-mandatories '())
	(new-optionals   '())
	(new-repeating   '())
	(keyworded (args-keyworded (command-arglist command)))
	(flag-taken      nil)
	(boolean-taken   nil)
	(boolean-value   t)
	(possible-flags  (loop :for a :in (command-arglist command)
			    :if (or (arg-short-arg a)
				    (arg-long-arg a))
			    :collect a))
	#| (optionals '()) |#)
    ;; Flagged arguments (optional or manditory)
    (dbugf 'lish-arg "considering flagged: ~s~%" old-list)
    (loop :with a
       :while (< i (length old-list)) :do
       #| (setf a (car old-list)) |#
       (setf a (shell-word-word (nth i old-list)))
       (if (and (stringp a) (> (length a) 0)
		(is-flag-char (char a 0)))
	   (if (and (> (length a) 1)
		    (is-flag-char (char a 0))
		    (is-flag-char (char a 1))) ; two dash arg
	       ;; --long-arg
	       (progn
		 (setf flag-taken nil boolean-taken nil)
		 (loop :for arg :in (command-arglist command) :do
		    ;; @@@ have to deal with repeating?
		    (when (equalp (subseq a 2) (arg-long-arg arg))
		      (if (eq (arg-type arg) 'boolean)
			  (progn
			    (dbugf 'lish-arg "boolean long arg ~s~%" arg)
			    (move-boolean-2 old-list new-flags i arg))
			  (progn
			    (dbugf 'lish-arg "long arg ~s~%" arg)
			    (move-flag old-list new-flags i arg)))
		      (setf flag-taken t)))
		 (when (not flag-taken)
		   (warn "Unrecognized long option ~a" a)
		   (incf i)))
	       ;; -abcxyz (short args)
	       (progn
		 (setf boolean-taken nil
		       boolean-value (is-normal-flag-char (char a 0)))
		 (loop :for cc :from 1 :below (length a) :do
		    (setf flag-taken nil)
		    (loop :for arg :in (command-arglist command) :do
		       (when (eql (arg-short-arg arg) (char a cc))
			 (setf flag-taken t)
			 ;; @@@ have to deal with repeating?
			 (if (eq (arg-type arg) 'boolean)
			     (progn
			       (dbugf 'lish-arg "short boolean arg ~s~%" arg)
			       (move-boolean old-list new-flags i arg
					     boolean-value)
			       (setf boolean-taken t))
			     (if (/= cc (1- (length a)))
				 (error "Unrecognized flag ~a." a)
				 (progn
				   (dbugf 'lish-arg "short arg ~s~%" arg)
				   (move-flag old-list new-flags i arg))))))
		    (when (not flag-taken)
		      (incf i)
		      (warn "Unrecognized option ~a" (char a cc))))
		  (if boolean-taken
		      (setf old-list (delete-nth i old-list))
		      (progn
			;;(incf i)
			(dbugf 'lish-arg "skipping flag value ~a ~w~%"
			       i (nth i old-list))
			;;(setf old-list (delete-nth i old-list))
			))))
	   ;; Arg doesn't start with a dash, so skip it
	   (progn
	     (dbugf 'lish-arg "skipping arg ~a ~w~%" i a)
	     (incf i))))
#|    ;; Default any left over defaultable flags
    (loop :for a :in possible-flags :do
       (when (arg-default a)
	 (push (arg-key a) new-flags)
	 (push (arg-default a) new-flags))) |#
    (setf new-flags (nreverse new-flags))
    ;; Non-flagged mandatories.
    (setf i 0)
    (dbugf 'lish-arg "considering non-flagged: ~s~%" old-list)
    (loop
       :for arg :in (command-arglist command) :do
       (if (not (or (arg-optional arg)
		    (arg-has-flag arg)
		    (arg-repeating arg)))
	   (if (> (length old-list) 0)
	       (progn
		 (dbugf 'lish-arg "found mandatory arg ~a~%" arg)
		 (move-arg old-list new-mandatories 0 arg))
	       (error "Missing mandatory argument: ~a." (arg-name arg)))
	   ;; skip
	   (incf i)))
    (setf new-mandatories (nreverse new-mandatories))
    ;; Non-flagged optionals
    (dbugf 'lish-arg "considering non-flagged optionals: ~s~%" old-list)
    (loop
       :for arg :in (command-arglist command) :do
       (if (and (arg-optional arg) (not (arg-repeating arg))
		(not (arg-has-flag arg)))
	   (if (> (length old-list) 0)
	       (move-key old-list new-optionals 0 arg keyworded)
	       #| (if (arg-default arg)
	           (push-key new-optionals arg (arg-default arg) keyworded) |#
	       ;; skip
	       (incf i))))
    (setf new-optionals (nreverse new-optionals))
    ;; Repeating
    (loop #| :with i = 0 :and did-one = nil :and end-flag |#
       :for arg :in (command-arglist command) :do
       (if (arg-repeating arg)
	   (cond
	     ((and (>= i (length old-list)) (not (arg-optional arg)))
	      (error "Missing mandatory argument: ~a." (arg-name arg)))
;	     ((setf end-flag (arg-end-flag arg command))
;	      ;; collect until end flag
;	      (move-repeating (old-list new-list 0 arg keyworded end-flag)))
;	     (check-for-multipe-repeats
;	      ;; error
;	      )
	     (t
	      ;; collect
	      (move-repeating old-list new-repeating 0 arg keyworded)))))
    (setf new-repeating (nreverse new-repeating))
    (when (> (length old-list) 0)
      (warn "Extra arguments: ~w" old-list))
    (concatenate
     'list new-mandatories new-optionals new-repeating new-flags)))

#|
(defun OLD-posix-to-lisp-args (command p-args)
  "Convert POSIX style arguments to lisp arguments. This makes flags like '-t'
become keyword arguments, in a way specified in the command's arglist."
  ;; (when (= (length p-args) 0)
  ;;   (return-from new-posix-to-lisp-args nil))
  (let ((i 0)            ; Where we are in the old list, so effectively
	                 ; a count of how many posix args we've skipped.
;;;	(new-list        '())
;;;	(old-list        (copy-list p-args)) ; so we don't modify it
	(old-list        (mapcar #'shell-word-word p-args))
	(new-flags       '())
	(new-mandatories '())
	(new-optionals   '())
	(new-repeating   '())
	(keyworded (args-keyworded (command-arglist command)))
	(flag-taken      nil)
	(boolean-taken   nil)
	(possible-flags  (loop :for a :in (command-arglist command)
			    :if (or (arg-short-arg a)
				    (arg-long-arg a))
			    :collect a))
	#| (optionals '()) |#)
    ;; Flagged arguments (optional or manditory)
    (loop :with a
       :while (< i (length old-list)) :do
       #| (setf a (car old-list)) |#
       (setf a (nth i old-list))
       (if (and (stringp a) (> (length a) 0)
		(char= (char a 0) #\-))	; arg starts with dash
	   (if (and (> (length a) 1) (eql (char a 1) #\-)) ; two dash arg
	       ;; --long-arg
	       (progn
		 (setf flag-taken nil boolean-taken nil)
		 (loop :for arg :in (command-arglist command) :do
		    ;; @@@ have to deal with repeating?
		    (when (equalp (subseq a 2) (arg-long-arg arg))
		      (if (eq (arg-type arg) 'boolean)
			  (progn
			    (dbug "boolean long arg ~s~%" arg)
			    (move-boolean-2 old-list new-flags i arg))
			  (progn
			    (dbug "long arg ~s~%" arg)
			    (move-flag old-list new-flags i arg)))
		      (setf flag-taken t)))
		 (when (not flag-taken)
		   (warn "Unrecognized long option ~a" a)
		   (incf i)))
	       ;; -abcxyz (short args)
	       (progn
		 (setf boolean-taken nil)
		 (loop :for cc :from 1 :below (length a) :do
		    (setf flag-taken nil)
		    (loop :for arg :in (command-arglist command) :do
		       (when (eql (arg-short-arg arg) (char a cc))
			 (setf flag-taken t)
			 ;; @@@ have to deal with repeating?
			 (if (eq (arg-type arg) 'boolean)
			     (progn
			       (move-boolean old-list new-flags i arg)
			       (setf boolean-taken t))
			     (if (/= cc (1- (length a)))
				 (error "Unrecognized flag ~a." a)
				 (move-flag old-list new-flags i arg)))))
		    (when (not flag-taken)
		      (warn "Unrecognized option ~a" (char a cc))))
		  (if boolean-taken
		      (setf old-list (delete-nth i old-list))
		      (progn
			(dbug "skipping flag arg ~a ~w~%" i a)
			;;(setf old-list (delete-nth i old-list))
			(incf i)))))
	   ;; Arg doesn't start with a dash, so skip it
	   (progn
	     (dbug "skipping arg ~a ~w~%" i a)
	     (incf i))))
#|    ;; Default any left over defaultable flags
    (loop :for a :in possible-flags :do
       (when (arg-default a)
	 (push (arg-key a) new-flags)
	 (push (arg-default a) new-flags))) |#
    (setf new-flags (nreverse new-flags))
    ;; Non-flagged mandatories.
    (setf i 0)
    (dbug "considering non-flagged: ~s~%" old-list)
    (loop
       :for arg :in (command-arglist command) :do
       (if (not (or (arg-optional arg)
		    (arg-has-flag arg)
		    (arg-repeating arg)))
	   (if (> (length old-list) 0)
	       (progn
		 (dbug "found mandatory arg ~a~%" arg)
		 (move-arg old-list new-mandatories 0 arg))
	       (error "Missing mandatory argument ~a." (arg-name arg)))
	   ;; skip
	   (incf i)))
    (setf new-mandatories (nreverse new-mandatories))
    ;; Non-flagged optionals
    (dbug "considering non-flagged optionals: ~s~%" old-list)
    (loop
       :for arg :in (command-arglist command) :do
       (if (and (arg-optional arg) (not (arg-repeating arg))
		(not (arg-has-flag arg)))
	   (if (> (length old-list) 0)
	       (move-key old-list new-optionals 0 arg keyworded)
	       #| (if (arg-default arg)
	           (push-key new-optionals arg (arg-default arg) keyworded) |#
	       ;; skip
	       (incf i))))
    (setf new-optionals (nreverse new-optionals))
    ;; Repeating
    (loop #| :with i = 0 :and did-one = nil :and end-flag |#
       :for arg :in (command-arglist command) :do
       (if (arg-repeating arg)
	   (cond
	     ((and (>= i (length old-list)) (not (arg-optional arg)))
	      (error "Missing mandatory argument ~a." (arg-name arg)))
;	     ((setf end-flag (arg-end-flag arg command))
;	      ;; collect until end flag
;	      (move-repeating (old-list new-list 0 arg keyworded end-flag)))
;	     (check-for-multipe-repeats
;	      ;; error
;	      )
	     (t
	      ;; collect
	      (move-repeating old-list new-repeating 0 arg keyworded)))))
    (setf new-repeating (nreverse new-repeating))
    (when (> (length old-list) 0)
      (warn "Extra arguments: ~w" old-list))
    (concatenate
     'list new-mandatories new-optionals new-repeating new-flags)))

(defun OLDER-posix-to-lisp-args (command p-args)
  "Convert POSIX style arguments to lisp arguments. This makes flags like '-t'
become keyword arguments, in a way specified in the command's arglist."
  ;; (when (= (length p-args) 0)
  ;;   (return-from new-posix-to-lisp-args nil))
  (let ((i 0)
;	(new-list        '())
	(old-list        (copy-list p-args)) ; so we don't modify it
	(new-flags       '())
	(new-mandatories '())
	(new-optionals   '())
	(new-repeating   '())
	(keyworded (args-keyworded (command-arglist command)))
	(possible-flags  (loop :for a :in (command-arglist command)
			    :if (or (arg-short-arg a)
				    (arg-long-arg a))
			    :collect a))
	#| (optionals '()) |#)
    ;; Flagged arguments (optional or manditory)
    (loop :for a :in p-args :do
       (if (and (stringp a) (> (length a) 0)
		(char= (char a 0) #\-))	; arg starts with dash
	   (if (eql (char a 1) #\-)	; two dash arg
	       ;; --long-arg
	       (loop :for arg :in (command-arglist command) :do
		  ;; @@@ have to deal with repeating?
		  (if (equalp (subseq a 2) (arg-long-arg arg))
		      (move-flag old-list new-flags i arg)
		      (incf i)))
	       ;; -abcxyz (short args)
	       (prog (flag-taken boolean-taken)
		  (loop :for cc :from 1 :below (length a) :do
		     (setf flag-taken nil)
		     (loop :for arg :in (command-arglist command) :do
			(when (eql (arg-short-arg arg) (char a cc))
			  (setf flag-taken t)
			  ;; @@@ have to deal with repeating?
			  (if (eq (arg-type arg) 'boolean)
			      (progn
				(move-boolean old-list new-flags i arg)
				(setf boolean-taken t))
			      (if (/= cc (1- (length a)))
				  (error "Unrecognized flag ~a." a)
				  (move-flag old-list new-flags i arg)))))
		     (when (not flag-taken)
		       (warn "Unrecognized option ~a" (char a cc))))
		  (when boolean-taken
		    (setf old-list (delete-nth i old-list)))))
	   (incf i)))
    ;; Default any left over defaultable flags
    (loop :for a :in possible-flags :do
       (when (arg-default a)
	 (push (arg-key a) new-flags)
	 (push (arg-default a) new-flags)))
    (setf new-flags (nreverse new-flags))
    ;; Non-flagged mandatories.
    (loop
       :for arg :in (command-arglist command) :do
       (if (not (or (arg-optional arg)
		    (arg-has-flag arg)
		    (arg-repeating arg)))
	   (if (> (length old-list) 0)
	       (move-arg old-list new-mandatories 0 arg)
	       (error "Missing mandatory argument ~a." (arg-name arg)))
	   (incf i)))
    (setf new-mandatories (nreverse new-mandatories))
    ;; Non-flagged optionals
    (loop
       :for arg :in (command-arglist command) :do
       (if (and (arg-optional arg) (not (arg-repeating arg))
		(not (arg-has-flag arg)))
	   (if (> (length old-list) 0)
	       (move-key old-list new-optionals 0 arg keyworded)
	       (if (arg-default arg)
		   (push-key new-optionals arg (arg-default arg) keyworded)
		   (incf i)))))
    (setf new-optionals (nreverse new-optionals))
    ;; Repeating
    (loop #| :with i = 0 :and did-one = nil :and end-flag |#
       :for arg :in (command-arglist command) :do
       (if (arg-repeating arg)
	   (cond
	     ((and (>= i (length old-list)) (not (arg-optional arg)))
	      (error "Missing mandatory argument ~a." (arg-name arg)))
;	     ((setf end-flag (arg-end-flag arg command))
;	      ;; collect until end flag
;	      (move-repeating (old-list new-list 0 arg keyworded end-flag)))
;	     (check-for-multipe-repeats
;	      ;; error
;	      )
	     (t
	      ;; collect
	      (move-repeating old-list new-repeating 0 arg keyworded)))))
    (setf new-repeating (nreverse new-repeating))
    (when (> (length old-list) 0)
      (warn "Extra arguments: ~w" old-list))
    (concatenate
     'list new-mandatories new-optionals new-repeating new-flags)))
|#

(defun posix-synopsis (command)
  "Return a string with the POSIX style argument synopsis for the COMMAND."
  (with-output-to-string (str)
    (format str "~a" (command-name command))
    ;; boolean flag options
    (loop :with first-time = t
       :for a :in (command-arglist command) :do
       (when (and (eql (arg-type a) 'boolean)
		  (arg-short-arg a))
	 (when first-time
	   (setf first-time nil)
	   (format str " [-"))
	 (format str "~c" (arg-short-arg a)))
       :finally (when (not first-time) (format str "]")))
    ;; non-boolean
    (loop :for a :in (command-arglist command) :do
       (when (not (and (eql (arg-type a) 'boolean)
		       (arg-short-arg a)))
	 (if (arg-optional a)
	     (format str " [")
	     (format str " "))
	 (if (arg-short-arg a)
	     (format str "-~a " (arg-short-arg a))
	     (when (arg-long-arg a)
	       (format str "--~a " (arg-long-arg a))))
	 (format str "~a" (arg-name a))
	 (when (arg-repeating a)
	   (format str "..."))
	 (when (arg-optional a)
	   (format str "]"))))))

;(defmacro defexternal 

#|
(defmacro call-with-keywords (command-name func &rest kw)
  (with-unique-names (args)
    `(let ((,args (loop :with arg-kw
		     :for a :in (lish:command-arglist
				 (lish:get-command ,command-name))
		     :do
		     (setf arg-kw (keywordify (lish:arg-name a)))
		     :if (or (not ,kw) (position arg-kw ,kw))
		     :collect arg-kw
		     :collect (symbolify (lish:arg-name a)))))
       (apply ,func ,args))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These arguemnt types have to come after commands are defined.

(defclass arg-command (arg-choice) () (:documentation "A lish command name."))
(defmethod convert-arg ((arg arg-command) (value string) &optional quoted)
  (declare (ignore arg quoted))
  (get-command value))

(defmethod argument-choices ((arg arg-command))
  "Return the possible path names."
  *command-list*)

;; This is for anything that can be the first word of command line.
(defclass arg-shell-command (arg-choice) ()
  (:documentation "A lish shell command name."))

(defmethod argument-choices ((arg arg-shell-command))
  "Return the possible path names."
  *command-list*)

;; EOF
