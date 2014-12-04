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
;; The way commands work, we could also someday have an interface which reads
;; Lish commands in a way that is more like Lisp machine command readers.

;; $Revision$

(in-package :lish)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command arguments

(defclass argument ()
  ((name	:documentation "Name"
		:initarg :name
		:accessor arg-name)
   (type	:documentation "Declared type"
		:initarg :type
		:accessor arg-type)
   (value	:documentation "Value"
		:initarg :value
		:accessor arg-value)
   (default	:documentation "Default value, if optional."
		:initarg :default
		:initform nil
		:accessor arg-default)
   (repeating	:type boolean
		:documentation "True if value can repeat."
		:initarg :repeating
		:initform nil
		:accessor arg-repeating)
   (optional	:type boolean
		:documentation "True if a value is not required."
		:initarg :optional
		:initform t
		:accessor arg-optional)
   (hidden	:type boolean
		:documentation "If true, don't show in help."
		:initarg :hidden
		:initform nil
		:accessor arg-hidden)
   (prompt	:type string
		:documentation "Show when asking user for value."
		:initarg :propmt
		:accessor arg-propmt)
   (help	:type string
		:documentation "Description for the user."
		:initarg :help
		:accessor arg-help)
   (short-arg	:type (or character null)
		:documentation "Command line argument, short form."
		:initarg :short-arg
		:initform nil
		:accessor arg-short-arg)
   (long-arg	:type (or string null)
		:documentation "Command line argument, long form."
		:initarg :long-arg
		:initform nil
		:accessor arg-long-arg))
  (:documentation "Generic command parameter."))

(defmethod initialize-instance :after ((o argument) &rest initargs)
  (declare (ignore initargs))
  ;; Make the long-arg default to the name if the short-arg is set.
  (when (slot-value o 'short-arg)
    (setf (slot-value o 'long-arg) (slot-value o 'name))))

(defmethod print-object ((o argument) stream)
  "Print a lish command argument in an unreadable way."
  (print-unreadable-object (o stream :identity nil :type t)
    (format stream
	    "~a ~s~:[~; repeating~]~:[~; optional~]~:[~; hidden~]~
~@[ -~a~]~@[ --~a~]"
	    (arg-name o) (arg-type o)
	    (arg-repeating o)
	    (arg-optional o)
	    (arg-hidden o)
	    (arg-short-arg o)
	    (arg-long-arg o))))

(defgeneric convert-arg (arg value)
  (:documentation "Convert an argument value from one type to another."))

(defmethod convert-arg ((arg argument) value)
  "The base default conversion just returns the value."
  value)

(defclass arg-boolean (argument) () (:documentation "A true or false value."))
(define-constant +true-strings+ '("T" "TRUE" "YES" "ON" "1"))
(define-constant +false-strings+ '("NIL" "FALSE" "NO" "OFF" "0"))
(defmethod convert-arg ((arg arg-boolean) (value string))
  (cond
    ((position value +true-strings+ :test #'equalp) t)
    ((position value +false-strings+ :test #'equalp) nil)
    (t (error "Can't convert ~w to a boolean." value))))

(defclass arg-number (argument) () (:documentation "A number."))
(defmethod convert-arg ((arg arg-number) (value string))
  (let* ((*read-eval* nil)
	 (num (read-from-string value nil nil)))
    (if (and num (numberp num))
	num
	(error "Can't convert ~w to a number." value))))

(defclass arg-integer (arg-number) () (:documentation "An integer."))
(defmethod convert-arg ((arg arg-integer) (value string))
  (let ((int (parse-integer value :junk-allowed nil)))
    (if (and int (integerp int))
	int
	(error "Can't convert ~w to an integer." value))))

(defclass arg-float (arg-number) ()
  (:documentation "An floating point number."))
(defmethod convert-arg ((arg arg-float) (value string))
  (let* ((*read-eval* nil)
	 (num (read-from-string value nil nil)))
    (if (and num (floatp num))
	num
	(error "Can't convert ~w to a float." value))))

(defclass arg-string (argument) () (:documentation "A string."))
(defmethod convert-arg ((arg arg-string) (value string))
  (declare (ignore arg))
  value)

(defclass arg-keyword (argument) () (:documentation "A Lisp keyword."))
(defmethod convert-arg ((arg arg-keyword) (value string))
   (if (char/= (char arg 0) #\:)
       (intern (string-upcase (subseq value 1)) (find-package :keyword))
       value))

(defclass arg-date (argument) () (:documentation "A date."))
(defmethod convert-arg ((arg arg-date) (value string))
  (declare (ignore arg))
  ;; @@@ This could be better.
  value)

(defclass arg-pathname (arg-string) () (:documentation "A file system path."))
(defmethod convert-arg ((arg arg-pathname) (value string))
  (declare (ignore arg))
  value)

(defclass arg-choice (argument)
  ((choices	:type list
		:documentation "A list of choices for value."
		:initarg :choices
		:accessor arg-choices)
   (choice-labels :type list
		:documentation "A list of string names for choices."
		:initarg :choice-labels
		:accessor arg-choice-labels)
   (choice-func :type function
		:documentation "A function to call to get the list of choices."
		:initarg :choice-func
		:accessor arg-choice-func))
  (:documentation "An argument whose value must be one of a list of choices."))

(defmethod convert-arg ((arg arg-choice) (value string))
  (let (choice)
    (if (setf choice (find value (arg-choices arg)
			   :test #'(lambda (a b)
				     (equalp a (princ-to-string b)))))
	choice
	(error "Argument ~w is not one of ~a." value (arg-choices arg)))))

#| Actually I think these should just be in the base class
(defclass arg-command-line (argument)
  ((short-arg	:type character
		:documentation "Command line argument, short form."
		:initarg :short-arg
		:accessor arg-short-arg)
   (long-arg	:type string
		:documentation "Command line argument, long form."
		:initarg :long-arg
		:accessor arg-long-arg))
  (:documentation "A parameter from a command line."))

(defclass arg-cmd-boolean (arg-boolean arg-command-line) ()
  (:documentation "A true or false value from the command line."))
|#

(defun argument-type-class (type)
  "Return the argument class for a given type. If the type is not a defined
ARG-* class, it defaults to the generic ARGUMENT class."
  (let* (;(pkg (symbol-package type))
	 class-symbol arg-class)
    (cond
      ((listp type)
       (if (not (eq (car type) 'or))
	 (error "Only (or ...) compound types are supported.")
	 'argument))
      ((or (symbolp type) (stringp type))
       (when (setf class-symbol
		   (intern (s+ "ARG-" (string-upcase type)) :lish #|pkg|#))
	 (setf arg-class (find-class class-symbol nil)))
       (or arg-class 'argument))
      ((eql type t)
       'argument)
      (t
       (error "Argument type is not a symbol, string or T.")))))

(defun arglist-value (arglist key)
  "Return a value from a DEFCOMMAND arglist argument."
  (let ((p (position key arglist)))
    (and p (elt arglist (1+ p)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun make-argument-list (arglist)
  "Take an ARGLIST from DEFCOMMAND and turn in into a list of argument objects,
like in the command object."
;;;  (declare (type list arglist))
  (when (not (listp arglist))
    (error "Command argument list must be a list."))
  (loop :with name :and type
     :for a :in arglist :do
;     (assert (listp a))
     (when (not (listp a))
       (error "Command argument list element must be a list."))
     #|   (setf name (arglist-value a :name)
     type (arglist-value a :type)) |#
     (setf name (first a)
	   type (second a))
     (when (not name)
       (error "Arguments must have a name."))
     (when (not type)
       (error "Arguments must have a type."))
     (setf a (append (list :name name :type type) (cddr a)))
     :collect (apply #'make-instance (argument-type-class type) a))))

(defun arg-has-flag (arg)
  (or (arg-short-arg arg) (arg-long-arg arg)))

;; They must be keyworded if there are any flagged arguments.
(defun args-keyworded (args)
  "Check if an argument must be keyworded. "
  (loop :for a :in args :do
     (when (arg-has-flag a)
       (return-from args-keyworded t)))
  nil)

;; Thankfully this is nowhere near as hairy as posix-to-lisp-args.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defun shell-to-lisp-args (shell-args)
  "Return a Lisp argument list for the given lish argument list."
  (let ((mandatories
	 (loop :for a :in shell-args
	    :if (not (arg-optional a))
	    :collect a))
	(optionals
	 (loop :for a :in shell-args
	    :if (arg-optional a)
	    :collect a))
	(repeating
	 (loop :for a :in shell-args
	    :if (arg-repeating a)
	    :collect a))
	(keyworded (args-keyworded shell-args))
	(new-list '()))
    ;; Mandatory arguments
    (loop :for a :in mandatories :do
       (push (symbolify (arg-name a)) new-list))
    ;; This is augmented here to allow paralellism in the let above.
    (setf keyworded (or keyworded
			(and optionals repeating
			     (and (not (equal optionals repeating))
				  (= (length optionals) 1)))
			(> (length repeating) 1)))
    (if keyworded
	(progn
	  (push '&key new-list)
	  (loop :for a :in optionals :do
	     (push
	      (if (arg-default a)
		  (list (symbolify (arg-name a)) (arg-default a))
		  (symbolify (arg-name a)))
	      new-list)))
	(cond
	  ;; If both optional and repeating, do repeating (i.e. &rest)
	  (repeating
	   (push '&rest new-list)
	   ;; Must be only one repeating, else it would be keyworded.
	   (push (symbolify (arg-name (first repeating))) new-list))
	  (optionals
	   (push '&optional new-list)
	   (loop :for a :in optionals :do
	      (push
	       (if (arg-default a)
		   (list (symbolify (arg-name a)) (arg-default a))
		   (symbolify (arg-name a)))
	       new-list)))))
    (nreverse new-list))))

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

(defparameter *initial-commands* nil
  "List of initial commands.")

(defparameter *command-list* nil
  "List of command names")

(defvar *lish-commands* nil
  "Hash table of commands. This means that commands are shared by all shell
instances.")

(defun lish-commands ()
  (when (not *lish-commands*)
    (setf *lish-commands* (make-hash-table :test #'equal)))
  *lish-commands*)

(defun set-command (name obj)
  (setf (gethash name (lish-commands)) obj))

(defun get-command (name)
  (gethash name (lish-commands)))

(defun init-commands ()
  "Set up the *LISH-COMMANDS* hash table, and load it with the commands from
*INITIAL-COMMANDS*, which is likely whatever commands were defined with
DEFBUILTIN."
  (loop :for (k v) :in *initial-commands*
     :do (set-command k v)))

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

;; There should be little distinction between a user defined command and
;; "built-in" command, except perhaps for a warning if you redefine a
;; pre-defined command, and the fact that things defined in here are
;; considered "built-in" and listed in help.

(defmacro defbuiltin (name (&rest arglist) &body body)
  "This is like defcommand, but for things that are considered built in to the
shell."
  (let ((func-name (command-function-name name))
	(command-name (intern (string name) :lish))
	(name-string (string-downcase name))
;;;	(name-string (concatenate 'string "\"" (string-downcase name) "\""))
	(params (shell-to-lisp-args (make-argument-list arglist))))
    `(progn
       (defun ,func-name ,params
	 ,@body)
;       (export (quote ,func-name))
       (push (quote ,command-name) *command-list*)
       (push (list ,name-string (make-instance
				 'command :name ,name-string
				 :arglist (make-argument-list ',arglist)
				 :loaded-from *load-pathname*
				 :built-in-p t))
	     *initial-commands*))))

;; This is differs from DEFBUILTIN in that:
;;   - It doesn't push to *initial-commands*
;;   - It doesn't set BUILT-IN-P
;;   - It doesn't automatically export
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

(defmacro defcommand (name (&rest arglist) &body body)
  "Define a command for the shell. NAME is the name it is invoked by. ARGLIST
is a shell argument list. The BODY is the body of the function it calls."
  (let ((func-name (command-function-name name))
	(command-name (intern (string name)))
	(name-string (string-downcase name))
	(params (shell-to-lisp-args (make-argument-list arglist))))
    `(progn
       (defun ,func-name ,params
	 ,@body)
       ;; Don't export stuff because it causes package variance on reloading.
       ;; @@@ Perhaps we should define commands in the LISH-USER package
       ;; instead, so they can be exported and used by other packages?
       (push (quote ,command-name) lish::*command-list*)
;;;       (set (find-symbol "*COMMAND-LIST*" :lish) (quote ,command-name))
       (set-command ,name-string
		    (make-instance (find-symbol "COMMAND" :lish)
				   :name ,name-string
				   :loaded-from *load-pathname*
				   :arglist (make-argument-list ',arglist))))))

;; Yet another defclass wrapper.
;; (defmacro defargtype (name (&rest superclasses) &body body)
;;   "This defines a command argument type. Mostly for the purposes "
;;   (let ((slots '()) (body
;; 	 (loop :for form :in body :do
;; 	    (cond
;; 	      ((listp form) ; a slot specifier
;; 	       )
;;     `(defclass ,name ,superclasses
;;        ,rock-the-body)))


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

#|

The rules for converting POSIX arguments to lambda lists are fairly complicated.
Here we try to examine some of the possiblities. We use a letter to denote the
category of argument:
  m = manditory  o = optional  r = repeating  f = flagged

When only manditory and optional are present, we don't need keywords.
The order of manditories vs optionals doesn't matter to lambda lists, but does
to POSIX.
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

Flagged manditory must be done as keywords, which DOES'T make other manditories
keywords.
Manditory flagged treated as optional flagged, except error afterward if
not present.

  mf1 m2 of1 o2		(m2 &key mf1 of1 o2)	[-of1] [-mf1] [o2] m2
  m1 mf2 o1 of2		(m1 &key mf2 o1 of2)	[-mf2] [-of2] [o1] m1
  mf1 mf2 of1 of2	(&key mf1 mf2 o1 of2)	[-mf1] [-mf2] [-of2] [o1]
  of1 o2 mf1 m2 	(m2 &key of1 o2 mf1)	[-of1] [-mf1] [o2] m2
  of1 of2 m1 mf2 	(m1 &key of1 of2 mf2)	[-of1] [-of2] [-mf2] m1
  mf1 mf2		(&key mf1 mf2)		[-mf1] [-mf2]

Repeating flagged: can have more than one, but values can't start with dashes!
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
     (setf ,new (push (convert-arg ,arg (nth ,i ,old)) ,new)
	   ,old (delete-nth ,i ,old))))

(defun arg-key (arg)
  (intern (string-upcase (arg-name arg)) :keyword))

(defmacro move-key (old new i arg keyworded)
  "Move the I'th item from the OLD to the NEW list, and return both."
  `(progn
     (when ,keyworded
       (setf ,new (push (arg-key ,arg) ,new)))
     (setf ,new (push (convert-arg ,arg (nth ,i ,old)) ,new))
     (setf ,old (delete-nth ,i ,old))))

(defmacro move-flag (old new i arg)
  `(progn
     (setf ,new (push (arg-key ,arg) ,new))
;;;     (format t "(nth (1+ ~s) ~s) = ~s~%" ,i ,old (nth ,i ,old))
;;;     (format t "(convert-arg ~s ~s) = ~s~%" ,arg (nth ,i ,old)
;;;	     (convert-arg ,arg (nth ,i ,old)))
;;;     (setf ,new (push (convert-arg ,arg (nth (1+ ,i) ,old)) ,new))
     (setf ,new (push (convert-arg ,arg (nth (1+ ,i) ,old)) ,new))
     (setf ,old (delete-nth ,i ,old))  ; flag
     (setf ,old (delete-nth ,i ,old)))) ; arg

(defmacro move-boolean-2 (old new i arg)
  `(progn
     (setf ,new (push (arg-key ,arg) ,new))
     (setf ,new (push t ,new))
     (setf ,old (delete-nth ,i ,old))  ; keyword
     (setf ,old (delete-nth ,i ,old)))) ; arg

(defmacro move-boolean (old new i arg)
  (declare (ignore old i))
  `(progn
     (setf ,new (push (arg-key ,arg) ,new))
     (setf ,new (push t ,new))))

(defmacro move-repeating (old new start arg keyworded &optional until)
  (let ((e (gensym "e")))
  `(progn
     (if ,until
	 (error "can't do until yet") ;; @@@
	 (progn
	   (if ,keyworded
	       (progn
		 (setf ,new (push (arg-key ,arg) ,new))
		 (setf ,new (push (nthcdr ,start ,old) ,new)))
	       (loop :for ,e :in (nthcdr ,start ,old) :do
		  (setf ,new (push ,e ,new))))
	   (setf ,old (subseq ,old 0 ,start)))))))

(defun posix-to-lisp-args (command p-args)
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
	#| (optionals '()) |#)
    ;; Flagged arguments (optional or manditory)
    (loop :for a :in p-args :do
       (if (and (stringp a) (> (length a) 0)
		(char= (char a 0) #\-)) ; arg starts with dash
	   (if (eql (char a 1) #\-)		    ; two dash arg
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
		(not (arg-has-flag arg)) (> (length old-list) 0))
	   (move-key old-list new-optionals 0 arg keyworded)
	   (incf i)))
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

;; EOF
