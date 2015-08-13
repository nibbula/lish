;;
;; args.lisp - Command arguments.
;;

(in-package :lish)

(defclass argument ()
  ((name
    :documentation "Name"
    :initarg :name
    :accessor arg-name)
   (type
    :documentation "Declared type"
    :initarg :type
    :accessor arg-type)
   (value
    :documentation "Value"
    :initarg :value
    :accessor arg-value)
   (default
    :documentation "Default value, if optional."
    :initarg :default
    :initform nil
    :accessor arg-default)
   (repeating
    :type boolean
    :documentation "True if value can repeat."
    :initarg :repeating
    :initform nil
    :accessor arg-repeating)
   (optional
    :type boolean
    :documentation "True if a value is not required."
    :initarg :optional
    :initform t
    :accessor arg-optional)
   (hidden
    :type boolean
    :documentation "If true, don't show in help."
    :initarg :hidden
    :initform nil
    :accessor arg-hidden)
   (use-supplied-flag
    :type boolean
    :documentation
    "True to set the argument supplied flag when calling functions. The flag
     will be named <name>-SUPPLIED-P."
    :initarg :use-supplied-flag
    :initform nil
    :accessor arg-use-supplied-flag)
   (prompt
    :type string
    :documentation "Show when asking user for value."
    :initarg :propmt
    :accessor arg-propmt)
   (help
    :type string
    :documentation "Description for the user."
    :initarg :help
    :accessor arg-help)
   (completion-function
    :type function-designator
    :initarg :completion-function
    :accessor arg-completion-function
    :initform nil
    :documentation "A special completion function for the argument.")
   (short-arg
    :type (or character null)
    :documentation "Command line argument, short form."
    :initarg :short-arg
    :initform nil
    :accessor arg-short-arg)
   (long-arg
    :type (or string null)
    :documentation "Command line argument, long form."
    :initarg :long-arg
    :initform nil
    :accessor arg-long-arg)
   (old-long-arg
    :type (or string null)
    :documentation "Command line argument, old long form, with a single dash."
    :initarg :old-long-arg
    :initform nil
    :accessor arg-old-long-arg))
  (:documentation "Generic command parameter."))

(defmethod initialize-instance :after
    ((o argument) &rest initargs &key &allow-other-keys)
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

(defgeneric convert-arg (arg value &optional quoted)
  (:documentation "Convert an argument value from one type to another.")
  (:method ((arg argument) value &optional quoted)
    "The base default conversion just returns the value."
    (declare (ignore quoted))
    value))

(defgeneric argument-choices (arg)
  (:documentation
   "Return a list of possible argument values or nil if unknown.")
  (:method ((arg argument))
    "The default choices are unknown."
    nil))

(defclass arg-boolean (argument) () (:documentation "A true or false value."))
(define-constant +true-strings+ '("T" "TRUE" "YES" "ON" "1"))
(define-constant +false-strings+ '("NIL" "FALSE" "NO" "OFF" "0"))
(defmethod convert-arg ((arg arg-boolean) (value string) &optional quoted)
  (declare (ignore quoted))
  (cond
    ((position value +true-strings+ :test #'equalp) t)
    ((position value +false-strings+ :test #'equalp) nil)
    (t (error "Can't convert ~w to a boolean." value))))
;; (defmethod argument-choices ((arg arg-boolean))
;;   (concatenate 'list +true-strings+ +false-strings+))

(defclass arg-number (argument) () (:documentation "A number."))
(defmethod convert-arg ((arg arg-number) (value string) &optional quoted)
  (declare (ignore quoted))
  (let* ((*read-eval* nil)
	 (num (read-from-string value nil nil)))
    (if (and num (numberp num))
	num
	(error "Can't convert ~w to a number." value))))

;; @@@ This should probably support the same args as PARSE-INTEGER and be
;; put somewhere else.
(defun parse-integer-with-radix (str)
  "Parse an integer from a string, allowing for a Lisp radix prefix."
  (cond
    ((and str (> (length str) 2)
	  (char= (char str 0) #\#))
     (cond
       ((position (char str 1) #(#\b #\o #\x) :test #'char-equal)
	(parse-integer str :junk-allowed nil :start 2
		       :radix (cond
				((char-equal (char str 1) #\b) 2)
				((char-equal (char str 1) #\o) 8)
				((char-equal (char str 1) #\x) 16))))
       ((digit-char-p (char str 1))
	(let (radix start)
	  (cond
	    ((char-equal #\r (char str 2))
	     (setf radix (parse-integer str :start 1 :end 2 :junk-allowed nil)
		   start 3))
	    ((and (digit-char-p (char str 2))
		  (char-equal #\r (char str 3)))
	     (setf radix (parse-integer str :start 1 :end 3 :junk-allowed nil)
		   start 4))
	    (t
	     (error "Malformed radix in integer ~a." str)))
	  (parse-integer str :junk-allowed nil :start start :radix radix)))
       (t
	(error "Malformed integer ~a." str))))
    (t
     (parse-integer str :junk-allowed nil))))

(defclass arg-integer (arg-number) () (:documentation "An integer."))
(defmethod convert-arg ((arg arg-integer) (value string) &optional quoted)
  (declare (ignore quoted))
  (let ((int (parse-integer-with-radix value)))
    (if (and int (integerp int))
	int
	(error "Can't convert ~w to an integer." value))))

(defclass arg-float (arg-number) ()
  (:documentation "An floating point number."))
(defmethod convert-arg ((arg arg-float) (value string) &optional quoted)
  (declare (ignore quoted))
  (let* ((*read-eval* nil)
	 (num (read-from-string value nil nil)))
    (if (and num (floatp num))
	num
	(error "Can't convert ~w to a float." value))))

(defclass arg-character (argument) () (:documentation "A character."))
(defmethod convert-arg ((arg arg-character) (value character) &optional quoted)
  (declare (ignore arg quoted))
  value)

(defmethod convert-arg ((arg arg-character) (value string) &optional quoted)
  (declare (ignore arg quoted))
  (if (= (length value) 1)
      (char value 0)
      (or (name-char value)
	  (error "Can't convert character arg from string: ~s" value))))

(defclass arg-string (argument) () (:documentation "A string."))
(defmethod convert-arg ((arg arg-string) (value string) &optional quoted)
  (declare (ignore arg quoted))
  value)

(defmethod convert-arg ((arg arg-string) (value t) &optional quoted)
  (declare (ignore arg quoted))
  (princ-to-string value))

(defclass arg-symbol (argument) () (:documentation "A symbol."))
(defmethod convert-arg ((arg arg-symbol) (value string) &optional quoted)
  (declare (ignore arg quoted))
  value)

(defmethod convert-arg ((arg arg-symbol) (value t) &optional quoted)
  (declare (ignore arg quoted))
  (princ-to-string value))

(defclass arg-keyword (argument) () (:documentation "A Lisp keyword."))
(defmethod convert-arg ((arg arg-keyword) (value string) &optional quoted)
  (declare (ignore quoted))
  (if (char/= (char arg 0) #\:)
      (intern (string-upcase (subseq value 1)) (find-package :keyword))
      value))

(defclass arg-object (argument) () (:documentation "A Lisp object."))
(defmethod convert-arg ((arg arg-object) (value string) &optional quoted)
  (if quoted
      value
      (multiple-value-bind (obj end) (safe-read-from-string value)
	;; If the whole string wasn't an object, just treat as a string.
	(if (= end (length value))
	    obj
	    value))))

(defclass arg-date (argument) () (:documentation "A date."))
(defmethod convert-arg ((arg arg-date) (value string) &optional quoted)
  (declare (ignore arg quoted))
  ;; @@@ This could be better.
  value)

(defclass arg-pathname (arg-string) ()
  (:default-initargs
   :completion-function #'complete-filename)
  (:documentation "A file system path."))
(defmethod convert-arg ((arg arg-pathname) (value string) &optional quoted)
  (declare (ignore arg quoted))
  value)

;; (defmethod argument-choices ((arg arg-pathname))
;;   "Return the possible path names."
;;   ;; @@@ Perhaps we should just use opsys:read-directory ?
;;   (completion::filename-completion-list ""))

(defclass arg-directory (arg-pathname) ()
  (:default-initargs
   :completion-function #'complete-directory)
  (:documentation "A file system directory."))
(defmethod convert-arg ((arg arg-directory) (value string) &optional quoted)
  (declare (ignore arg quoted))
  value)

;; (defmethod argument-choices ((arg arg-directory))
;;   "Return the possible directory names."
;;   ;; @@@ Perhaps we should just use opsys:read-directory ?
;;   (completion::filename-completion-list ""))

(defclass arg-stream (argument)
  () (:documentation "An stream of some sort."))
(defmethod convert-arg ((arg arg-pathname) (value symbol) &optional quoted)
  (declare (ignore arg quoted))
  (symbol-value value))

(defclass arg-input-stream (arg-stream)
  () (:documentation "An input stream."))
(defclass arg-output-stream (arg-stream)
  () (:documentation "An output stream."))
(defclass arg-io-stream (arg-stream)
  () (:documentation "An input/output stream."))

(defclass arg-stream-or-filename (arg-stream)
  () (:documentation "An stream or a filename."))
(defclass arg-input-stream-or-filename (arg-stream-or-filename)
  () (:documentation "An input stream or a filename."))
(defclass arg-output-stream-or-filename (arg-stream-or-filename)
  () (:documentation "An output stream or a filename."))
(defclass arg-io-stream-or-file (arg-stream-or-filename)
  () (:documentation "An I/O stream or a filename."))

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

(defmethod convert-arg ((arg arg-choice) (value string) &optional quoted)
  (declare (ignore quoted))
  (let (choice
	(choices (argument-choices arg)))
    (unless choices
      (error "Choice argument has no choices ~a." (arg-name arg)))
    (if (setf choice (find value choices
			   :test #'(lambda (a b)
				     (equalp a (princ-to-string b)))))
	choice
	(error "~s is not one of the choices for the argument ~:@(~a~)."
	       value (arg-name arg)))))

(defmethod argument-choices ((arg arg-choice))
  (cond
    ((slot-boundp arg 'choices)
     (arg-choices arg))
    ((and (slot-boundp arg 'choice-func)
	  (arg-choice-func arg))
     (funcall (arg-choice-func arg)))
    (t nil)))

(defclass arg-lenient-choice (arg-choice)
  ()
  (:documentation
   "An argument with known choices, but accepting anything."))
  
(defmethod convert-arg ((arg arg-lenient-choice) (value string)
			&optional quoted)
  (declare (ignore quoted))
  (let (choice
	(choices (argument-choices arg)))
    (if (not choices)
	(warn "Choice argument has no choices ~a." (arg-name arg))
	(if (setf choice (find value choices
			       :test #'(lambda (a b)
					 (equalp a (princ-to-string b)))))
	    choice
	    (progn
	      (warn "~s is not one of the choices for the argument ~:@(~a~)."
		    value (arg-name arg))
	      value)))))

;; This is so if it's not provided, it can toggle.
(defclass arg-boolean-toggle (arg-boolean)
  ()
  (:default-initargs
   :default :toggle)
  (:documentation "A true or false value, that can be toggled."))
(defmethod initialize-instance
    :after ((o arg-boolean-toggle) &rest initargs &key &allow-other-keys)
  "Initialize a arg-boolean-toggle."
  (declare (ignore initargs))
  (setf (arg-default o) :toggle))
;; (defmethod convert-arg ((arg arg-boolean-toggle) (value string) &optional quoted)
;;   (declare (ignore quoted))
;;   (cond
;;     ((position value +true-strings+ :test #'equalp) t)
;;     ((position value +false-strings+ :test #'equalp) nil)
;;     (t (error "Can't convert ~w to a boolean." value))))

(defun option-name-list ()
  (loop :for o :in *options* :collect (arg-name o)))

(defclass arg-option (arg-choice)
  ()
  (:default-initargs
   :choice-func #'option-name-list)
  (:documentation "A shell option."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun argument-class-name (symbol)
    "Return a string representing the argument class for the SYMBOL."
    (s+ "ARG-" (string-upcase symbol))))

(defun argument-type-class (type)
  "Return the argument class for a given type. If the type is not a defined
ARG-* class, it defaults to the generic ARGUMENT class."
  (flet ((try-sym (s pkg)
	   (find-class
	    (and (find-package pkg)
		 (find-symbol (argument-class-name s) pkg))
	    nil)))
    (cond
      ((listp type)
       (if (not (eq (car type) 'or))
	   (error "Only (or ...) compound types are supported.")
	   'argument))
      ((or (eq type t) (eq type 't) (and (stringp type) (equalp type "T")))
       'argument)
      ((symbolp type)
       (or (try-sym type :lish-user)
	   (try-sym type :lish)
	   (try-sym type (symbol-package type))
	   (progn (warn "Defaulting argument type ~s" type) 'argument)))
      ((stringp type)
       (or (try-sym type :lish-user)
	   (try-sym type :lish)
	   (try-sym type *package*))
       (progn (warn "Defaulting argument type ~s" type) 'argument))
      (t
       (error "Argument type is not a symbol, string or T.")))))

(defun arglist-value (arglist key)
  "Return a value from a DEFCOMMAND arglist argument."
  (let ((p (position key arglist)))
    (and p (elt arglist (1+ p)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-argument (a &optional compile-time)
    (let (name type)
      (when (not (listp a))
	(error "Argument list element must be a list."))
      (setf name (first a)
	    type (second a))
      (when (not name)
	(error "Arguments must have a name."))
      (when (not type)
	(error "Arguments must have a type."))
      (setf a (append (list :name name :type type) (cddr a)))
      (apply #'make-instance
	     (if compile-time
		 'argument
		 (argument-type-class type))
	     a)))

  (defun make-argument-list (arglist &optional compile-time)
    "Take an ARGLIST from DEFCOMMAND and turn it into a list of argument
objects, like in the command object."
;;;  (declare (type list arglist))
    (when (not (listp arglist))
      (error "Command argument list must be a list."))
    (loop :for a :in arglist :collect (make-argument a compile-time))))
       
;; You can just do you own defclass, but the problem is what package
;; it gets defined in, because we do snipping off of the ARG- prefix.
;; It seems like we could either:
;;   1. Require the ARG- prefix everywhere, and require exporting, importing
;;      and normal symbol management.
;; or
;;   2. Just define all ARG- classes in a lish package and look them up
;;      there too.
;; I'm taking the second option for now, even though I may regret it.

;; Yet another defclass wrapper.
(defmacro %defargtype (name package (&rest superclasses) &body body)
  "See the documentation for defargtype."
  (let* ((bod body)
	 (class-name (intern (argument-class-name name) package))
	 (doc (when (stringp (first bod))
		(pop bod)))
	 (slots (if (listp (first bod))
		    (pop bod)
		    (error "Missing slot list in defargtype.")))
	 (conversions '()))
    (loop
       :for form = bod :then (cdr bod)
       :while (not (endp form))
       :do
       (cond
	 ((keywordp (car form))
	  (case (car form)
	    (:convert
	     (let ((val-type (cadr form))
		   (conv-body (cddr form)))
	       (pop bod)
	       (pop bod)
	       (push
		`(defmethod convert-arg ((arg ,class-name) (value ,val-type)
					 &optional quoted)
		   (declare (ignorable arg value quoted))
		   ,@conv-body)
		conversions)))
	    (otherwise
	     (error "Unknown keyword in defargtype: ~s." (car form)))))
	 (t
	  (error "Unknown form in defargtype: ~s." (car form)))))
    `(progn
       (defclass ,class-name ,superclasses
	 ,slots
	 ,@(when doc `((:documentation ,doc))))
       ,@conversions)))

(defmacro defargtype (name (&rest superclasses) &body body)
  "Define a command argument type. The syntax is something like:

(defargtype foo (superclasses...)
  \"doc\"
  ((slot :blah blah))
  :convert zoo-type
    (and (arg-foo-ish arg) (bar-ize value))
  :convert zib-type
    (progn
      (zabble arg)
      (zibble arg value)))

This defines a class ARG-FOO with SUPERCLASSES, with slot definitions suitable
for DEFCLASS. DOC is optional class documentation. The optional :CONVERT
clauses define ARG-CONVERT methods for the new argument class and the given
type, e.g.:
  (arg-convert ((arg arg-foo) (value zoo-type)))
where ARG is the name of the argument and VALUE is the name of the
value to be converted.
"
  `(%defargtype ,name :lish-user (,@superclasses) ,@body))

(defmacro define-builtin-arg-type (name (&rest superclasses) &body body)
  `(%defargtype ,name :lish (,@superclasses) ,@body))

(defun arg-has-flag (arg)
  (or (arg-short-arg arg) (arg-long-arg arg)))

;; They must be keyworded if there are any flagged arguments.
(defun args-keyworded (args)
  "Check if an argument must be keyworded."
  (loop :for a :in args :do
     (when (arg-has-flag a)
       (return-from args-keyworded t)))
  nil)

;; Thankfully this is nowhere near as hairy as posix-to-lisp-args.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun command-to-lisp-args (command-args)
    "Return a Lisp argument list for the given lish argument list."
    (let ((mandatories
	   (loop :for a :in command-args
	      :if (not (arg-optional a))
	      :collect a))
	  (optionals
	   (loop :for a :in command-args
	      :if (arg-optional a)
	      :collect a))
	  (repeating
	   (loop :for a :in command-args
	      :if (arg-repeating a)
	      :collect a))
	  (keyworded (args-keyworded command-args))
	  (new-list '()))
      ;; Mandatory arguments
      (loop :for a :in mandatories :do
	 (push (symbolify (arg-name a)) new-list))
      ;; This is augmented here to allow for (mostly theoretical) paralellism
      ;; in the let above.
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
		    (if (arg-use-supplied-flag a)
			(list (symbolify (arg-name a)) (arg-default a)
			      (symbolify (s+ (arg-name a) "-supplied-p")))
			(list (symbolify (arg-name a)) (arg-default a)))
		    (if (arg-use-supplied-flag a)
			;; The default is NIL then.
			(list (symbolify (arg-name a)) NIL
			      (symbolify (s+ (arg-name a) "-supplied-p")))
			(symbolify (arg-name a))))
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
		     (if (arg-use-supplied-flag a)
			 (list (symbolify (arg-name a)) (arg-default a)
			       (symbolify (s+ (arg-name a) "-supplied-p")))
			 (list (symbolify (arg-name a)) (arg-default a)))
		     (if (arg-use-supplied-flag a)
			 ;; The default is NIL then.
			 (list (symbolify (arg-name a)) NIL
			       (symbolify (s+ (arg-name a) "-supplied-p")))
			 (symbolify (arg-name a))))
		 new-list)))))
      (nreverse new-list))))

;; EOF
