;;;
;;; args.lisp - Command arguments.
;;;

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
    :documentation "True if the value can repeat."
    :initarg :repeating
    :initform nil
    :accessor arg-repeating)
   (rest
    :type boolean
    :documentation
    "True if the argument is repeating and consumes the rest of the arguments,
     ignoring arguments that look like flags. Setting this will also make
     repeating be true."
    :initarg :rest
    :initform nil
    :accessor arg-rest)
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
    ;;:initform nil
    :accessor arg-short-arg)
   (long-arg
    :type (or string null)
    :documentation "Command line argument, long form."
    :initarg :long-arg
    ;;:initform nil
    :accessor arg-long-arg)
   (old-long-arg
    :type (or string null)
    :documentation "Command line argument, old long form, with a single dash."
    :initarg :old-long-arg
    ;;:initform nil
    :accessor arg-old-long-arg)
   (pattern
    :type (or string null)
    :documentation "Pattern to match."
    :initarg :pattern
    :initform nil 
    :accessor arg-pattern)
   (override ;; @@@ get rid of this after real parsing
    :initarg :override :accessor arg-override :initform nil
    :documentation
    "A boolen to process before other arguments, or a function that given the
 argument returns a boolean indicating whether to overrride.")
   (flattenable-p
    :initarg :flattenable-p :accessor arg-flattenable-p
    :type boolean :initform nil
    :documentation "True if we can flatten multiple arguments of this type."))
  (:documentation "Generic command parameter."))

(defmethod initialize-instance :after
    ((o argument) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  ;; Make the long-arg default to the name if the short-arg is set.
  (when (and (slot-boundp o 'short-arg)
	     (slot-value o 'short-arg)
	     (not (slot-boundp o 'long-arg)))
    (setf (slot-value o 'long-arg) (princ-to-string (slot-value o 'name))))

  (when (not (slot-boundp o 'short-arg))
    (setf (slot-value o 'short-arg) nil))

  (when (not (slot-boundp o 'long-arg))
    (setf (slot-value o 'long-arg) nil))

  (when (not (slot-boundp o 'old-long-arg))
    (setf (slot-value o 'old-long-arg) nil))

  ;; Setting REST forces REPEATING to be true.
  (when (and (slot-boundp o 'rest) (slot-value o 'rest))
    (setf (slot-value o 'repeating) t)))

(defvar *arg-normal-flag-char* #\-
  "Normal argument flag character.")

(defvar *arg-opposite-flag-char* #\+
  "Opposite argument flag character.")

(defvar *arg-flag-chars* (vector *arg-normal-flag-char*
				 *arg-opposite-flag-char*)
  "Sequence of possible argument flag characters.")

(defun is-flag-char (c)
  "Return true if C is a flag character."
  (position c *arg-flag-chars*))

(defun is-normal-flag-char (c)
  "Return true if C is a normal flag character."
  (char= c *arg-normal-flag-char*))

(defun is-opposite-flag-char (c)
  "Return true if C is an opposite flag character."
  (char= c *arg-opposite-flag-char*))

(defmethod print-object ((o argument) stream)
  "Print a lish command argument in an unreadable way."
  (let ((short-arg (and (slot-boundp o 'short-arg) (arg-short-arg o)))
	(long-arg (and (slot-boundp o 'long-arg) (arg-long-arg o))))
    (print-unreadable-object (o stream :identity nil :type t)
      (format stream
	      "~a~:[~; repeating~]~:[~; optional~]~:[~; hidden~]~:[~; rest~]~
              ~:[~2*~; ~c~a~]~:[~3*~; ~c~c~a~]"
	      (arg-name o)
	      ;; (arg-type o)
	      (arg-repeating o)
	      (arg-optional o)
	      (arg-hidden o)
	      (arg-rest o)
	      short-arg *arg-normal-flag-char* short-arg
	      long-arg *arg-normal-flag-char* *arg-normal-flag-char* long-arg))))

(define-condition argument-error (shell-error cell-error)
  ()
  (:report (lambda (c s)
	     (when (shell-error-format c)
	       (format s "~?"
		       (shell-error-format c)
		       (shell-error-arguments c)))))
  (:documentation "An error with command arguments."))

(defun arg-error (format &rest arguments)
  "Shorthand for an argument error."
  (restart-case
      (signal 'argument-error :format format :arguments arguments)
    (use-value (value)
      :report "Use a new value."
      :interactive (lambda ()
		     (multiple-value-list
		      (eval
		       (read-from-string
			(rl:rl :prompt "Enter a new value (evaluated): ")))))
      value)))

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
    (t (arg-error "Can't convert ~w to a boolean." value))))
;; (defmethod argument-choices ((arg arg-boolean))
;;   (concatenate 'list +true-strings+ +false-strings+))

(defclass arg-number (argument) () (:documentation "A number."))
(defmethod convert-arg ((arg arg-number) (value string) &optional quoted)
  (declare (ignore quoted))
  (let* ((num (safe-read-from-string value nil nil)))
    (if (and num (numberp num))
	num
	(arg-error "Can't convert ~w to a number." value))))

(defclass arg-integer (arg-number) () (:documentation "An integer."))
(defmethod convert-arg ((arg arg-integer) (value string) &optional quoted)
  (declare (ignore quoted))
  (handler-case
      (let ((int (parse-integer-with-radix value)))
	(if (and int (integerp int))
	    int
	    (arg-error "Can't convert ~w to an integer." value)))
    ;; Make the error be a little more descriptive.
    (error (c)
      (arg-error "Can't convert arg ~w to an integer: ~a" value c))))

(defclass arg-float (arg-number) ()
  (:documentation "An floating point number."))
(defmethod convert-arg ((arg arg-float) (value string) &optional quoted)
  (declare (ignore quoted))
  (let* ((num (safe-read-from-string value nil nil)))
    (if (and num (floatp num))
	num
	(arg-error "Can't convert ~w to a float." value))))

(defclass arg-character (argument) () (:documentation "A character."))
(defmethod convert-arg ((arg arg-character) (value character) &optional quoted)
  (declare (ignore arg quoted))
  value)

(defmethod convert-arg ((arg arg-character) (value string) &optional quoted)
  (declare (ignore arg quoted))
  (if (= (length value) 1)
      (char value 0)
      (or (name-char value)
	  (arg-error "Can't convert character arg from string: ~s" value))))

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
      (handler-case
	  (multiple-value-bind (obj end) (safe-read-from-string value)
	    ;; If the whole string wasn't an object, just treat as a string.
	    (if (= end (length value))
		obj
		value))
	(error ()
	  ;; Just return the string value if we have problems reading it.
	  (warn "Couldn't convert argument ~s into a Lisp object." value)
	  value))))

(defclass arg-case-preserving-object (argument) ()
  (:documentation "A Lisp object, read with case preserved."))
(defmethod convert-arg ((arg arg-case-preserving-object) (value string)
			&optional quoted)
  (if quoted
      value
      (handler-case
	  (multiple-value-bind (obj end)
	      (fancy-read-from-string value :safe t :case :preserve)
	    ;; If the whole string wasn't an object, just treat as a string.
	    (if (= end (length value))
		obj
		value))
	(error ()
	  ;; Just return the string value if we have problems reading it.
	  (warn "Couldn't convert argument ~s into a Lisp object." value)
	  value))))

(defclass arg-sequence (arg-object) () (:documentation "A sequence."))
(defmethod convert-arg ((arg arg-sequence) (value sequence) &optional quoted)
  (declare (ignore arg quoted))
  value)

(defclass arg-list (arg-object) () (:documentation "A list."))
(defmethod convert-arg ((arg arg-list) (value list) &optional quoted)
  (declare (ignore arg quoted))
  ;; @@@ We could probably do better here.
  ;; Maybe?: (let (result) (omap (_ (push _ result)) value) result)
  (coerce value 'list))

(defclass arg-function (arg-symbol) () (:documentation "A function name."))
;; (defmethod convert-arg ((arg arg-function) (value string) &optional quoted)
;;   (declare (ignore arg quoted))
;;   (symbolify value))

(defclass arg-package (arg-keyword)
  ()
  (:documentation
   "A package designator, either a keyword or a package."))

(defmethod convert-arg ((arg arg-package) value &optional quoted)
  (declare (ignore arg quoted))
  (if (stringp value)
      (intern (string-upcase
	       (if (char= (char value 0) #\:)
		   (subseq value 1)
		   value)) (find-package :keyword))
      value))

(defmethod argument-choices ((arg arg-package))
  (declare (ignore arg))
  (sort-muffled
   (mapcar (_ (string-downcase (package-name _))) (list-all-packages))
   #'string<))

(defclass arg-date (argument) () (:documentation "A date."))
(defmethod convert-arg ((arg arg-date) (value string) &optional quoted)
  (declare (ignore arg quoted))
  ;; @@@ This could be better when we have a date parser.
  value)

(defclass arg-pathname (arg-string) ()
  (:default-initargs
   :completion-function #'shell-complete-filename
   :flattenable-p t)
  (:documentation "A file system path."))
(defmethod convert-arg ((arg arg-pathname) (value string) &optional quoted)
  (declare (ignore arg quoted))
  value)
(defmethod convert-arg ((arg arg-pathname)
			(value file-expansion) &optional quoted)
  (declare (ignore arg quoted))
  (file-expansion-files value))

;; (defmethod argument-choices ((arg arg-pathname))
;;   "Return the possible path names."
;;   ;; @@@ Perhaps we should just use opsys:read-directory ?
;;   (completion::filename-completion-list ""))

(defclass arg-directory (arg-pathname) ()
  (:default-initargs
   :completion-function #'shell-complete-directory)
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
(defmethod convert-arg ((arg arg-stream) (value symbol) &optional quoted)
  (declare (ignore arg quoted))
  (symbol-value value))

(defclass arg-input-stream (arg-stream)
  () (:documentation "An input stream."))
(defclass arg-output-stream (arg-stream)
  () (:documentation "An output stream."))
(defclass arg-io-stream (arg-stream)
  () (:documentation "An input/output stream."))

(defclass arg-stream-or-filename (arg-stream arg-pathname)
  () (:documentation "An stream or a filename."))
(defclass arg-input-stream-or-filename (arg-stream-or-filename)
  () (:documentation "An input stream or a filename."))
(defclass arg-output-stream-or-filename (arg-stream-or-filename)
  () (:documentation "An output stream or a filename."))
(defclass arg-io-stream-or-file (arg-stream-or-filename)
  () (:documentation "An I/O stream or a filename."))

(defun arg-choice-compare-ignore-case (choice value)
  "Return true if VALUE as a string is equalp to CHOICE."
  (equalp choice (princ-to-string value)))

(defun arg-choice-compare (choice value)
  "Return true if VALUE as a string is equal to CHOICE."
  ;; (equal choice (princ-to-string value))
  (string-equal choice (princ-to-string value))
  )

(defclass arg-choice (argument)
  ((choices
    :type list
    :documentation "A list of choices for value."
    :initarg :choices
    :accessor arg-choices)
   (choice-labels
    :type list
    :documentation "A list of string names for choices."
    :initarg :choice-labels
    :accessor arg-choice-labels)
   (choice-func
    :type (or function symbol)
    :documentation "A function to call to get the list of choices."
    :initarg :choice-func
    :accessor arg-choice-func)
   (test
    :initarg :test :accessor arg-choice-test :type function
    :documentation "Function to compare values to choices."))
  (:default-initargs
   :test #'arg-choice-compare-ignore-case)
  (:documentation "An argument whose value must be one of a list of choices."))

(defmethod convert-arg ((arg arg-choice) (value string) &optional quoted)
  (declare (ignore quoted))
  (let (choice
	(choices (argument-choices arg)))
    (unless choices
      (error "Choice argument has no choices ~a." (arg-name arg)))
    (if (setf choice (find value choices :test (arg-choice-test arg)))
	choice
	(arg-error "~s is not one of the choices for the argument ~:@(~a~)."
		   value (arg-name arg)))))

(defmethod argument-choices ((arg arg-choice))
  (cond
    ((slot-boundp arg 'choices)
     (arg-choices arg))
    ((and (slot-boundp arg 'choice-func)
	  (arg-choice-func arg))
     (funcall (arg-choice-func arg)))
    (t nil)))

(defmethod arg-help ((arg arg-choice))
  (let ((help (if (slot-boundp arg 'help)
		  (slot-value arg 'help) nil)))
    (cond
      ((and (slot-boundp arg 'choices) (slot-value arg 'choices))
       (format nil "~@[~a ~]Choices: ~{~s~^ ~}" help (arg-choices arg)))
      ((and (slot-boundp arg 'choice-func) (slot-value arg 'choice-func))
       (format nil "~@[~a ~]Choice function: ~a" help (arg-choice-func arg)))
      ((and (slot-boundp arg 'help) (slot-value arg 'help))
       help)
      (t ""))))

(defclass arg-lenient-choice (arg-choice)
  ((quiet
    :initarg :quiet :accessor arg-lenient-choice-quiet
    :initform nil :type boolean
    :documentation "True to not warn if the choice is not in the list."))
  (:documentation
   "An argument with known choices, but accepting anything."))
  
(defmethod convert-arg ((arg arg-lenient-choice) (value string)
			&optional quoted)
  (declare (ignore quoted))
  (let (choice
	(choices (argument-choices arg)))
    (if (not choices)
	(when (not (arg-lenient-choice-quiet arg))
	  (warn "Choice argument has no choices ~a." (arg-name arg)))
	(if (setf choice (find value choices :test (arg-choice-test arg)))
	    choice
	    (progn
	      (when (not (arg-lenient-choice-quiet arg))
		(warn "~s is not one of the choices for the argument ~:@(~a~)."
		      value (arg-name arg)))
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
;;     (t (arg-error "Can't convert ~w to a boolean." value))))

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

(defun new-argument-type-class (type)
  (cond
    ((listp type)
     (if (not (eq (car type) 'or))
	 (error "Only (or ...) compound types are supported.")
	 'argument))
    ((or (eq type t) (eq type 't) (and (stringp type) (equalp type "T")))
     'argument)
    ((or (symbolp type) (stringp type))
     (symbolify (argument-class-name type) :package :lish-user))
    (t
     (error "Argument type is not a symbol, string or T."))))

(defun arglist-value (arglist key)
  "Return a value from a DEFCOMMAND arglist argument."
  (let ((p (position key arglist)))
    (and p (elt arglist (1+ p)))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun check-argument (a)
    (let (name type)
      (when (not (listp a))
	(error "Argument list element must be a list."))
      (setf name (first a)
	    type (second a))
      (when (not name)
	(error "Arguments must have a name."))
      (when (not type)
	(error "Arguments must have a type."))))

  (defun transform-arg (a)
    (let ((name (first a))
	  (type (second a)))
      (when (symbolp name)
	(setf name (string-downcase name)))
      (setf a (append `(:name ,name :type ',type) (cddr a)))))

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
      (when (symbolp name)
	(setf name (string-downcase name)))
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
	 default-initargs
	 (conversions '()))
    (loop
       :for form = bod :then (cdr form)
       :while (not (endp form))
       :do
       (cond
	 ((keywordp (car form))
	  (case (car form)
	    (:convert
	     (let ((val-type (cadr form))
		   (conv-body (copy-list (caddr form))))
	       ;; (format *trace-output* "before form = ~s~%" form) (finish-output *trace-output*)
	       ;; Not cdddr because we have to leave one for the loop to eat.
	       (setf form (cddr form))
	       ;; (format *trace-output* "after form = ~s~%" form) (finish-output *trace-output*)
	       (push
		`(defmethod convert-arg ((arg ,class-name) (value ,val-type)
					 &optional quoted)
		   (declare (ignorable arg value quoted))
		   ,conv-body)
		conversions)))
	    ;; @@@ What about :metaclass ?
	    (otherwise
	     (error "Unknown keyword in defargtype: ~s." (car form)))))
	 ((listp (car form))
	  (case (caar form)
	    (:default-initargs
	     (setf default-initargs (car form))
	     ;;(pop form)
	      )
	    (otherwise
	     (error "Unknown clause in defargtype: ~s." (car form)))))
	 (t
	  (error "Unknown form in defargtype: ~s." (car form)))))
    `(progn
       (defclass ,class-name ,superclasses
	 ,slots
	 ,@(when default-initargs `(,default-initargs))
	 ,@(when doc `((:documentation ,doc))))
       ,@conversions)))

(defmacro defargtype (name (&rest superclasses) &body body)
  "Define a command argument type. The syntax is something like:

(defargtype foo (superclasses...)
  \"doc\"
  ((slot :blah blah))
  [ (:default-initargs ...) ]
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

See the documentation for ‘lish:argument’.
"
  `(%defargtype ,name :lish-user (,@superclasses) ,@body))

(defmacro define-builtin-arg-type (name (&rest superclasses) &body body)
  `(%defargtype ,name :lish (,@superclasses) ,@body))

(defun arg-has-flag (arg)
  (or (and (slot-boundp arg 'short-arg) (arg-short-arg arg))
      (and (slot-boundp arg 'long-arg) (arg-long-arg arg))))

;; They must be keyworded if there are any flagged arguments.
(defun args-keyworded (args)
  "Check if an argument must be keyworded."
  ;; (loop :for a :in args :do
  ;;    (when (arg-has-flag a)
  ;;      (return-from args-keyworded t)))
  ;; nil)
  (declare (ignore args))
  ;; HOW ABOUT THIS:
  t)

;; Thankfully this is nowhere near as hairy as posix-to-lisp-args.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun command-to-lisp-args (command-args &key pass-keys-as)
    "Return a Lisp argument list for the given Lish argument list."
    (let ((mandatories
	   (loop :for a :in command-args
	      :if (not (arg-optional a))
	      :collect a))
	  (optionals
	   (loop :for a :in command-args
	      :if (arg-optional a)
	      :collect a))
	  ;; (repeating
	  ;;  (loop :for a :in command-args
	  ;;     :if (arg-repeating a)
	  ;;     :collect a))
	  ;;(keyworded (or (args-keyworded command-args) pass-keys-as))
	  ;; (keyworded t)
	  (new-list '()))
      ;; Mandatory arguments
      ;; (loop :for a :in mandatories :do
      ;; 	 (push (symbolify (arg-name a)) new-list))
      ;; This is augmented here to allow for (mostly theoretical) paralellism
      ;; in the let above.
      ;; HOW ABOUT NOT!!:
      ;; (setf keyworded (or keyworded
      ;; 			  (and optionals repeating
      ;; 			       (and (not (equal optionals repeating))
      ;; 				    (= (length optionals) 1)))
      ;; 			  (> (length repeating) 1)))
      ;; (if keyworded
	  (progn
	    ;; Put in a rest argument to catch all the keys.
	    (when pass-keys-as
	      (push '&rest new-list)
	      (push pass-keys-as new-list))
	    (push '&key new-list)
	    ;; Mandatory arguments
	    (loop :for a :in mandatories :do
	       (push (symbolify (arg-name a)) new-list))
	    (loop :for a :in optionals :do
	       (push
		(if (arg-default a)
		    (if (arg-use-supplied-flag a)
			(list (symbolify (arg-name a)) (arg-default a)
			      (symbolify (s+ (arg-name a) "-supplied-p")))
			(list (symbolify (arg-name a)) (arg-default a)))
		    (if (arg-use-supplied-flag a)
			;; The default is NIL then.
			(list (symbolify (arg-name a)) 'NIL
			      (symbolify (s+ (arg-name a) "-supplied-p")))
			(symbolify (arg-name a))))
		new-list)))
	  #|
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
		 new-list))))) |#
      (nreverse new-list))))

;; EOF
