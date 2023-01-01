;;;
;;; piping.lisp - Piping for Lish
;;;

;; Piping, I/O redirection, and I/O functions that are useful for using in
;; a lish command line or script.

(in-package :lish)

(defun lisp-args-to-command (args &key (auto-space nil))
  "Turn the arguments into a string of arguments for a system command. String
arguments are concatenated together. Symbols are downcased and turned into
strings. Keywords are like symbols but prefixed with '--'. Everything else is
just turned into a string as printed with PRINC. If AUTO-SPACE is true, put
spaces between every argument."
  (with-output-to-string (str)
    (loop :with first-time = t
       :for a :in args :do
       (when auto-space
	 (if first-time
	     (setf first-time nil)
	     (princ " " str)))
       (typecase a
	 (keyword			; this is sort of goofy
	  (princ "--" str)
	  (princ (string-downcase (symbol-name a)) str))
	 (symbol
	  (princ (string-downcase (symbol-name a)) str))
	 (t
	  (princ a str))))))

(defun possibly-read (expr)
  (typecase expr
    (string (shell-read expr))
    (t expr)))

#|
;; This needs so much work.
(defun copy-stream (source destination)
  "Copy data from reading from SOURCE and writing to DESTINATION, until we get
an EOF on SOURCE."
  (let ((buf (make-array *buffer-size*
			 :element-type (stream-element-type source)))
	pos)
    (loop :do
       (setf pos (read-sequence buf source))
       (when (> pos 0)
	 (write-sequence buf destination :end pos))
       :while (= pos *buffer-size*))))
|#

(defun byte-copy-stream (source destination)
  "This seems like a slow thing that will only work on bivalent streams?"
  (loop :with b
     :while (setf b (read-byte source nil nil))
     :do (write-byte b destination)))

(defun append-files (input-files output-file &key callback)
  "Copy the data from ‘input-files’ appending it to ‘output-file’. Create
‘output-file’ if it doesn't exist."
  (with-open-file (out (quote-filename output-file) :direction :output
		       :if-exists :append
		       :if-does-not-exist :create
		       :element-type '(unsigned-byte 8))
    (loop :for file :in input-files :do
       (with-open-file (in (quote-filename file) :direction :input
			   :element-type '(unsigned-byte 8))
	 (when callback
	   (funcall callback file))
	 (copy-stream in out)))))

;; This is mostly for convenience from the command line
(defun append-file (input-file output-file)
  "Copy the data from INPUT-FILE appending it to OUTPUT-FILE. Create
OUTPUT-FILE if it doesn't exist."
  (append-files (list input-file) output-file))

(defun run-with-output-to (file-or-stream commands &key supersede append)
  "Run commands with output to a file or stream. COMMANDS can be a SHELL-EXPR,
or a list of arguments."
  (when (and supersede append)
    (error "Can't both supersede and append to a file."))
  (let ((result nil))
    (multiple-value-bind (vals in-stream)
	(shell-eval (possibly-read commands)
		    :context (modified-context *context* :out-pipe t))
      (unwind-protect
	   (when (and vals (> (length vals) 0))
	     (with-open-file-or-stream
		 (out-stream file-or-stream
			     :direction :output
			     :if-exists
			     (if supersede
				 :supersede
				 (if append
				     :append
				     :error))
			     :if-does-not-exist :create
			     #+sbcl :element-type #+sbcl :default
			     #-sbcl :element-type #-sbcl '(unsigned-byte 8)
			     )
	       #+sbcl
	       (if (and
		    (or (eq (stream-element-type in-stream) :default)
			(eq (stream-element-type in-stream) 'unsigned-byte))
		    (or (eq (stream-element-type out-stream) :default)
			(eq (stream-element-type out-stream) 'unsigned-byte)))
		   (byte-copy-stream in-stream out-stream)
		   (copy-stream in-stream out-stream))
	       #-sbcl (copy-stream in-stream out-stream))
	     (setf result vals))
	(when in-stream
	  (close in-stream))))
    result))

(defun run-with-input-from (file-or-stream commands)
  "Run commands with input from a file or stream. COMMANDS can be a SHELL-EXPR,
or a list to be converted by LISP-ARGS-TO-COMMAND."
  (let ((result nil))
    (with-open-file-or-stream (in-stream file-or-stream)
      (multiple-value-bind (vals)
	  (shell-eval (possibly-read commands)
		      :context (modified-context *context* :in-pipe in-stream))
	(setf result vals)))
    result))

(defun input-line-words ()
  "Return lines from *standard-input* as a string of words."
  (with-output-to-string (s)
    (loop :with l = nil :and first = t
       :while (setf l (read-line *standard-input* nil nil))
       :do
       (if first
	   (progn (format s "~a" l)
		  (setf first nil))
	   (format s " ~a" l)))))

(defun input-line-list (&optional (stream *standard-input*))
  "Return lines from *standard-input* as list of strings."
  (loop :with l = nil
     :while (setf l (read-line (or stream *standard-input*) nil nil))
     :collect l))

;; @@@ trying to make it work with binary streams?
;; (defun map-output-lines (func command &key binary)
;;   "Return a list of the results of calling the function FUNC with each output
;; line of COMMAND. COMMAND should probably be a string, and FUNC should take one
;; string as an argument."
;;   (let (vals stream)
;;     (unwind-protect
;;       (progn
;; 	(multiple-value-setq (vals stream)
;; 	  (shell-eval (possibly-read command)
;; 		      :context (modified-context *context* :out-pipe t)))
;; 	(when (and vals (> (length vals) 0))
;; 	  (loop
;; 	     :with l = nil
;; 	     :and str = (if binary
;; 			    (make-instance 'utf8b-stream:utf8b-input-stream
;; 					   :input-stream stream)
;; 			    stream)
;; 	     :while (setf l (read-line str nil nil))
;; 	     :collect (funcall func l))))
;;       (when stream
;; 	(close stream)))))

(defun map-output-lines (func command)
  "Return a list of the results of calling the function ‘func’ with each output
line of ‘command’. ‘command’ should probably be a string, and ‘func’ should take
one string as an argument."
  (let (vals stream)
    (unwind-protect
	 (progn
	   (multiple-value-setq (vals stream)
	     (shell-eval (possibly-read command)
			 :context (modified-context *context* :out-pipe t)))
	   (when (and vals (> (length vals) 0))
	     (loop
		:with l = nil
		:while (setf l (read-line stream nil nil))
		:collect (funcall func l))))
      (when stream
	(close stream)))))

;; This is basically backticks #\` or $() in bash.
(defun command-output-words (command &optional quoted)
  "Return lines output from command as a string of words."
  (labels ((convert-to-words (in-stream out-stream)
	     (loop :with l = nil :and first-time = t
		:while (setf l (read-line in-stream nil nil))
		:do
		(format out-stream
			(if quoted "~:[~; ~]\"~a\"" "~:[~; ~]~a")
			(not first-time) l)
		(setf first-time nil))))
    (with-output-to-string (s)
      (let (vals stream)
	(unwind-protect
	   (progn
	     (multiple-value-setq (vals stream)
	       (shell-eval (possibly-read command)
			   :context (modified-context *context* :out-pipe t)))
	     (when (and vals (> (length vals) 0))
	       (convert-to-words stream s)))
	  (when stream
	    (close stream)))))))

(defun command-output-list (command)
  "Return lines output from command as a list."
  (map-output-lines #'identity command))

(defun pipe (&rest commands)
  "Send output from commands to subsequent commands."
  (labels ((sub (cmds &optional in-stream)
	     (let (vals stream)
	       (unwind-protect
	         (progn
		   (multiple-value-setq (vals stream)
		     (shell-eval (possibly-read (car cmds))
				 :context (modified-context
					   *context*
					   :in-pipe in-stream
					   :out-pipe (and (cadr cmds) t))))
		   (if (and vals (listp vals) (> (length vals) 0))
		       (if (cdr cmds)
			   (apply #'pipe stream (cdr cmds))
			   (values-list vals))
		       nil))
		 (when (and stream (open-stream-p stream))
		   (finish-output stream)
		   (close stream))))))
    (if (streamp (car commands))
	(sub (cdr commands) (car commands))
	(sub commands))))

;; @@@ This doesn't really work the way I'd like because backgrounding of
;; compound commands needs some redesign. Or more like a total re-write.
(defun in-bg (expr)
  "Evaluate EXPR in the background. If it's a string, evaluate it as shell
command in the background. If it's something else, evaluate it as a lisp
expression in a separagte thread, if threads are supported."
  (typecase expr
    (string
     (shell-eval (shell-read expr)
		 :context (modified-context *context* :background t)))
    (t
     (shell-eval `(progn ,expr)
		 :context (modified-context *context* :background t)))))

(defun in-pipe-p ()
  "Return true if we have a pipeline as input."
  (when (and *shell* *context*)
    (and (context-in-pipe *context*) t)))

(defun out-pipe-p ()
  "Return true if we have a pipeline as output."
  (when (and *shell* *context*)
    (and (context-out-pipe *context*) t)))

(defgeneric input-source-p (thing)
  (:documentation "Return true if ‘thing’ could be an input source.")
  (:method ((thing stream)) (input-stream-p thing))
  (:method ((thing pathname)) (nos:file-exists thing))
  (:method ((thing string)) (nos:file-exists thing))
  (:method ((thing vector))
    (equal (array-element-type thing) '(unsigned-byte 8))))

#|
(defun input-source-p (thing)
  "Return true if ‘thing’ could be an input source."
  (or (and (streamp thing) (input-stream-p thing))
      (and (or (pathnamep thing) (stringp thing))
	   (nos:file-exists thing))
      ;; as string stream
      (stringp thing)
      ;; as byte array
      (and (vectorp thing)
	   (equal (array-element-type thing) '(unsigned-byte 8)))))
|#

(defun as-input-stream (thing)
  "Make an input stream out of ‘thing’."
  (etypecase thing
    (stream thing) ;; assuming it's an input stream
    (pathname
     (open thing :direction :input))
    (string
     (if (nos:file-exists thing)
	 ;; (open thing :direction :input)
	 (open (quote-filename thing) :direction :input)
	 (make-string-input-stream thing)))
    #|
    @@@ finish bi-valent-streams or os-streams?
    (vector
     (make-bivalent-stream thing))
    |#
    ))

(defmacro with-streamlike-input ((arg &key use-stdin) &body body)
  "Allow a command to take stream-like input, getting the input from the
argument ‘arg’, or from the the shell input *input*. Set ‘arg’ to be an input
stream, or NIL if we can't."
  (with-names (arg-value used-stdin)
    `(let ((,arg-value ,arg) ,used-stdin)
       (unwind-protect
	    (progn
	      (setf ,arg
		    (cond
		      ((and (null ,arg-value)
			    (and *input* (input-source-p *input*)))
		       (as-input-stream *input*))
		      ((and ,arg-value (input-source-p ,arg-value))
		       (as-input-stream ,arg-value))
		      (,use-stdin
		       (setf ,used-stdin t)
		       *standard-input*)))
	      ,@body)
	 (when (and ,arg (streamp ,arg) (not ,used-stdin))
	   (close ,arg))))))

(defgeneric possible-file-name-p (thing)
  (:documentation "Return true if ‘thing’ could be a file name.")
  (:method (thing) (declare (ignore thing)) nil)
  (:method ((thing pathname)) t)
  (:method ((thing os-pathname)) t)
  (:method ((thing string)) t)
  (:method ((thing vector))
    (equal (array-element-type thing) '(unsigned-byte 8))))

;; This is useful for commands which want to take a list of files as an argument
;; or as *input*.
(defmacro with-files-or-input ((arg &key on-unknown-input-type arg-list)
			       &body body)
  "Evaluate ‘body’ adding ‘*input*’ to ‘arg’ if it could be a file
or list of files. ‘arg’ should probably be the name of command argument that
could be a list of files. If ‘arg-list’ is given, it is the name of the command
argument plist, as provided to :args-as, and ‘arg’ is the keyword."
  (let ((place (if arg-list
		   `(getf ,arg-list (keywordify ,arg))
		   arg)))
    `(progn
       (cond
	 ((possible-file-name-p lish:*input*)
	  (push lish:*input* ,place))
	 ((consp lish:*input*)
	  ;; We only check first item, but whatever.
	  (when (possible-file-name-p (first lish:*input*))
	    (setf ,place (append lish:*input* ,place))))
	 (t
	  ;; What to do for *input* that we can't determine is a file name.
	  ;; By default, just ignore it.
	  ,on-unknown-input-type))
       ,@body)))

;; (defun spread (command &rest commands)
;;   "Send output from a command to multiple commands in parallel."
;;   )

;; (defun gather (command &rest streams)
;;   "Gather multiple streams and send them in order to a command."
;;   )

;; (defvar *files-to-delete* '()
;;   "A list of files to delete at the end of a command.")
;;
;; ;; This has a lot of potential problems / security issues.
;; (defun != (&rest commands)
;;   "Temporary file name output substitution."
;;   (multiple-value-bind (vals stream)
;;       (shell-eval (possibly-read commands)
;;                   :context (modified-context *context* :out-pipe t))
;;     (if (and vals (> (length vals) 0))
;; 	(let ((fn (nos:mktemp "lish")))
;; 	  (push fn *files-to-delete*)
;; 	  (with-posix-file (fd fn (logior O_WRONLY O_CREAT O_EXCL) #o600)
;; 	    (let ((buf (make-string (buffer-size))))
;; 	      (loop :while (read-sequence buf stream)
;; 	(progn
;; 	  (close stream)
;; 	  nil))))

(defun run-with-env (env-alist &rest args)
  "Run a shell command with environement variables as specified in ‘env-alist’."
  (with-shell ()
    (shell-eval (possibly-read args)
		:context (modified-context *context* :environment env-alist))))

;; Most of the ! functions are wrapped in with-shell so they should be able to
;; be used outside of an interactive shell.

(defun ! (&rest args)
  "Evaluate the shell command."
  (with-shell ()
    (shell-eval (shell-read (lisp-args-to-command args)))))

(defun unix-truthy (value)
  "If the result is number, 0 is T and anything else is NIL.
If the result isn't a number, only NIL is false, as usual."
  (cond
    ((numberp value)
     (zerop value))
    (t (and value t))))

(defun !? (&rest args)
  "Evaluate the shell command, converting a Unix shell result code into boolean.
This means that if the result is number, 0 is T and anything else is NIL.
If the result isn't a number, only NIL is false, as usual."
  (with-shell ()
    (let ((result (shell-eval (shell-read (lisp-args-to-command args)))))
      (cond
	((numberp result)
	 (zerop result))
	(t (and result t))))))

(defun !$ (&rest command)
  "Return lines output from command as a string of words. This is basically
like $(command) in bash."
  (with-shell ()
    (command-output-words (lisp-args-to-command command))))

(defun !$$ (&rest command)
  "Return lines of output from command as a string of quoted words."
  (with-shell ()
    (command-output-words (lisp-args-to-command command) t)))

(defun !@ (&rest command)
  "Return the output from command, broken into words by the shell reader."
  (with-shell ()
    (shell-words-to-list (lish::shell-expr-words (shell-read (!- command))))))

;; @@@ What about something like:
;; (defmacro !q (&rest args)
;;   `(!@ ,@(loop :for a :in args :collect
;; 	    (typecase a
;; 	      (symbol (string-downcase a))
;; 	      (string (s+ #\" a #\"))
;; 	      (t a)))))

;; ;; @@@ or even
;; (defmacro !qq (&rest args)
;;   `(!@ ,@(loop :for a :in args :collect
;; 	    (typecase a
;; 	      (symbol (if (boundp a)
;; 			  (symbol-value a)
;; 			  (string-downcase a)))
;; 	      (string (s+ #\" a #\"))
;; 	      (t a)))))

;; @@@ How about this?
;; Consider how something like this could be integrated into all the other
;; normal macros? This doesn't have all the return value options.
(defmacro !q (&rest args)
  "Simpler shell quotation. Make a shell command out of the separate words of
the downcased symbols. If a symbol starts with a ‘@’ character, than it will
probably be evaluated. Lists or anything else, get passed as is.
For example, you can say:

   (loop for f in (glob \"*.cl\")
     do (!q mv -i @f (s+ (remove-suffix f \".cl\") \".lisp\")))

instead of having to deal with string quoting, like:

   (loop for f in (glob \"*.cl\")
     do (! \"mv -i \\\"\" f \"\\\" \" (s+ (remove-suffix f \".cl\") \".lisp\")))
"
  `(!= ,@(loop :for a :in args
	    :collect (typecase a
		       (symbol (if (and (> (length (symbol-name a)) 1)
					(char= #\@ (char (symbol-name a) 0)))
				   (symbolify (subseq (symbol-name a) 1))
				   (string-downcase a)))
		       (string (s+ #\" a #\"))
		       (t a)))))

(defun !_ (&rest command)
  "Return a list of the lines of output from the command."
  (with-shell ()
    (command-output-list (lisp-args-to-command command))))

(defun !- (&rest command)
  "Return a string containing the output from the command."
  (with-shell ()
    (with-output-to-string (str)
      (run-with-output-to str (lisp-args-to-command command)))))

(defun !and (&rest commands)
  "Run commands until one fails. Return the last result."
  (let (result)
    (loop :for c :in commands
	  :while (unix-truthy (setf result (shell-eval (shell-read c)))))
    result))

(defun !or (&rest commands)
  "Run commands until one succeeds. Return the last result."
  (let (result)
    (loop :for c :in commands
	  :until (unix-truthy (setf result (shell-eval (shell-read c)))))
    result))

;; (defun !bg (&rest commands)
;;   "Run commands in the background."
;;   (declare (ignore commands))
;;   )

(defun !! (&rest commands)
  "Pipe output of commands. Return a stream of the output."
  (with-shell ()
    (multiple-value-bind (vals stream)
	(shell-eval (shell-read (lisp-args-to-command commands))
		    :context (modified-context *context* :out-pipe t))
      (if (and vals (> (length vals) 0))
	  stream
	  (progn
	    (close stream)
	    nil)))))

(defun !> (file-or-stream &rest commands)
  "Run commands with output to a file or stream."
  (with-shell ()
    (run-with-output-to file-or-stream (lisp-args-to-command commands))))

(defun !>> (file-or-stream &rest commands)
  "Run commands with output appending to a file or stream."
  (declare (ignore file-or-stream commands))
  )

(defun !<> (file-or-stream &rest commands)
  "Run commands with input and output to a file or stream."
  (declare (ignore file-or-stream commands))
  )

(defun !>! (file-or-stream &rest commands)
  "Run commands with output to a file or stream, superseding it."
  (run-with-output-to file-or-stream commands :supersede t))

(defun !>>! (file-or-stream &rest commands)
  "Run commands with output appending to a file or stream, overwritting it."
  (declare (ignore file-or-stream commands))
  )

(defun !< (file-or-stream &rest commands)
  "Run commands with input from a file or stream."
  (with-shell ()
    (run-with-input-from file-or-stream (lisp-args-to-command commands))))

(defun !!< (file-or-stream &rest commands)
  "Run commands with input from a file or stream and return a stream of output."
  (with-shell ()
    (with-open-file-or-stream (in-stream file-or-stream)
      (multiple-value-bind (vals stream)
	  (shell-eval (shell-read (lisp-args-to-command commands))
		      :context
		      (modified-context *context*
					:out-pipe t
					:in-pipe in-stream))
	(if (and vals (> (length vals) 0))
	    stream
	    (progn
	      (close stream)
	      nil))))))

(defun !h (n)
  "Return the N-th history item as a string. If N is positive it counts from
the beginning of history. If N is negative it counts backward from the most
recent."
  (rl:history-entry-line (rl:history-nth n)))

(defun !hh (n)
  "Return the N-th history item, as a history entry object. If N is positive it
counts from the beginning of history. If N is negative it counts backward from
the most recent."
  (rl:history-nth n))

(defun !v (n)
  "Return the N-th history result values list. If N is positive it counts from
the beginning of history. If N is negative it counts backward from the most
recent."
  (getf (rl:history-entry-extra (rl:history-nth n)) :values))

(defsetf !v (n) (new-values)
  `(setf (getf (rl:history-entry-extra (rl:history-nth ,n)) :values)
	 ,new-values))

;; @@@ What about !h as a complex executed command object?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; literal arg comands (= suffix)

(defun != (&rest args)
  "Run a command with the separate verbatim arguments, without shell syntax."
  (with-shell ()
    (shell-eval (expr-from-args args))))

(defun !?= (&rest args)
  "Evaluate the shell command, converting the system result code into boolean.
This means the 0 is T and anything else is NIL."
  (with-shell ()
    (let ((result
	   (shell-eval (expr-from-args args))))
      (and (numberp result) (zerop result)))))

(defun !$= (&rest command)
  "Return lines output from command as a string of words. This is basically
like $(command) in bash."
  (with-shell ()
    (command-output-words (expr-from-args command))))

(defun !$$= (&rest command)
  "Return lines of output from command as a string of quoted words."
  (with-shell ()
    (command-output-words (expr-from-args command) t)))

(defun !@= (&rest command)
  "Return the output from command, broken into words by the shell reader."
  (with-shell ()
    (shell-words-to-list
     (lish::shell-expr-words
      (shell-read
       (with-output-to-string (str)
	 (run-with-output-to str (expr-from-args command))))))))

(defun !_= (&rest args)
  "Return a list of the lines of output from the command."
  (with-shell ()
    (command-output-list (expr-from-args args))))

(defun !-= (&rest command)
  "Return a string containing the output from the command."
  (with-shell ()
    (with-output-to-string (str)
      (run-with-output-to str (expr-from-args command)))))

(defun !!= (&rest commands)
  "Pipe output of commands. Return a stream of the output."
  (with-shell ()
    (multiple-value-bind (vals stream)
	(shell-eval (expr-from-args commands)
		    :context (modified-context *context* :out-pipe t))
      (if (and vals (> (length vals) 0))
	  stream
	  (progn
	    (close stream)
	    nil)))))

;; EOF
