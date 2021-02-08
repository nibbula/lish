;;;
;;; horrible.lisp - Things are in here so you don't have look at them.
;;;

(defun msg (fmt &rest args)
  (when *build-verbose*
    (format t "~&~?~%" fmt args)
    (finish-output)))

(defun sf-getenv (s)
  #+clisp (ext:getenv s)
  #+sbcl (sb-ext:posix-getenv s)
  #+openmcl (ccl::getenv s)
  #+cmu (let ((v (assoc (intern (string-upcase s) :keyword)
			ext:*environment-list*)))
	  (if v (cdr v)))
  #+ecl (si::getenv s)
  #+excl (sys::getenv s)
  #+lispworks (hcl:getenv s)
  #+gcl (system:getenv s)
  #+abcl (ext:getenv s)
  #+clasp (ext:getenv s)
  #+cormanlisp
  ;; @@@ maybe we could get this added to cormanlisp?
  (let (name null-pointer value size result blurp)
    (unwind-protect
	 (progn
	   (setf name (ct:lisp-string-to-c-string s)
		 null-pointer (ct:malloc 1)
		 size (win:getenvironmentvariable name null-pointer 0))
	   (if (and (zerop size) (= (win:getlasterror) 203))
	       (setf result nil)
	       (progn
		 (setf value (ct:malloc (1+ size))
		       blurp (win:getenvironmentvariable name value size))
		 (when (/= (1+ blurp) size)
		   (error "corman bootstrap getenv failed?."))
		 (setf result (ct:c-string-to-lisp-string value)))))
      (when null-pointer
	(ct:free null-pointer))
      (when value
	(ct:free value)))
    result)
  #-(or clisp sbcl openmcl cmu ecl excl lispworks gcl abcl clasp cormanlisp)
  (error "PORTABILITY BUG: I don't know how to getenv."))

(defun exit-lisp (&key code abort timeout)
  "Halt the entire Lisp system." ;; But not necessarily the operating system.
  (declare (ignorable code timeout abort))
  (when (not code)
    (setf code (if abort 1 0)))
  #+openmcl (ccl::quit code)
  #+ccl (ccl::quit code)
  #+cmu (ext:quit)
  ;; #+sbcl (sb-ext:quit)
  #+sbcl (sb-ext:exit :code code :abort abort :timeout timeout)
  #+excl (excl:exit code)
  #+lispworks (lispworks:quit :status code)
  #+clisp (funcall 'ext:quit)
  #+ecl (ext:quit code)
  #+clasp (ext:quit code)
  #+abcl (ext:quit :status code)
  #+cormanlisp (win32:exitprocess code)
  #-(or openmcl ccl cmu sbcl excl lispworks clisp ecl clasp abcl)
  (error "PORTABILITY BUG: I don't know how to even EXIT-LISP!"))

;; Workaround for some implementations default encoding
(defmacro with-unicode-files (() &body body)
  `(let (#+lispworks
	 (system:*file-encoding-detection-algorithm* '(utf-8-file-encoding)))
     ,@body))

(defun lisp-args (#| &key all-p |#)
  "Arguments given when starting the Lisp system."
  #+sbcl     sb-ext:*posix-argv*
  #+clisp    (ext:argv)
  #+cmu	     ext:*command-line-strings*
  #+openmcl  (ccl::command-line-arguments)
  #+excl     (sys:command-line-arguments) 
  #+ecl	     (ext:command-args)
  #-(or sbcl clisp cmu openmcl excl ecl)
  (error "PORTABILITY BUG: I don't know how to get the program arguments."))

;; Minimal crap taken from dlib.

(defmacro _ (&rest exprs)
  "Shorthand for single argument lambda. The single argument is named '_'."
  `(lambda (_)
     (declare (ignorable _))
     ,@exprs))

(defun begins-with (prefix thing &key (test #'eql))
  "True if THAT begins with THIS."
  (let ((pos (search prefix thing :test test)))
    (and pos (zerop pos))))

(defun remove-prefix (sequence prefix &key (test #'eql))
  "Remove PREFIX from SEQUENCE. SEQUENCE and PREFIX are both sequences.
If SEQUENCE is is not prefixed by PREFIX, just return SEQUENCE."
  (if (begins-with prefix sequence :test test)
      (subseq sequence (length prefix))
      (copy-seq sequence)))

(defun remove-suffix (sequence suffix &key (test #'eql))
  "Remove SUFFIX from the end of SEQUENCE. If SEQUENCE doesn't end in SUFFIX,
just return SEQUENCE. Elements are compared with TEST which defaults to EQL."
  (let ((pos (search suffix sequence :from-end t :test test)))
    (if (and pos (= pos (- (length sequence) (length suffix))))
	(subseq sequence 0 pos)
	(copy-seq sequence))))

;; EOF
