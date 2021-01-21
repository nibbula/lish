;;;
;;; reader.lisp - Lish reader
;;;

(in-package :lish)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

(defun read-string (s)
  "Read a lish string. It has similar syntax to a lisp string. ~
Assumes the opening double quote has already been read. ~
Read until a double quote. Backslash escapes the special meaning of ~
the following character. Return the string and how long it is. If we got to ~
the end and didn't get a close quote, the third value is true.~
"
  (let ((v (make-stretchy-string 10))
	(i 0)
	(end-quote nil)
	(quote-next nil))
    (loop :for c :across s :do
       (setf end-quote (and (eql c #\") (not quote-next)))
       :while (not end-quote)
       :do
       (if (and (eql c #\\) (not quote-next))
	   (setf quote-next t)
	   (progn
	     (setf quote-next nil)
	     (vector-push-extend c v)))
       (incf i))
    (values v i (not end-quote))))

;; I'm not so old fashioned that I think ^L should be in here, but are there
;; any other unicode things that should?
(defparameter *whitespace* #(#\space #\newline #\tab #\return)
  "Word separators for lish.")

(defun contains-whitespace-p (s)
  (position-if #'(lambda (x) (position x *whitespace*)) s))

(defparameter *reader-quote-char*
  ;; I don't think #\\ should be in here.
  #(#\" #\( #\) #\! #\space #\; #\| #\< #\> #\& #\^) 
  "Characters which the reader interprets specially if not quoted.")

(defun shell-read (line &key partial (package *lish-user-package*))
  "Read objects in shell syntax and return them. It returns a SHELL-EXPR or
a normal lisp object.
The syntax is vaguely like:
  ; comment
  command [arg...]
  command \"string\" !*lisp-object* !(lisp-code)
  command word\ with\ spaces \"string \\\" with a double quote\"
  command | command | ...
  command < file-name
  command > file-name
  ([lisp expressions...])

If PARTIAL is true, don't signal an error if we can't read a full expression.
Instead we return *CONTINUE-SYMBOL* as the first value, and as the second
value, an explaination which consists of (tag-symbol datum...)."
;  (setf line (expand-global-aliases line))
  (let (words word-start
	(c nil)				; current char
	(i 0)				; index in line
	(len (length line))
	(args '())
	(sub-expr '())
	(w (make-stretchy-string 12))	; temp word
	(in-word nil)			; t if in word
	(in-first-word t)		; t if in the first word on the line
	(string-quote nil)
	(lisp-quote nil)
	(in-compound nil)
	(did-quote nil)
	(brace-depth 0))		;
    (labels ((reset-word ()
	       "Reset the word to be empty."
	       (setf (fill-pointer w) 0
		     in-word nil
		     in-first-word nil
		     did-quote nil))
	     (finish-word ()
	       "Finish the current word."
	       (dbugf 'reader "finish-word ~s ~s~%" w in-word)
	       (when in-word
		 (if (and (not sub-expr) (>= (length w) 2)
			  (char= (aref w 0) #\#) (char= (aref w 1) #\\))
		     (progn
		       ;; As a special hack for reading lisp characters for
		       ;; parenless lisp evaluation, convert words starting with
		       ;; #\ to the actual character.
		       (push (make-shell-word
			      :word (read-from-string
				     (copy-seq w) nil *continue-symbol*)
			      :eval nil
			      :start word-start
			      :end i
			      :quoted nil)
			     args))
		     (push (make-shell-word
			    :word (if sub-expr sub-expr (copy-seq w))
			    :eval (and sub-expr t)
			    :start word-start
			    :end i
			    :quoted did-quote)
			   args)))
	       (reset-word))
	     ;; (ignore-word ()
	     ;;   "Ignore the current word."
	     ;;   (when in-word
	     ;; 	 (reset-word)))
	     (add-to-word ()
	       "Add the character to the current word or start a new one."
	       (when (not in-word)
		 (setf word-start i))
	       (setf in-word t)
	       (vector-push-extend c w)
	       (incf i))
	     (read-lisp-expr ()
	       (when lisp-quote
		 (decf i))
	       (handler-bind
		   ((end-of-file
		     (_ (declare (ignore _))
			(do-continue 'lisp-expr (copy-seq w))))
		    ;; The spec is a bit vauge about what type of error should
		    ;; be signaled if a symbol with a package marker doesn't
		    ;; exist. In section 2.3.5 it says it should be correctable
		    ;; and the effect should be the same as reading the symbol
		    ;; with *package* set to the package prefix, but there's no
		    ;; symbol existence error that can happen when it's not
		    ;; prefixed.
		    ;;
		    ;; For example SBCL signals a reader-error, while CCL
		    ;; signals a simple-error. I guess we should catch either?
		    ;; This really only matters when partial is set.
		    (reader-error (_ (do-reader-error _)))
		    (simple-error (_ (do-reader-error _))))
		 ;; read a form as a separate word
		 (multiple-value-bind (obj pos)
		     (with-package package
		       (if partial
			   (clean-read-from-string line *junk-package* nil
						   *continue-symbol* :start i)
			   (read-from-string line nil
					     *continue-symbol* :start i)))
		   (setf word-start i
		        i pos)
		   (push (make-shell-word :word obj
					  :eval t
					  :quoted lisp-quote
					  :start word-start
					  :end i)
			 args)
		   )))
	     (return-partial ()
	       (setf word-start i)
	       (push (make-shell-word
		      :word (subseq line i)
		      :start word-start
		      :end (length line)
		      :quoted nil
		      :eval nil)
		     args)
	       (return-from shell-read
		 (make-shell-expr :line line :words (nreverse args))))
	     (do-continue (reason &optional data)
	       "Handle when the expression is incomplete."
	       (if partial
		   (return-partial)
		   (return-from shell-read
		     (values *continue-symbol* `(,reason ,@data)))))
	     (do-reader-error (c)
	       "Handle when the expression has an error."
	       ;; (format t "lish-read error ~a~%" c)
	       (if partial
		   (return-partial)
		   (signal c)))
	     (next-char ()
	       "Return the next character or NIL."
	       (when (< (+ i 1) len) (aref line (1+ i))))
	     (reverse-things ()
	       "Reverse the things we've been consing, so they're in order."
	       (setf words (nreverse args)))
	     (make-the-expr ()
	       "Make an expression, with it's own copy of the lists."
	       (dbugf 'reader "make-the-expr ~s~%" words)
	       (setf in-compound nil)
	       (make-shell-expr
		:line line
		:words (copy-seq words)))
	     (make-compound (key &optional (inc 2))
	       "Make a compound expression with type KEY."
	       ;; (finish-word)
	       ;; (reverse-things)
	       ;; (let ((e (list key (make-the-expr))))
	       ;; 	 (setf args (list e)))
	       ;; (setf word-start (list i))
	       ;; (incf i inc)
	       ;; (setf word-end (list i)
	       ;; 	     word-quoted (list nil)
	       ;; 	     word-eval (list nil)
	       ;; 	     in-compound t)))
	       (dbugf 'reader "make-compound ~s~%" key)
	       ;; (ignore-word)
	       (finish-word)
	       (reverse-things)
	       (let ((e (list key (make-the-expr))))
	       	 (setf args (list e)))
	       (incf i inc)
	       (setf word-start	 0
		     ;; word-end	 (list 0)
	       	     ;; word-quoted (list nil)
	       	     ;; word-eval	 (list nil)
		     in-compound key)))
      (loop
	 :named tralfaz
	 :while (< i len)
	 :do
	 (setf c (aref line i))
	 (cond
	   ;; quoted char
	   (string-quote
	     ;; @@@ Actually I think we should leave some quote chars in until
	     ;; after expansion. That way we can expand part of word, while
	     ;; having some chars protected from expansion, e.g. glob chars
	     ;; can be quoted.
	     (when (not (position c *reader-quote-char*))
	       (vector-push-extend #\\ w))
	     (vector-push-extend c w)
	     (when (not in-word)
	       (setf word-start (1- i)))
	     (setf in-word t)
	     (setf string-quote nil)
	     (incf i))
	   ;; a string
	   ((eql c #\")
	    (finish-word)
	    (dbugf 'reader "read-string~%")
	    ;; read a string as a separate word
	    (multiple-value-bind (str ink cont)
		(read-string (subseq line (1+ i)))
	      (when (and cont (not partial))
		(return-from shell-read
		  (values *continue-symbol* `(string ,str ,i))))
	      (setf word-start i)
	      (incf i (+ (if cont 1 2) ink))
	      (push (make-shell-word :word str
				     :start word-start
				     :end i
				     :quoted t
				     :eval nil)
		    args)))
	   ;; a lisp function application
	   ((eql c #\()
	    (finish-word)
	    (read-lisp-expr)
	    (setf lisp-quote nil))
	   ((and (or (eql c #\') (eql c #\`)) (eql (next-char) #\())
	    (finish-word)
	    (setf lisp-quote t)
	    (incf i))
	   ((eql c #\#)
	    ;; This is so we can use the Lisp reader interpretation of # at
	    ;; the beginning of a shell line, but otherwise, in the rest of
	    ;; the line ‘#’ is treated as a normal character.
	    ;; This means we can do #+foo etc. before an expressin in scripts
	    ;; and but still have command arguments, like filenames, that
	    ;; begin with ‘#’. Of course ‘#’ in Lisp sub-expressions still
	    ;; should work.
	    (cond
	     (in-word
	      ;; # doesn't break words, like (
	      (add-to-word))
	     (in-first-word
	      (read-lisp-expr))
	     (t
	      (add-to-word))))
	   ;; Hack to not expand , in braces
	   ((eql c #\{)
	    (incf brace-depth)
	    (add-to-word))
	   ((eql c #\})
	    (when (plusp brace-depth)
	      (decf brace-depth))
	    (add-to-word))
	   ;; a lisp expr
	   ;; ((eql c #\!)
	   ((or (eql c #\!) (and (eql c #\,) (zerop brace-depth)))
	    (dbugf 'reader "sub-expr ")
	    (setf sub-expr nil)
	    (finish-word)
	    (when (not in-word)
	      (setf word-start i))
	    ;; read a form as a separate word
	    (handler-bind
		((end-of-file
		  (_ (declare (ignore _))
		     (do-continue 'bang-expr (copy-seq w))))
		 (reader-error (_ (do-reader-error _))))
	      (multiple-value-bind (obj pos)
		  (with-package package
		    (if partial
			(clean-read-from-string line *junk-package* nil
						*continue-symbol*
						:start (+ i 1))
			(read-from-string line nil *continue-symbol*
					  :start (+ i 1))))
		(setf i pos)
		(setf in-word t) ; so it gets output
		(dbugf 'reader " ~s~%" obj)
		(if (and obj (integerp obj)
			 *shell* (get-option *shell* 'history-expansion))
		    (map-into w #'identity (subseq line word-start i))
		    (setf sub-expr obj))
		(finish-word)
		(setf sub-expr nil)
		)))
	   ;; quote char
	   ((eql c #\\)
	    (setf string-quote t)
	    (incf i))
	   ;; whitespace
	   ((position c *whitespace*)
	    (finish-word)
	    (incf i))
	   ;; comment
	   ((eql c #\;)
	    (finish-word)
	    ;; (loop :for j :from i :below len
	    ;;    :while (not (eql (aref line j) #\newline))
	    ;;    :do (incf i))
	    (setf i (or (position #\newline line :start i) len)))
	   ;; pipe plus
	   ((and (eql c #\|) (eql (next-char) #\+))
	    (make-compound :pipe-plus))
	   ;; pipe
	   ((and (eql c #\|) (not (eql (next-char) #\|)))
	    (make-compound :pipe 1))
	   ;; redirect
	   ((or (eql c #\<) (eql c #\>))
	    (finish-word)
	    (reverse-things)
	    ;; @@@ need to get the file name as a word
	    (let ((e (list
		      (if (eql c #\>)
			  (if (eql (next-char) #\>)
			      (progn (incf i) :append-to)
			      :redirect-to)
			  :redirect-from)
		      (make-the-expr))))
	      (setf word-start i)
	      (setf args (list (make-shell-word :start word-start
						;; :word (list e) ;; ???
						:word e
						:end i
						:quoted nil
						:eval t))))
	    (dbugf 'reader "args 1 = ~s~%" args)
	    (incf i))
	   ;; and
	   ((and (eql c #\&) (eql (next-char) #\&))
	    (make-compound :and))
	   ;; or
	   ((and (eql c #\|) (eql (next-char) #\|))
	    (make-compound :or))
	   ;; sequence
	   ((eql c #\^)
	    (make-compound :sequence 1))
	   ;; any other character: add to word
	   (t
	    (add-to-word)))
	 :finally
	 (progn
	   (dbugf 'reader "Finally!~%")
	   (dbugf 'reader "args 2 = ~s~%" args)
	   (finish-word)
	   (dbugf 'reader "args 3 = ~s~%" args)
	   (reverse-things)
	   ))
      (if (and (= (length words) 1) (consp (first words))
	       (not in-compound))
	  ;; just a lisp expression to be evaluated
	  (first words)
	  ;; a normal shell expression
	  (if (and in-compound (< (length words) 2))
	      (return-from shell-read
		(values *continue-symbol* `(compound ,in-compound)))
	      (make-the-expr))))))

;; EOF
