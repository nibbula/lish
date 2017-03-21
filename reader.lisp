;;
;; reader.lisp - Lish reader
;;

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

(defparameter *reader-quote-char*
  ;; I don't think #\\ should be in here.
  #(#\" #\( #\) #\! #\space #\; #\| #\< #\> #\& #\^) 
  "Characters which the reader interprets specially if not quoted.")

(defun shell-read (line &key partial (package *lish-user-package*))
  "Read objects in shell syntax and return them. If PARTIAL is true, don't 
signal an error if we can't read a full expression. It returns a SHELL-EXPR or
a normal lisp object.
The syntax is vaguely like:
  ; comment
  command [arg...]
  command \"string\" !*lisp-object* !(lisp-code)
  command word\ with\ spaces \"string \\\" with a double quote\"
  command | command | ...
  command < file-name
  command > file-name
  ([lisp expressions...])"
;  (setf line (expand-global-aliases line))
  (let (word-start word-end word-quoted word-eval words
	(c nil)				; current char
	(i 0)				; index in line
	(len (length line))
	(args '())
	(sub-expr '())
	(w (make-stretchy-string 12))	; temp word
	(in-word nil)			; t if in word
	(in-first-word t)		; t if in the first word on the line
	(do-quote nil)
	(in-compound nil)
	(did-quote nil))		;
    (labels ((finish-word ()
	       "Finish the current word."
	       (dbugf 'reader "finish-word ~s~%" w)
	       (when in-word
		 (if sub-expr
		     (progn
		       (push (copy-seq w) sub-expr)
		       (setf sub-expr (nreverse sub-expr))
		       (push t word-eval)
		       (push sub-expr args))
		     (progn
		       (push (copy-seq w) args)
		       (push nil word-eval)))
		 (push i word-end)
		 (push did-quote word-quoted)
		 (setf (fill-pointer w) 0
		       in-word nil
		       in-first-word nil
		       did-quote nil)))
	     (ignore-word ()
	       "Ignore the current word."
	       (when in-word
		 (setf (fill-pointer w) 0
		       in-word nil
		       in-first-word nil
		       did-quote nil)))
	     (add-to-word ()
	       "Add the character to the current word or start a new one."
	       (when (not in-word)
		 (push i word-start))
	       (setf in-word t)
	       (vector-push-extend c w)
	       (incf i))
	     (read-lisp-expr ()
	       (handler-bind
		   ((end-of-file (_  (declare (ignore _)) (do-continue)))
		    (reader-error (_ (do-reader-error _))))
		 ;; read a form as a separate word
		 (multiple-value-bind (obj pos)
		     (with-package package
		       (if partial
			   (clean-read-from-string line *junk-package* nil
						   *continue-symbol* :start i)
			   (read-from-string line nil
					     *continue-symbol* :start i)))
		   (push i word-start)
		   (setf i pos)
		   (push obj args)
		   (push i word-end)
		   (push nil word-quoted)
		   (push nil word-eval))))
	     (return-partial ()
	       (push i word-start)
	       (push (subseq line i) args)
	       (push (length line) word-end)
	       (push nil word-quoted)
	       (push nil word-eval)
	       (return-from shell-read
		 (make-shell-expr
		  :line line
		  :words (nreverse args)
		  :word-start (reverse word-start)
		  :word-end (nreverse word-end) 
		  :word-quoted (nreverse word-quoted)
		  :word-eval (nreverse word-eval)
		  )))
	     (do-continue ()
	       "Handle when the expression is incomplete."
	       (if partial
		   (return-partial)
		   (return-from shell-read *continue-symbol*)))
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
	       (setf word-start  (reverse word-start)
		     word-end    (nreverse word-end)
		     word-quoted (nreverse word-quoted)
		     word-eval   (nreverse word-eval)
		     words       (nreverse args)))
	     (make-the-expr ()
	       "Make an expression, with it's own copy of the lists."
	       (dbugf 'reader "make-the-expr ~s~%" words)
	       (setf in-compound nil)
	       (make-shell-expr
		:line line
		:words (copy-seq words)
		:word-start (copy-seq word-start)
		:word-end (copy-seq word-end)
		:word-quoted (copy-seq word-quoted)
		:word-eval (copy-seq word-eval)))
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
	       (ignore-word)
	       (reverse-things)
	       (let ((e (list key (make-the-expr))))
	       	 (setf args (list e)))
	       (incf i inc)
	       (setf word-start	 (list 0)
		     word-end	 (list 0)
	       	     word-quoted (list nil)
	       	     word-eval	 (list nil)
	       	     in-compound t)))
      (loop
	 :named tralfaz
	 :while (< i len)
	 :do
	 (setf c (aref line i))
	 (cond
	   ;; quoted char
	   (do-quote
	     ;; @@@ Actually I think we should leave some quote chars in until
	     ;; after expansion. That way we can expand part of word, while
	     ;; having some chars protected from expansion, e.g. glob chars
	     ;; can be quoted.
	     (when (not (position c *reader-quote-char*))
	       (vector-push-extend #\\ w))
	     (vector-push-extend c w)
	     (when (not in-word)
	       (push (1- i) word-start))
	     (setf in-word t)
	     (setf do-quote nil)
	     (incf i))
	   ;; a string
	   ((eql c #\")
	    (finish-word)
	    (dbugf 'reader "read-string~%")
	    ;; read a string as a separate word
	    (multiple-value-bind (str ink cont)
		(read-string (subseq line (1+ i)))
	      (when (and cont (not partial))
		(return-from shell-read *continue-symbol*))
	      (push i word-start)
	      (push str args)
	      (incf i (+ 2 ink))
	      (push i word-end)
	      (push t word-quoted)
	      (push nil word-eval)))
	   ;; a lisp function application
	   ((eql c #\()
	    (finish-word)
	    (read-lisp-expr))
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
	   ;; a lisp expr
	   ((eql c #\!)
	    (dbugf 'reader "sub-expr")
	    (when (not in-word)
	      (push i word-start))
	    (when (not sub-expr)
	      (push 's+ sub-expr))	; !!!
	    (when (length w)
	      (dbugf 'reader " ~s" w)
	      (push (copy-seq w) sub-expr)
	      (setf (fill-pointer w) 0))
	    ;; read a form as a separate word
	    (handler-bind
		((end-of-file (_  (declare (ignore _)) (do-continue)))
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
		(push obj sub-expr))))
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
	    (let ((e (list
		      (if (eql c #\>)
			  (if (eql (next-char) #\>)
			      (progn (incf i) :append-to)
			      :redirect-to)
			  :redirect-from)
		      (make-the-expr))))
	      (setf args (list e)))
	    (setf word-start (list i))
	    (incf i)
	    (setf word-end (list i)
		  word-quoted (list nil)
		  word-eval (list t)))
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
	   (finish-word)
	   (reverse-things)))
      (if (and (= (length words) 1) (consp (first words))
	       (not in-compound))
	  ;; just a lisp expression to be evaluated
	  (first words)
	  ;; a normal shell expression
	  (make-the-expr)))))

;; EOF
