;;
;; prompt.lisp - Things to make the prompt
;;

(in-package :lish)

(defvar *lish-level* nil
  "Number indicating the depth of lish recursion. Corresponds to the ~
LISH_LEVEL environment variable.")

(defun fixed-homedir ()
  "(user-homedir-pathname) with a trailing slash removed if there was one."
  (let ((h (safe-namestring (truename (user-homedir-pathname)))))
    (if (equal #\/ (aref h (1- (length h))))
	(subseq h 0 (1- (length h)))
	h)))

(defun twiddlify (name)
  "Turn (user-homedir-pathname) occuring in name into a tilde."
  (replace-subseq (safe-namestring (fixed-homedir)) "~"
		  (safe-namestring name) :count 1))

;; This is mostly for bash compatibility.

(defun format-prompt (sh prompt &optional (escape-char #\%))
  "Return the prompt string with bash-like formatting character replacements.
So far we support:
%%      A percent.
%a      #\\bell
%e      #\\escape
%n      #\\newline
%r      #\\return
%NNN    The character whose ASCII code is the octal value NNN.
%s      The name of the shell, which is usually “Lish”.
%v      Shell version.
%V      Even more shell version.
%u      User name.
%h      Host name truncated at the first dot.
%H      Host name.
%w      Working directory, tildified.
%W      The basename of `$PWD', tildified.
%$      If the effective UID is 0, `#', otherwise `$'.
%i      The lisp implementation nickname.
%p      The shortest nickname of *lish-user-package*.
%P      The current value of *lish-user-package*.
%d      <3 char weekday> <3 char month name> <date>.
%t      24 hour HH:MM:SS
%T      12 hour HH:MM:SS
%@      The time, in 12-hour am/pm format.
%A      The time, in 24-hour HH:MM format.
Not implemented yet:
%!      The history number of this command.
%#      The command number of this command.
%[      Start of non-printing characters.
%]      End of non-printing characters.
%l      The basename of the shell's terminal device name.
%D{FORMAT}
        Some date formated by Unix strftime. Without the FORMAT just put some
        locale-specific date.
%j      The number of jobs currently managed by the shell.
"
  (declare (ignore sh))
  ;;(let ((out (make-stretchy-string 80)))
  (let ((out (make-stretchy-string (length prompt))))
    (with-output-to-string (str out)
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
				 (parse-integer
				  (subseq prompt i (+ i 3)) :radix 8))
				str))
		   (#\s (write-string (princ-to-string *shell-name*) str))
		   (#\v (princ *major-version* str))
		   (#\V (write-string (princ-to-string *version*) str))
		   (#\u (write-string (nos:user-name) str))
		   (#\h (write-string
			 (initial-span (os-machine-instance) ".") str))
		   (#\H (write-string (machine-instance) str))
		   (#\w (write-string (twiddlify (nos:current-directory)) str))
		   (#\W (write-string
			 (twiddlify (basename (nos:current-directory))) str))
		   (#\$ (write-char
			 (if (= (nos:user-id :effective t) 0) #\# #\$) str))
		   (#\i (write-string (princ-to-string
				       *lisp-implementation-nickname*) str))
		   (#\p (write-string
			 (s+ (and *lish-user-package*
				  (shortest-package-nick *lish-user-package*)))
			 str))
		   (#\P (write-string
			 (s+ (and *lish-user-package*
				  (package-name *lish-user-package*))) str))
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
	     (write-char c str))))
    out))

;; @@@ Docstring is WRONG!
(defun symbolic-prompt-to-string (sh symbolic-prompt #| &optional ts-in |#)
  "Take a symbolic prompt and turn it into a string. A symbolic prompt can be
any printable lisp object, which is converted to a string. If it is a list, it
translates sublists starting with certain keywords, to terminal codes to do
text effects to the enclosed objects. The keywords recognized are:
  :BOLD :UNDERLINE :INVERSE
and the colors
  :BLACK :RED :GREEN :YELLOW :BLUE :CYAN :WHITE and :DEFAULT.
The colors can be prefixed by :FG- or :BG- for the foreground or background.
Symbols will be replaced by their value. Functions will be evaluated with
the primary result printed as a string."
  (fatchar:span-to-fat-string
   symbolic-prompt
   :filter (_ (format-prompt sh _))
   :unknown-func
   (lambda (x) ; eval is magic
     (cond
       ((and (listp x) (symbolp (car x)) (fboundp (car x)))
	(apply (car x) (cdr x)))
       ((symbolp x)
	(when (boundp x)
	  (symbol-value x)))
       (t (princ-to-string x))))))

#|
(defun fill-prompt ()
  (#\f (let ((len (length out)) out-char cols)
	 (when (> len 1)
	   (setf out-char (aref out (1- len))
		 cols (terminal-window-columns
		       (rl:line-editor-terminal
			(lish-editor sh))))
	   (loop :repeat (- cols len)
	      :do (write-char out-char str))))))

1. Expand the %<things> in the symbolic version.
2. Convert to fatchar and expand the %fill which can now know the true size
3. Convert from fatchar to final device form

|#

(defvar *fallback-prompt* "Lish> "
  "Prompt to use as a last resort.")

(defgeneric make-prompt (shell)
  (:documentation "Return a string to prompt with."))
(defmethod make-prompt ((sh shell))
  "Return a string to prompt with."
  (or (and (lish-prompt sh)
	   ;; (format-prompt
	   ;;  sh (symbolic-prompt-to-string (lish-prompt sh))))
	   (symbolic-prompt-to-string sh (lish-prompt sh)))
      ;; (if (and (lish-prompt-char sh)
      ;; 	       (characterp (lish-prompt-char sh)))
      ;; 	  (format nil "~a "
      ;; 		  (make-string (+ 1 *lish-level*)
      ;; 			       :initial-element (lish-prompt-char sh)))
	  *fallback-prompt*))

;; This isn't even "safe".
(defun safety-prompt (sh)
  "Return a prompt, in a manner unlikely to fail."
  (or (and (lish-prompt-function sh)
	   (or (ignore-errors (funcall (lish-prompt-function sh) sh))
	       (format t "Your prompt function failed.~%")))
      (or (ignore-errors (make-prompt sh))
	  (progn
	    (format t "Your prompt is broken.~%")
	    *fallback-prompt*))))

(defun is-lisp-expr-p (expr)
  "Return true if the the shell expr is likely just a wrapped Lisp expr."
  (and (= 1 (length (shell-expr-words expr)))
       (consp (elt (shell-expr-words expr) 0))))

(defun is-probably-a-lisp-expr-p (word)
  (let ((trimmed (ltrim word)))
    (and (not (zerop (length trimmed)))
	 (eql #\( (aref trimmed 0)))))

(defun colorize-lisp (expr fat-string)
  "Colorize a lisp expression."
  (declare (ignore expr fat-string))
  ;; @@@
  nil)

(defun themify-shell-word-in-fat-string (word string theme-item)
  "Apply the THEME-ITEM, which should be a style, to the WORD in the fat
string STRING. Don't do anything if theme-item isn't found or is nil."
  (when (theme:theme-value theme:*theme* theme-item)
    (let* ((fcs string #| (fat-string-string string) |#)
	   (style (oelt (style:themed-string theme-item '("x")) 0)))
      ;; (dbugf :recolor "word ~s~%style ~s~%item ~s~%string ~s~%"
      ;; 	     word style theme-item string)
      (when (not (zerop (length (word-word word))))
	(loop :for i :from (shell-word-start word) :below (shell-word-end word)
	   :do (copy-fatchar-effects style (aref fcs i)))))))

(defun unthemify-shell-word-in-fat-string (word string)
  (loop :for i :from (shell-word-start word) :below (shell-word-end word)
     :do (remove-effects (aref string i))))

(defun colorize-expr (expr fat-str)
  "Colorize a shell expression."
  (let* ((first-word (and (shell-expr-p expr) (first (shell-expr-words expr))))
	 type)
    (flet ((theme-it (tt word)
	     "Apply theme TT to WORD."
	     (themify-shell-word-in-fat-string word fat-str tt)))
      (cond
	((is-lisp-expr-p expr)
	 (colorize-lisp expr fat-str))
	((and (listp first-word) (keywordp (first first-word)))
	 ;; colorize the sub-expression of a compound expression
	 (colorize-expr (second first-word) fat-str))
	((shell-expr-p expr)
	 (when (and first-word (stringp (word-word first-word)))
	   ;; @@@ stupid hack to not turn lisp red
	   (when (not (is-probably-a-lisp-expr-p (word-word first-word)))
	     (setf type (command-type *shell* (word-word first-word)))
	     ;; (dbugf :recolor "command-type ~s~%" type)
	     (case type
	       ((:external-command :builtin-command :shell-command :command
	         :alias :global-alias :function)
		(theme-it `(:command ,type :style) first-word))
	       (:file (theme-it '(:command :system-command :style) first-word))
	       (otherwise (theme-it '(:command :not-found :style) first-word)))))
	 (loop :for w :in (rest (shell-expr-words expr))
	    :do
	      (when (stringp (word-word w))
		(cond
		  ((handler-case
		       (nos:file-exists (glob:expand-tilde (word-word w)))
		     (opsys-error (c)
		       (declare (ignore c))))
		   (theme-it '(:command-arg :existing-path :style) w))
		  (t
		   (unthemify-shell-word-in-fat-string w fat-str))))))))))

(defun colorize (e)
  "Colorize the editor buffer."
  (when (lish-colorize *shell*)
    ;;(format t "buf = ~s ~s~%" (type-of (rl::buf e)) (rl::buf e))
    (let ((expr (shell-read (char-util:simplify-string (rl::buf e))
			    :partial t)))
      (when (shell-expr-p expr)
	(colorize-expr expr (rl::buf e))
	;; (dbugf :recolor "fat-str ~s~%" (rl::buf e))
	))))

;; EOF
