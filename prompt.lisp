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
		   (#\h (write-string (princ-to-string dlib:*host*) str))
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

;; @@@ Consider dealing with the overlap between this and
;; fatchar:span-to-fatchar-string.

(defun symbolic-prompt-to-string (sh symbolic-prompt #| &optional ts-in |#)
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
(defun symbolic-prompt-to-string (symbolic-prompt &optional ts-in)
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
  (with-output-to-string (str)
    (if (not (consp symbolic-prompt))
	(princ symbolic-prompt str)
	(let ((ts (or ts-in (make-terminal-stream str 'terminal-ansi))))
	  (loop :for s :in symbolic-prompt :do
	     (typecase s
	       (string (terminal-write-string ts s))
	       (character (terminal-write-char ts s))
	       (cons
		(cond
		  ((keywordp (car s))
		   (case (car s)
		     (:normal
		      (terminal-normal ts)
		      (symbolic-prompt-to-string (cdr s) ts))
		     (:bold
		      (terminal-bold ts t)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-bold ts nil))
		     (:underline
		      (terminal-underline ts t)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-underline ts nil))
		     (:inverse
		      (terminal-inverse ts t)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-inverse ts nil))
		     ((:black :fg-black)
		      (terminal-color ts :black nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts :default nil))
		     ((:red :fg-red)
		      (terminal-color ts :red nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts :default nil))
		     ((:green :fg-green)
		      (terminal-color ts :green nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts :default nil))
		     ((:yellow :fg-yellow)
		      (terminal-color ts :yellow nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts :default nil))
		     ((:blue :fg-blue)
		      (terminal-color ts :blue nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts :default nil))
		     ((:magenta :fg-magenta)
		      (terminal-color ts :magenta nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts :default nil))
		     ((:cyan :fg-cyan)
		      (terminal-color ts :cyan nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts :default nil))
		     ((:white :fg-white)
		      (terminal-color ts :white nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts :default nil))
		     ((:default :fg-default)
		      (terminal-color ts :default nil)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts :default nil))
		     ;; background
		     ((:bg-black)
		      (terminal-color ts nil :black)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts nil :default))
		     ((:bg-red)
		      (terminal-color ts nil :red)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts nil :default))
		     ((:bg-green)
		      (terminal-color ts nil :green)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts nil :default))
		     ((:bg-yellow)
		      (terminal-color ts nil :yellow)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts nil :default))
		     ((:bg-blue)
		      (terminal-color ts nil :blue)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts nil :default))
		     ((:bg-magenta)
		      (terminal-color ts nil :magenta)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts nil :default))
		     ((:bg-cyan)
		      (terminal-color ts nil :cyan)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts nil :default))
		     ((:bg-white)
		      (terminal-color ts nil :white)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts nil :default))
		     ((:bg-default)
		      (terminal-color ts nil :default)
		      (symbolic-prompt-to-string (cdr s) ts)
		      (terminal-color ts nil :default))
		     (otherwise
		      (error "Unrecognized attribute ~a" (car s)))))
		  ((and (symbolp (car s)) (fboundp (car s)))
		   ;; (terminal-format ts "~a"
		   ;;   (apply (symbol-function (car s)) (cdr s))))
		   (terminal-format ts "~a" (eval s)))
		  (t
		   (error "Unrecognized thing in attribute list ~a" (car s))
		   )))
	       (symbol
		(when (boundp s)
		  (terminal-format ts "~a" (symbol-value s))))
	       (t
		(terminal-format ts "~a" s)
		)))
	  (terminal-finish-output ts)))))
|#

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

;; EOF
