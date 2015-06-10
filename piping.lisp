;;
;; piping.lisp - Piping for Lish
;;

;; $Revision$

;;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
;;|| Piping
;;||
;;|| Piping, I/O redirection, and I/O functions that are useful for using in
;;|| a lish command line or script.

(in-package :lish)

(defun lisp-args-to-command (args &key (auto-space nil))
  "Turn the arguments into a string of arguments for a system command. String
arguements are concatenated together. Symbols are downcased and turned into
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

#|
;; I suppose we could make this generic so that streams can do a special
;; things with it, but that might be sort of edging into the stream protocol,
;; which simple-streams and 
(defun copy-stream (source destination)
  "Copy data from reading from SOURCE and writing to DESTINATION, until we get
an EOF on SOURCE."
  ;; ^^^ We could try to make *buffer-size* be the minimum of the file size
  ;; (if it's a file) and the page size, but I'm pretty sure that the stat
  ;; call and possible file I/O is way more inefficient than wasting less than
  ;; 4k of memory to momentarily. Of course we could mmap it, but it should
  ;; end up doing approximately that anyway and the system should have a
  ;; better idea of how big is too big, window sizing and all that. Also,
  ;; that's way more complicated. Even this comment is too much. Let's just
  ;; imagine that a future IDE will collapse or footnotify comments tagged
  ;; with "^^^".
  (let ((buf (make-array *buffer-size*
			 :element-type (stream-element-type source)))
	pos)
    (loop :do
       (setf pos (read-sequence buf source))
       (when (> pos 0)
	 (write-sequence buf destination :end pos))
       :while (= pos *buffer-size*))))
|#

(defun run-with-output-to (file-or-stream commands &key supersede)
  "Run commands with output to a file or stream."
  (let ((result nil))
    (multiple-value-bind (vals in-stream show-vals)
	(shell-eval
	 *shell* (shell-read (lisp-args-to-command commands)) :out-pipe t)
      (declare (ignore show-vals))
      (unwind-protect
	   (when (and vals (> (length vals) 0))
	     (with-open-file-or-stream (out-stream file-or-stream
						   :direction :output
						   :if-exists
						   (if supersede
						       :supersede :error)
						   :if-does-not-exist :create)
	       (copy-stream in-stream out-stream))
	     (setf result vals))
	(close in-stream)))
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

(defun map-output-lines (func command)
  "Return a list of the results of calling the function FUNC with each output
line of COMMAND. COMMAND should probably be a string, and FUNC should take one
string as an argument."
  (multiple-value-bind (vals stream show-vals)
      (shell-eval *shell* (shell-read command) :out-pipe t)
    (declare (ignore show-vals))
    (when (and vals (> (length vals) 0))
      (loop :with l = nil
	 :while (setf l (read-line stream nil nil))
	 :collect (funcall func l)))))

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
      (let* ((expr (shell-read command))
;	     (seq (shell-expr-words expr))
;	     (cmd (first seq))
;	     (args (cdr seq))
	     )
	;; (nos:with-process-output (proc cmd args)
	(multiple-value-bind (vals stream show-vals)
	    (shell-eval *shell* expr :out-pipe t)
	  (declare (ignore show-vals))
	  (when (and vals (> (length vals) 0))
	    (convert-to-words stream s)))))))

(defun command-output-list (command)
  "Return lines output from command as a list."
  (map-output-lines #'identity command))

;; (defvar *files-to-delete* '()
;;   "A list of files to delete at the end of a command.")
;;
;; ;; This has a lot of potential problems / security issues.
;; (defun != (&rest commands)
;;   "Temporary file name output substitution."
;;   (multiple-value-bind (vals stream show-vals)
;;       (shell-eval *shell* (shell-read (lisp-args-to-command commands))
;;                   :out-pipe t)
;;     (declare (ignore show-vals))
;;     (if (and vals (> (length vals) 0))
;; 	(let ((fn (nos:mktemp "lish")))
;; 	  (push fn *files-to-delete*)
;; 	  (with-posix-file (fd fn (logior O_WRONLY O_CREAT O_EXCL) #o600)
;; 	    (let ((buf (make-string (buffer-size))))
;; 	      (loop :while (read-sequence buf stream)
;; 	(progn
;; 	  (close stream)
;; 	  nil))))

;;; The problem with these shelly symbols is: even I can't remember them all.
;;; I think I can remember all the Tetris™ pieces or even all the Blokus™
;;; pieces, so it's not really not a capacity thing. The issue seems to be
;;; memorability versus succinctness. Perhaps if I could come up with one
;;; memorable word for each one of these and then these could be used as
;;; replacements (even automatcially / abbrev-like). Or maybe once they're
;;; working and I'm using them regularly, I will figure it out.
;;;
;;; I call these the Snormnambulous™®© Shellbilitous Frobmogrifiers.
;;; [after drinking far Too. Much. Coffee.]
;;;
;;; I'm also a bit worried that if we use these things in real code (vs
;;; interactive commands lines), we may start to look like Perl (and I
;;; really hope you know why that should be worrying:
;;; (e.g. https://sites.google.com/site/steveyegge2/ancient-languages-perl))

(defun ! (&rest args)
  "Evaluate the shell command."
  (shell-eval *shell* (shell-read (lisp-args-to-command args))))

(defun !? (&rest args)
  "Evaluate the shell command, converting Unix shell result code into boolean.
This means the 0 is T and anything else is NIL."
  (let ((result (shell-eval *shell* (shell-read (lisp-args-to-command args)))))
    (and (numberp result) (zerop result))))

(defun !$ (&rest command)
  "Return lines output from command as a string of words. This is basically like $(command) in bash."
  (command-output-words (lisp-args-to-command command)))

(defun !$$ (&rest command)
  "Return lines output from command as a string of quoted words."
  (command-output-words (lisp-args-to-command command) t))

(defun !_ (&rest command)
  "Return a list of the lines of output from the command."
  (command-output-list (lisp-args-to-command command)))

(defun !- (&rest command)
  "Return a string containing the output from the command."
  (with-output-to-string (str)
    (run-with-output-to str command)))

(defun !and (&rest commands)
  "Run commands until one fails."
  (declare (ignore commands))
  )

(defun !or (&rest commands)
  "Run commands if previous command succeeded."
  (declare (ignore commands))
  )

(defun !bg (&rest commands)
  "Run commands in the background."
  (declare (ignore commands))
  )

(defun !! (&rest commands)
  "Pipe output of commands. Return a stream of the output."
  (multiple-value-bind (vals stream show-vals)
      (shell-eval *shell* (shell-read (lisp-args-to-command commands))
		  :out-pipe t)
    (declare (ignore show-vals))
    (if (and vals (> (length vals) 0))
	stream
	(progn
	  (close stream)
	  nil))))

(defun !> (file-or-stream &rest commands)
  "Run commands with output to a file or stream."
  (run-with-output-to file-or-stream commands))

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
  (with-open-file-or-stream (in-stream file-or-stream)
    (multiple-value-bind (vals stream show-vals)
	(shell-eval *shell* (shell-read (lisp-args-to-command commands))
		    :in-pipe in-stream)
      (declare (ignore stream show-vals))
      (values-list vals))))

(defun !!< (file-or-stream &rest commands)
  "Run commands with input from a file or stream and return a stream of output."
  (with-open-file-or-stream (in-stream file-or-stream)
    (multiple-value-bind (vals stream show-vals)
	(shell-eval *shell* (shell-read (lisp-args-to-command commands))
		    :out-pipe t
		    :in-pipe in-stream)
      (declare (ignore show-vals))
      (if (and vals (> (length vals) 0))
	  stream
	  (progn
	    (close stream)
	    nil)))))

;; Perhaps just use CL-INTERPOL?
;;
;; (defun !-reader (stream char arg)
;;   (declare (ignore char arg))
;;   (read stream nil nil t))

;; (set-dispatch-macro-character #\# #\! #'!-reader)
;; (set-macro-character #\! (get-macro-character #\)))))

;; @@@ consider features in inferior-shell?

;; EOF
