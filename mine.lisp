;;
;; mine.lisp - Dig for command data
;;

;; This whole thing is an horrible hack.
;;
;; One could postulate a world in which it could be eliminated by having every
;; developer that makes a command, also making a defexternal or some other
;; agreed upon format which all shells could read. Either that or we would
;; submit patches to every debian/ubuntu/macports/bsdports/cygwin etc. package
;; which provides commands, to include a defexternal or similar.  Oh, and then
;; every maintainer would keep them up to date.  I think it's safe to say,
;; that's not going to happen.
;;
;; Another only slightly more feasible possibility would be that we would
;; maintain a massive accurately hand-crafted database of definitions for
;; every command. And then somehow keep it up to date. This also seems quite
;; unlikely, mostly for the “up to date” part. This is what other shells seem
;; to do.
;;
;; I suppose if I could muster the enormous motivation for such a project, I
;; could devise a format which would be a superset of the capabilities of
;; bash, zsh, fish, (others?) and lish, and develop a combined database that
;; would encompass all the combined knowledge, and then develop modules for
;; each shell to read it, and then submit patches, then the ‘universal’
;; command database might a hope of being maintained. But of course the
;; maintenance of it should really lie with the individual command
;; implementors. This also seems very far-fetched.
;;
;; So instead this.
;; The general idea is we shamelessly try dig up the command line arguments from
;; where ever they may reside. In order of authority/accuracy that would seem
;; to be:
;;  1. The command executable
;;  2. The man page
;;  3. Some other shell's definition.
;;
;; Generating the data on demand has the meager advantages of:
;;   - Only having to store data for commands you have used.
;;   - Hopefully being as up to date as the source that's stolen from.
;;
;; This approach has the disadvantages of:
;;   - Likely to fail
;;   - Slow
;;   - Hackish and complex
;;   - Incomplete

(in-package :lish)

(defvar *command-sections* '("man1" "man6" "man8")
  "Normal sections where commands live.")

(defvar *compression-suffixes*
  '(("gz"  chipz:gzip)
    ("bz2" chipz:bzip2)
    ("bz"  chipz:bzip2)
    ("zip" chipz:zlib)))

(defmacro with-possibly-compressed-file ((stream-var filename) &body body)
  "Evaluate the ‘body’ with ‘stream-var’ open on ‘filename’. If ‘filename’ has
a known compression suffix, then the stream is appropriately decompressed."
  (with-names (in-stream method)
    `(let ((,method
	     (cadr
	      (find-if #'(lambda (s)
			   (and (glob:fnmatch (s+ "*." s) ,filename) t))
		       *compression-suffixes* :key #'car))))
       (if ,method
	   (progn
	     (format t "Uncompressing~%")
	     (with-input-from-string
		 (,stream-var
		  (map 'string #'code-char
		       (with-open-file (,in-stream
					,filename
					:direction :input
					:element-type '(unsigned-byte 8))
			 (chipz:decompress nil ,method ,in-stream))))
	       ,@body))
	   (progn
	     (format t "Plain~%")
	     (with-open-file (,stream-var ,filename)
	       ,@body))))))
      
(defstruct mined-arg
  name
  description
  short-arg
  long-arg
  type
  default)

(defstruct mined-cmd
  name
  short-description
  long-description
  args)

(defun get-request (line)
  (let (req args)
    (multiple-value-setq (req args) 
      (scan-to-strings "\\.([^ ]*)\\s+(.*)$" line))
    (when req
      args)))

(defun mine-page (stream)
  (let ((cmd (make-mined-cmd)) line next match strings)
    (declare (ignorable match))
    (loop
       :while (setf line (read-line stream nil nil))
       :do
       (cond
	 ((eq next 'name)
	  (dbugf 'mine-man "blurg ~s~%" line)
	  (cond
	    ((begins-with ".Nm" line :test #'equalp)
	     (setf (mined-cmd-name cmd) (subseq line 4)
		   next 'short-description))
	    ((multiple-value-setq (match strings)
	       (scan-to-strings "^\\s*(\\w+)\\s+\\\\-\\s+(.*)$" line))
	     (dbugf 'mine-man "got name~%")
	     (setf (mined-cmd-name cmd) (aref strings 0)
		   (mined-cmd-short-description cmd) (aref strings 1))))
	  (setf next nil))
	 ((eq next 'synopsis)
	  (setf next nil))
	 ((eq next 'short-description)
	  (setf (mined-cmd-short-description cmd) (subseq line 4))
	  (setf next nil))
	 ((eq next 'long-description)
	  (cond
	    ((begins-with ".\\\"" line) #| ignore comment |# )
	    ((begins-with ".B" line)
	     (push (subseq line 3) (mined-cmd-long-description cmd))))
	  (setf next nil))
	 ((begins-with ".\\\"" line)
	  ;; ignore comments
	  )
	 ((begins-with ".sh" line :test #'equalp)
	  (cond
	    ((equalp (subseq line 4) "NAME")
	     (dbugf 'mine-man "Name~%")
	     (setf next 'name))
	    ((equalp (subseq line 4) "DESCRIPTION")
	     (dbugf 'mine-man "Description~%")
	     (setf next 'long-description))
	    ((equalp (subseq line 4) "OPTIONS")
	     (dbugf 'mine-man "Options~%")
	     (setf next 'options))
	    ((equalp (subseq line 4) "SYNOPSIS")
	     (dbugf 'mine-man "Synopsis~%")
	     (setf next 'long-description))))
	 ((begins-with ".Nd" line :test #'equalp)
	  (setf (mined-cmd-short-description cmd) (subseq line 4)))))
    cmd))
	  
(defun mine-file (file)
  (with-possibly-compressed-file (f file)
    (mine-page f)))

(defun manpath ()
  "Return the manual path."
  (or (nos:environment-variable "MANPATH")
      (and (nos:command-pathname "manpath") (!$ "manpath"))
      #-windows
      (warn "Can't figure out the where the manuals are.")
      ))

(defun find-manual-file (name)
  "Return the manual page file for something named NAME."
  (let ((manpath (manpath)))
    (when manpath
      (loop :for dir :in (split-sequence #\: manpath)
	 :do
	 ;;(format t "~a:~%" dir)
	 (loop :for sec :in *command-sections*
	    :do
	    ;;(format t "  ~a:~%" sec)
	    (loop :for f :in (glob (s+ dir "/" sec "/*"))
	       ;;:do
	       ;; (format t "    ~a~%" (path-snip-ext
	       ;; 			 (path-snip-ext (path-file-name f))))
	       :when (equal name (path-snip-ext
				  (path-snip-ext (path-file-name f))))
	       :do (return-from find-manual-file f)))))))

(defun mine-manual (command-name)
  (let ((file (find-manual-file command-name)))
    (and file (mine-file file))))

(defun mine-manual-pages ()
  (loop :for dir :in (split-sequence #\: (manpath))
     :do
     (format t "~a:~%" dir)
     (loop :for sec :in *command-sections*
	:do
	(format t "  ~a:~%" sec)
	(loop :for f :in (glob (s+ dir "/" sec "/*"))
	   :do
	   (format t "    ~a~%" (basename f))
	   (mine-file f)))))

(defparameter *newline-fudge* 5
  "How many strings without newlines to prospectively tolerate.")

;; This is the equivalent of screen scaping.
(defun get-binary-usage-strings (file)
  "Slyly try to extract the usage from strings in a binary executable."
  (let ((type (magic:guess-file-type file))
	contents start end usage str (no-nl-count 0) c cc len style
	arg-count)
    (when (and (or (equal "x-executable" (magic:content-type-name type))
		   (equal "x-pie-executable" (magic:content-type-name type))
		   (equal "x-sharedlib" (magic:content-type-name type)))
	       (equal "application" (magic:content-type-category type)))
      (with-open-file (stream file :element-type '(unsigned-byte 8))
	(setf contents (slurp stream :element-type '(unsigned-byte 8))
	      len (length contents)))
      (labels ((string-it (s) (map 'string #'code-char s))
	       (read-null-terminated-string ()
		 (setf end (position 0 contents :start start))
		 (when end
		   (prog1 (string-it (subseq contents start end))
		     (setf start end))))
	       (skip-zeros ()
		 (loop :while (and (< start len)
				   (zerop (aref contents start)))
		    :do (incf start)))
	       (check-newline ()
		 (or (and (eql #\newline (aref str (1- (length str))))
			  (setf no-nl-count 0))
		     (and (< no-nl-count *newline-fudge*)
			  (incf no-nl-count))))
	       (valid-byte (c)
		 (and (not (zerop c))
		      (eq (type-of (setf cc (code-char c))) 'standard-char)
		      (or (alpha-char-p cc) (other-char cc))))
	       (other-char (cc)
		 (or (digit-char-p cc) (position cc "-+_"))))
	;; usage tagged
	(when (setf start (search "usage" contents
				  :test (lambda (a b)
					  (char-equal a (code-char b)))))
	  (dbugf 'mine-bin "got usage start ~s~%" start)
	  (loop :with i = 0
	     :while (and (setf str (read-null-terminated-string))
			 (< i 200) ;; @@@ some limit?
			 ;;(eql #\newline (aref str (1- (length str))))
			 (check-newline)
			 )
	     :do
	     (push str usage)
	     (skip-zeros)
	     (incf i))
	  (setf style :gnu))
	(setf arg-count (loop :for u :in usage
			   :if (scan "^\\s*-" u) :count u))
	;;(format t "arg-count = ~s~%" arg-count)
	(when (or (not usage)
		  (< arg-count 2))
	  ;; Any strings starting with -
	  (setf start 0 usage nil)
	  (loop :with i = 1 :and digit-count :and arg-count = 0
	     :while (and (setf start (position (char-code #\-) contents
					       :start start))
			 (< arg-count 150))
	     :do
	     ;;(format t "start ~a~%" start)
	     (when (and start (not (zerop start))
			(not (valid-byte (aref contents (1- start)))))
	       (setf i 1 digit-count 0)
	       (loop
		  :while (and (setf c (aref contents (+ start i)))
			      (valid-byte c))
		  :do (incf i)
		  ;; don't do too many digits
		  (when (other-char cc)
		    (incf digit-count))
		  :while (and (< i 50)	; Arbitrary length limits
			      (< digit-count 5)))
	       (when (and (not (zerop i)) (>= i 5)
			  (< digit-count 5)
			  (> (- i 2) digit-count))
		 (setf str (string-it (subseq contents start (+ start i))))
		 (pushnew str usage :test #'string-equal)
		 (incf arg-count)
		 ;;(format t "~a~%" str)
		 ))
	     (incf start i)
	     (skip-zeros))
	   (setf style :whatever))
	(values (nreverse usage) style)))))

(defun get-binary-usage-gnu (file strings)
  (let (usage-line doc args arg b e)
    (declare (ignorable b e))
    (setf strings (flatten (mapcar (_ (split-sequence #\newline _)) strings)))
    (macrolet ((match (string)
		 `(multiple-value-setq (b e starts ends)
		    (scan ,string line)))
	       (slarg (&rest props)
		 `(progn
		    (when arg
		      (push arg args))
		    (setf arg (list ,@props)))))
    (dbugf 'mine-bin "not very ~s~%" (length strings))
    (loop :with s = strings :and (line starts ends)
       :while s
       :do
       (setf line (car s))
       (dbugf 'mine-bin "line = ~s~%" line)
       (cond
	 ;; initial usage line
	 ((and (not usage-line) (all-matches "^[Uu]sage:" line))
	  (setf usage-line line))
	 ;; lines of documentation before main arguments
	 ((and (not args) (not (scan "^\\s*-" line)))
	  (if (zerop (length line))
	      (when (stringp (car doc))
		(rplaca doc (s+ (car doc) #\newline)))
	      (push line doc)))
	 ;; -e --example       Blah blah blah.
	 ((match "^\\s*-([A-Za-z0-9?])[,]?\\s*--([-A-Za-z0-9_/:]+)(\\s+(.*)|\\s*)$")
	  (slarg :name      (subseq line (aref starts 1) (aref ends 1))
		 :type	    'arg-boolean
		 :short-arg (char line (aref starts 0))
		 :long-arg  (subseq line (aref starts 1) (aref ends 1))
		 :help      (trim (subseq line (aref starts 2) (aref ends 2)))))
	 ;; -e --example=foo   Blah blah blah.
	 ((match "^\\s*-([A-Za-z0-9?])[,]?\\s*--([-A-Za-z0-9_/:]+)[[]*=([A-Za-z]+)[]]*(\\s+(.*)|\\s*)$")
	  (slarg :name      (subseq line (aref starts 1) (aref ends 1))
		 :type	    'arg-string
		 :short-arg (char line (aref starts 0))
		 :long-arg  (subseq line (aref starts 1) (aref ends 1))
		 :help      (trim (subseq line (aref starts 3) (aref ends 3)))))
	 ;; -e                 Blah blah blah.
	 ((match "^\\s*-([A-Za-z0-9?])\\s+(.*)$")
	  (slarg :name (s+ "dash-" (char line (aref starts 0)))
		 :type 'arg-boolean
		 :short-arg (char line (aref starts 0))
		 :help (subseq line (aref starts 1) (aref ends 1))))
	 ;; --example          Blah blah blah.
	 ((match "^\\s*--([-A-Za-z0-9_/:]+)(\\s+(.*)|\\s*)$")
	  (slarg :name (subseq line (aref starts 0) (aref ends 0))
		 :type 'arg-boolean
		 :long-arg (subseq line (aref starts 0) (aref ends 0))
		 :help (trim (subseq line (aref starts 1) (aref ends 1)))))
	 ;; --example=foo      Blah blah blah.
	 ((match "^\\s*--([-A-Za-z0-9_/:]+)[[]*=([A-Za-z]+)[]]*(\\s+(.*)|\\s*)$")
	  (slarg :name (subseq line (aref starts 0) (aref ends 0))
		 :type 'arg-string
		 :long-arg (subseq line (aref starts 0) (aref ends 0))
		 :help (trim (subseq line (aref starts 2) (aref ends 2)))))
	 ;; lines starting with spaces after args
	 ((and arg (scan "^\\s+" line))
	  (setf (getf arg :help)
		(s+ (getf arg :help)
		    (if (not (zerop (length (getf arg :help))))
			" " "")
		    (trim line))))
	 ;; start a new arg after blank lines
	 ((zerop (length line))
	  (when arg
	    (push arg args))
	  (setf arg nil)))
       (setf s (cdr s)))
    (when arg
      (push arg args))
    (when doc
      (dbugf 'mine-bin "what up doc ~s~%" (car doc))
      (if args
	  ;; Get rid of up to *newline-fudge* doc strings with no newlines.
	  (loop :with i = 0
	     :while (and doc
			 (< i *newline-fudge*)
			 (char/= #\newline
				 (char (car doc) (1- (length (car doc))))))
	     :do
	     (setf doc (cdr doc))
	     (incf i))
	  ;; If we didn't find any command line args, chances are good
	  ;; the doc is junk.
	  (setf doc
		(if (char= #\newline (char (car doc) (1- (length (car doc)))))
		    (list (car doc))
		    nil))))

    (when usage-line
      (make-mined-cmd
       :name (path-file-name file)
       :short-description (replace-subseq "%s" (path-file-name file) usage-line)
       :long-description (join-by-string (nreverse doc) #\newline)
       :args (nreverse args))))))

(defun get-binary-usage-whatever (file strings)
  (make-mined-cmd
   :name (path-file-name file)
   :short-description (princ-to-string (path-file-name file))
   :long-description ""
   :args (loop :for s :in strings
	    :collect `(:name ,(subseq s 1)
		       :type arg-boolean
		       ,@(if (char= (aref s 1) #\-)
			     (list :long-arg (subseq s 2))
			     (list :old-long-arg (subseq s 1)))))))

(defun get-binary-usage (file)
  (multiple-value-bind (strings style)
      (get-binary-usage-strings file)
    (case style
      (:gnu (get-binary-usage-gnu file strings))
      (otherwise (get-binary-usage-whatever file strings)))))

(defun convert-arglist (arglist)
  (loop :with type
     :for a :in arglist
     :do
     (setf type (getf a :type))
     ;;(remf a :type)
     :collect (apply #'make-instance type a)))

;; @@@ Implement checksums 
(defun mine-command (command-name &key quietly)
  (let* ((cmd (get-command command-name))
	 (path (command-pathname command-name))
	 (mined (or (and path (get-binary-usage path))
		    (if quietly
			(without-warning (mine-manual command-name))
			(mine-manual command-name)))))
    (when mined
      (cond
	((not cmd)
	 ;; make a new external command
	 (make-external-command command-name
				path
				(convert-arglist (mined-cmd-args mined))
				(join-by-string
				 (flatten
				  (append
				   (list
				    (mined-cmd-short-description mined))
				   (list
				    (mined-cmd-long-description mined))))
				 #\newline)))
	((typep cmd 'external-command)
	 (cond
	   ((external-command-manual cmd)
	    (error "Won't override a manual command."))
	   (t
	    (setf (command-arglist cmd) (convert-arglist (mined-cmd-args mined))
		  (documentation (command-function-name command-name) 'function)
		  (join-by-string (flatten
				   (append
				    (list (mined-cmd-short-description mined))
				    (list (mined-cmd-long-description mined))))
				  #\newline)))))
	(t
	 (error "Command must be external or undefined to mine it."))))))

(defun mine-command-help ()
  ;; This is probably a bad idea. If we ever implement it, it should be an
  ;; option which is off by default.
  )

(defun mine ()
  (mine-manual-pages)
  (mine-command-help)
  ;; what else? other shells?
  )

;; EOF
