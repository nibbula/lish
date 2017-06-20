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
  "Evaluate the BODY with STREAM-VAR open on FILENAME. If FILENAME has
a known compression suffix, then the stream is appropriately decompressed."
  (let ((in-stream (gensym "wpcf-in-stream"))
	(method    (gensym "wpcf-method")))
    `(let (,method)
       (if (setf ,method
		 (cadr (find-if
			#'(lambda (s) (glob:fnmatch (s+ "*." s) ,filename))
			*compression-suffixes* :key #'car)))
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
      (ppcre:scan-to-strings "\\.([^ ]*)\\s+(.*)$" line))
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
	       (ppcre:scan-to-strings "^\\s*(\\w+)\\s+\\\\-\\s+(.*)$" line))
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
      (!$ "manpath")
      (error "Can't figure out the where the manuals are.")))

(defun find-manual-file (name)
  "Return the manual page file for something named NAME."
  (loop :for dir :in (split-sequence #\: (manpath))
     :do
     ;;(format t "~a:~%" dir)
     (loop :for sec :in *command-sections*
	:do
	;;(format t "  ~a:~%" sec)
	(loop :for f :in (glob (s+ dir "/" sec "/*"))
	   ;;:do
	   ;; (format t "    ~a~%" (path-snip-ext
	   ;; 			 (path-snip-ext (path-file-name f))))
	   :when (equal name (path-snip-ext (path-snip-ext (path-file-name f))))
	   :do (return-from find-manual-file f)))))

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
	contents start end usage str (no-nl-count 0))
    (when (and (or (equal "x-executable" (magic:content-type-name type))
		   (equal "x-sharedlib" (magic:content-type-name type)))
	       (equal "application" (magic:content-type-category type)))
      (with-open-file (stream file :element-type '(unsigned-byte 8))
	(setf contents (slurp stream)
	      start (search "usage" contents
			    :test (lambda (a b)
				    (char-equal a (code-char b)))))
	(flet ((read-null-terminated-string ()
		 (setf end (position 0 contents :start start))
		 (when end
		   (prog1 (map 'string #'code-char (subseq contents start end))
		     (setf start end))))
	       (skip-zeros ()
		 (loop :with len = (length contents)
   		    :while (and (< start len) (zerop (aref contents start)))
		    :do (incf start)))
	       (check-newline ()
		 (or (and (eql #\newline (aref str (1- (length str))))
			  (setf no-nl-count 0))
		     (and (< no-nl-count *newline-fudge*) (incf no-nl-count)))))
	  (dbugf 'mine-bin "got start ~s~%" start)
	  (when start
	    (loop :with i = 0
	       :while (and (setf str (read-null-terminated-string))
			   (< i 200) ;; @@@ some limit?
			   ;;(eql #\newline (aref str (1- (length str))))
			   (check-newline)
			   )
	       :do
	       (push str usage)
	       (skip-zeros)
	       (incf i)))
	  (nreverse usage))))))

(defun get-binary-usage (file)
  (let ((strings (flatten (mapcar (_ (split-sequence #\newline _))
				  (get-binary-usage-strings file))))
	usage-line doc args arg b e)
    (declare (ignorable b e))
    (macrolet ((match (string)
		 `(multiple-value-setq (b e starts ends)
		    (ppcre:scan ,string line)))
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
	 ((and (not usage-line) (ppcre:all-matches "^[Uu]sage:" line))
	  (setf usage-line line))
	 ;; lines of documentation before main arguments
	 ((and (not args) (not (ppcre:scan "^\\s*-" line)))
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
	 ((and arg (ppcre:scan "^\\s+" line))
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
       :long-description (join (nreverse doc) #\newline)
       :args (nreverse args))))))

(defun convert-arglist (arglist)
  (loop :with type
     :for a :in arglist
     :do
     (setf type (getf a :type))
     ;;(remf a :type)
     :collect (apply #'make-instance type a)))

;; @@@ Implement checksums 
(defun mine-command (command-name)
  (let* ((cmd (get-command command-name))
	 (path (command-pathname command-name))
	 (mined (or (and path (get-binary-usage path))
		    (mine-manual command-name))))
    (when mined
      (cond
	((not cmd)
	 ;; make a new external command
	 (make-external-command command-name
				path
				(convert-arglist (mined-cmd-args mined))
				(join (flatten
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
		  (join (flatten
			 (append
			  (list (mined-cmd-short-description mined))
			  (list (mined-cmd-long-description mined))))
			#\newline)))))
	(t
	 (error "Command must be external or undefined to mine it."))))))

(defun mine-command-help ()
  )

(defun mine ()
  (mine-manual-pages)
  (mine-command-help)
  ;; what else? other shells?
  )

;; EOF
