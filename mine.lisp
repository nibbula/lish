;;
;; mine.lisp - Dig for command data
;;

;; This whole thing is an horrible hack.
;;
;; One could postulate a world in which it could be eliminated by having every
;; developer that makes a command, also making a defexternal or some other
;; agreed upon format which all shells could read. Either that or I and/or my
;; minions would submit patches to every debian/ubuntu/macports/bsdports/cygwin
;; etc. package which provides commands, to include a defexternal or similar.
;; Oh, and then every maintainer would keep them up to date.
;; I think it's safe to say, that's not going to happen.
;;
;; Another only slightly more feasible possibility would be that we would
;; maintain a massive accurately hand-crafted database of definitions for
;; every command. And then somehow keep it up to date. This also seems quite
;; unlikely, mostly for the “up to date” part.
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
  (let ((out-str   (gensym "wpcf-out-str"))
	(in-stream (gensym "wpcf-in-stream"))
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
		  (with-output-to-string (,out-str
					  nil :element-type '(unsigned-byte 8))
		    (with-open-file (,in-stream
				     ,filename
				     :direction :input
				     :element-type '(unsigned-byte 8))
		      (chipz:decompress ,out-str ,method ,in-stream))))
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

(defun mine-page (stream)
  (let ((cmd (make-mined-cmd)) line next)
    (loop
       :while (setf line (read-line stream nil nil))
       :do
       (cond
	 ((eq next 'name)
	  (cond
	    ((begins-with ".Nm" line :test #'equalp)
	     (setf (mined-cmd-name cmd) (subseq line 4)
		   next 'short-description))
	    ((ppcre:all-matches "^\\(\\w+\\s+\\\\-\\s+" line)
	     (setf (mined-cmd-name cmd) (subseq line 4)
		   next 'short-description))))
	 ((eq next 'short-description)
	  (setf (mined-cmd-short-description cmd) (subseq line 4)))
	 ((begins-with ".Sh" line :test #'equalp)
	  (cond
	    ((equalp (subseq line 4) "NAME")
	     (setf next 'name))
	    ((equalp (subseq line 4) "DESCRIPTION")
	     (setf next 'long-description))))
	 ((begins-with ".Nd" line :test #'equalp)
	  (setf (mined-cmd-short-description cmd) (subseq line 4)))
	 ((begins-with ".Nd" line :test #'equalp)
	  )))
    cmd))
	  
(defun mine-file (file)
  (with-possibly-compressed-file (f file)
    (mine-page f)))

(defun mine-manual-pages ()
  (loop :for dir :in (split-sequence #\: (environment-variable "MANPATH"))
     :do
     (format t "~a:~%" dir)
     (loop :for sec :in *command-sections*
	:do
	(format t "  ~a:~%" sec)
	(loop :for f :in (glob (s+ dir "/" sec "/*"))
	   :do
	   (format t "    ~a~%" (basename f))
	   (mine-file f)))))

;; This is the (im)moral equivalent of screen scaping.
(defun get-binary-usage (file)
  "Slyly try to extract the usage from strings in a binary executable."
  (let ((type (magic:guess-file-type file))
	contents start end usage str)
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
		    :do (incf start))))
	  (when start
	    (loop :with i = 0
	       :while (and (setf str (read-null-terminated-string))
			   (< i 20)
			   (eql #\newline (aref str (1- (length str)))))
	       :do
	       (push str usage)
	       (skip-zeros)
	       (incf i)))
	  (nreverse usage))))))

(defun mine-command-help ()
  )

(defun mine ()
  (mine-manual-pages)
  (mine-command-help)
  ;; what else? other shells?
  )

;; EOF
