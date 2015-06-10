;;
;; mine.lisp - Dig for command data
;;

;; $Revision$

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
  (loop :for dir :in (split-sequence #\: (getenv "MANPATH"))
     :do
     (format t "~a:~%" dir)
     (loop :for sec :in *command-sections*
	:do
	(format t "  ~a:~%" sec)
	(loop :for f :in (glob (s+ dir "/" sec "/*"))
	   :do
	   (format t "    ~a~%" (basename f))
	   (mine-file f)))))

(defun mine-command-help ()
  )

(defun mine ()
  (mine-manual-pages)
  (mine-command-help)
  ;; what else? other shells?
  )

;; EOF
