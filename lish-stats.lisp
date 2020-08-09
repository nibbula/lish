;;
;; lish-stats.lisp - Command statistics for Lish.
;;

(defpackage :lish-stats
  (:documentation "Command statistics for Lish.")
  (:use :cl :dlib :dlib-misc :lish)
  (:export
   #:!stats
   ))
(in-package :lish-stats)

(defvar *command-stats* nil
  "Command statistics.")

(defstruct command-record
  "Statistics for commands."
  (type nil)
  (count 0 :type integer)
  (dates '()))
  
(defun record-command-stats (name type)
  (when (lish-collect-stats *shell*)
    (when (not *command-stats*)
      (setf *command-stats* (make-hash-table :test #'equal)))
    (let* ((nt (cons name type))
	   (record (gethash nt *command-stats*)))
      (when (not record)
	(setf record (make-command-record)))
      (setf (command-record-type record) type)
      (incf (command-record-count record))
      (push (get-universal-time) (command-record-dates record))
      (setf (gethash nt *command-stats*) record))))

;; @@@ This means if we want the real statistics, we'll have to combine them
;; all together. Otherwise we would have to deal with concurrent access. Or we
;; could require clsql and sqlite do constant updating, But that doesn't seem
;; like a good idea at this point.
(defun save-command-stats ()
  (when (lish-collect-stats *shell*)
    (let ((filename (s+ (user-homedir-pathname)
			"/.lish-stats-" (get-universal-time) "-"
			(random 100000))))
      (with-open-file (str filename :direction :output)
	(loop :with h
	   :for k :being :the :hash-keys :of *command-stats* :do
	   (setf h (gethash k *command-stats*))
	   (format str "(:command ~s :type ~s :count ~s :dates ~a)~%" (car k)
		   (command-record-type h) (command-record-count h)
		   (command-record-dates h))))
      filename)))

(defun show-command-stats (&optional (all nil))
  (declare (ignore all) #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
  (when (not *command-stats*)
    (error "No statistics are set in this session."))
  (let ((table 
	 (loop :with h
	    :for k :being :the :hash-keys :of *command-stats* :do
	    (setf h (gethash k *command-stats*))
	    :collect
	    (list 
	     (car k) (command-record-type h) (command-record-count h)))))
    ;; @@@ why is this sort so insane on sbcl? it spits a bunch of notes????
    (setf table (sort table #'> :key #'third)
	  table (make-table-from table
				 :column-names '("Command" "Type" "Count")))
    (table:print-table table)))

(defun merge-stats (from to)
  (when (not (equal (command-record-type from) (command-record-type to)))
    (warn "Command types to merge differ."))
  ;; Add the counts
  (incf (command-record-count to) (command-record-count from))
  (let (new-dates)
    ;; First normalize the existing dates
    (loop :for d :in (command-record-dates to) :do (pushnew d new-dates))
    ;; Then add the new ones
    (loop :for d :in (command-record-dates from) :do (pushnew d new-dates))
    (setf (command-record-dates to) new-dates)))

(defun glom-stats-files ()
  (let ((glom (make-hash-table :test #'equal)))
    ;; Read all the files into glom
    (loop :with expr :and from :and to
       :for f :in (glob (s+ (user-homedir-pathname) "/.lish-stats-*")) :do
       (format t "Reading from ~a...~%" f) (finish-output)
       (with-open-file (stream f)
	 (loop :with name
	    :while (setf expr (read stream nil nil))
	    :do
	    ;; Get an existing record
	    (setf name (cadr expr)
		  to (gethash name glom))
	    ;; Make a new record from expression we read
	    (setf from (apply #'make-command-record (cddr expr)))
	    (if (not to)
		(setf to from)
		(progn
		  ;; (format t "merge ~a ~a -> ~a~%" name from to)
		  (format t "merge ~a ~a -> ~a~%" name
			  (command-record-type from)
			  (command-record-type to))
		  (merge-stats from to)))
	    (setf (gethash name glom) to)))
       (format t "OK~%"))
    ;; Write the merged things out
    (with-open-file (stream
		     (s+ (user-homedir-pathname) "/.lish-stats-merged")
		     :direction :output
		     :if-exists :supersede)
      (loop :for k :being :the :hash-keys :of glom :using (hash-value v)
	 :do
	 (format stream "(:command ~s :type ~s :count ~s :dates ~s)~%"
		 (string k)
		 (command-record-type v)
		 (command-record-count v)
		 (command-record-dates v))))
    (format t "Done.~%")))

(defcommand stats
    (("command" choice :choices ("save" "show")
      :help "What to do with the statistics."))
  "Show command statistics."
  (cond
    ((equal command "save")
     (format t "Stats saved in ~a.~%" (save-command-stats)))
    ((equal command "start")
     (show-command-stats))
    ((equal command "stop")
     (show-command-stats))
    ((equal command "show")
     (show-command-stats))
    (t
     (show-command-stats))))

(add-hook lish:*pre-command-hook* #'record-command-stats)
(add-hook lish:*exit-shell-hook* #'save-command-stats)

;; EOF
