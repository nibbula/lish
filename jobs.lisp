;;
;; jobs.lisp - Shell job objects
;;

(in-package :lish)

;; If we someday jettison being tied to a text only world, we can get rid of
;; the *job-counter* and job-id, which only exists so you can type it in text.
;; In that world, we could have the job object *in* the command line. We would
;; of course have to have some, presumablely interactive, way of putting the
;; object in there.

;; So, of course, I would like a portable atomic operations library, pretty
;; much like the one in SBCL. Does such a thing exist? Probably. But how am I
;; supposed to find it??? So instead you get another page from the cookbook of
;; failure.

(declaim (type fixnum *job-counter*))

#+sbcl (sb-ext:defglobal *job-counter* 0 "A counter for job identifier numbers.")
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl (defalias 'atomic-incf 'sb-ext:atomic-incf))

#-sbcl (defvar *job-counter* 0 "A counter for job identifier numbers.")
(eval-when (:compile-toplevel :load-toplevel :execute)
  #-sbcl (defalias 'atomic-incf 'incf))

(deftype job-status ()
  "A keyword indicating what's going on with a job."
  '(member :running :stopped :suspended :dead))

(defclass job ()
  ((id
    :initarg :id :accessor job-id
    ;;:initform (atomic-incf *job-counter*)
    :type integer
    :documentation "Still stuck in the text world.")
   (name
    :initarg :name :accessor job-name
    :documentation "Some kind of name for the job.")
   (command-line
    :initarg :command-line :accessor job-command-line
    :documentation "The command line that started it.")
   (status
    :initarg :status :accessor job-status :initform :running :type job-status
    :documentation "Keyword indicating what's going on with the job."))
  (:default-initargs
   :id (atomic-incf *job-counter*))
  (:documentation "A generic shell job."))

(defmethod print-object ((object job) stream)
  "Print a job to STREAM."
  (with-slots (id name status command-line) object
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~s ~s ~s ~s" id name status command-line))))

(defgeneric continue-job-in-foreground (job)
  (:documentation "Continue a stopped job in the foreground."))

(defgeneric continue-job-in-background (job)
  (:documentation "Continue a stopped job in the background."))

(defgeneric kill-job (job &key signal)
  (:documentation "Destroy a job."))

(defgeneric list-all-jobs (type)
  (:documentation "List all jobs of a type."))

(defgeneric check-job-status (shell type)
  (:documentation "Check status of all jobs of a type."))

(defun job-p (object)
  "Return true if OBJECT is a JOB."
  (typep object 'job))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; System jobs

(defclass system-job (job)
  ((pid
    :initarg :pid :accessor job-pid
    :documentation "System process identifier.")
   (process-group
    :initarg :process-group :accessor job-process-group  
    :documentation "System process group.")
   ;; (process-handle-value ;; @@@ resolve this somehow
   ;;  :initarg :process-handle-value :accessor job-process-handle-value
   ;;  :documentation "Job process handle.")
   )
  (:documentation "An operating system job."))

(defmethod job-pid ((job job))
  "Pretend non-system jobs have a NIL PID."
  nil)

(defmethod continue-job-in-foreground ((job system-job))
  #+unix
  (multiple-value-bind (result status)
      (os-unix::resume-background-pid (job-pid job))
    (handle-job-change job result status)))

(defmethod continue-job-in-background ((job system-job))
  #+unix
  (progn
    (uos::background-pid (job-pid job))
    (setf (job-status job) :running)))

(defmethod kill-job ((job system-job) &key signal)
  #+unix (os-unix:kill (job-pid job) (or signal uos:+SIGTERM+))
  #+windows (funcall (caddr (find signal *siggy* :key #'second)) (job-pid job))
  )

(defmethod list-all-jobs ((type (eql 'system-job)))
  ;; @@@ This is underwhelming. We probably should use the OS specific
  ;; system-procces-list, but of course that creates more problems.
  (mapcar (_ (make-instance 'system-job
			    :id (os-process-id _)
			    :name (os-process-name _)
			    :command-line ""
			    :pid (os-process-id _)))
	  (nos:process-list)))

(defmethod check-job-status (shell (type (eql 'system-job)))
  (let (job pid result status)
    (loop :do
       (multiple-value-setq (pid result status) (nos:check-jobs))
       :while pid
       :do
	(if (setf job (find pid (lish-jobs shell) :test #'eql :key #'job-pid))
	    (handle-job-change job result status)
	    (format t "Unknown job changed ~a~%" pid)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp jobs

(defclass lisp-job (job)
  ((resume-function
    :initarg :resume-function :accessor job-resume-function
    :documentation "A function that resumes the job."))
  (:documentation "A job which is a Lisp closure."))

(defmethod continue-job-in-foreground ((job lisp-job))
  (when (job-resume-function job)
    (setf (lish-jobs *shell*) (delete job (lish-jobs *shell*)))
    (funcall (job-resume-function job))))

(defmethod continue-job-in-background ((job lisp-job))
  (error "I don't know how to background a Lisp job yet."))

(defmethod kill-job ((job lisp-job) &key signal)
  (declare (ignore signal))
  (error "I don't know how to kill a Lisp job yet."))

(defmethod list-all-jobs ((type (eql 'lisp-job)))
  ;; @@@ To really do this we would have to somehow find all shell instances.
  ;; It would probably be cool to be able to resume a job from a diffent shell,
  ;; but we would probably have to add a terminal switching interface.
  (remove-if (_ (not (typep _ 'lisp-job))) (lish-jobs *shell*)))

(defmethod check-job-status (shell (type (eql 'lisp-job)))
  (declare (ignore shell))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thread jobs

(defclass thread-job (job)
  ((thread
    :initarg :thread :accessor job-thread  
    :documentation "A thread object."))
  (:documentation "A job which is a thread."))

(defmethod continue-job-in-foreground ((job thread-job))
  (bt:join-thread (job-thread job)))

(defmethod continue-job-in-background ((job thread-job))
  ;; @@@ If we created the the thread, we could have set up
  ;; a condition variable that in the interrupt handler we would
  ;; ask the thread to wait on, then we could use
  ;; condition-notify here to wake it up, effectively backgrounding it.
  (error "Threads can't be backgrounded yet."))

(defmethod kill-job ((job thread-job) &key signal)
  (declare (ignore signal))
  (bt:destroy-thread (job-thread job)))

(defmethod list-all-jobs ((type (eql 'thread-job)))
  ;; @@@ This still has the id specification problem.
  (when bt:*supports-threads-p*
    (mapcar (_ (make-instance 'thread-job
			      :id nil
			      :name (bt:thread-name _)
			      :command-line ""
			      :thread _))
	    (bt:all-threads))))

(defmethod check-job-status (shell (type (eql 'thread-job)))
  (loop :for j :in (lish-jobs shell)
     :when (typep j 'thread-job) :do
       (cond
	 ((not (find (job-thread j) (bt:all-threads)))
	  (format t ";; Thread done ~a~%" (job-name j))
	  (delete-job j))
	 ((not (bt:thread-alive-p (job-thread j)))
	  ;; It's destroyed but it's still in all-threads ??
	  (setf (job-status j) :dead)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *job-types* '(system-job lisp-job thread-job)
  "All the different job types.")

(defun job-type-name (job)
  "Return a personized string for the type of JOB."
  (if (not (typep job 'job))
      "????"
      (string-capitalize (remove-suffix (string (type-of job)) "-JOB"))))

(defun find-job (job-descriptor)
  "Return a job given a descriptor."
  (cond
    ;; Presumably this is a good guess.
    ((null job-descriptor)
     (first (lish-jobs *shell*)))
    ((stringp job-descriptor)
     ;; strip off a leading percent
     (when (and (not (zerop (length job-descriptor)))
		(char= (char job-descriptor 0) #\%))
       (setf job-descriptor (subseq job-descriptor 1)))
     (or
      (find job-descriptor (lish-jobs *shell*) :test #'equalp :key #'job-name)
      (find job-descriptor (bt:all-threads)
	    :test #'equalp
	    :key #'bt:thread-name)
      (and (setf job-descriptor (ignore-errors (parse-integer job-descriptor)))
	   (find job-descriptor (lish-jobs *shell*)
		 :test #'eql :key #'job-id))))
    ((numberp job-descriptor)
     (find job-descriptor (lish-jobs *shell*) :test #'= :key #'job-id))
    ((symbolp job-descriptor)
     (or (find (string job-descriptor) (lish-jobs *shell*) :test #'equalp
	       :key #'job-name)
	 (when (find-package :bt)
	   (find (string job-descriptor) (bt:all-threads)
		 :test #'equalp
		 :key #'bt:thread-name))))
    (t
     (find job-descriptor (lish-jobs *shell*) :test #'equalp
	   :key #'job-name))))

;; EOF
