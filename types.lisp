;;;
;;; types.lisp - Shell types
;;;

(in-package :lish)

(define-condition shell-error (error)
  ((format
    :accessor shell-error-format
    :initarg :format
    :type string
    :documentation "Format control for error reporting.")
   (arguments
    :accessor shell-error-arguments
    :initarg :arguments :initform nil
    :type list
    :documentation "Format arguments for error reporting."))
  (:report (lambda (c s)
	     (when (shell-error-format c)
	       (format s "~?"
		       (shell-error-format c)
		       (shell-error-arguments c)))))
  (:documentation "An error ocurring in the shell."))

(defstruct file-expansion
  "Result of filename expansion."
  files)

(defgeneric spread (object)
  (:documentation "Spread a shell argument.")
  (:method (object) object)
  (:method ((object file-expansion)) (file-expansion-files object)))

(defmethod print-object ((object file-expansion) stream)
  "Print a file-expansion to STREAM."
  (with-slots (files) object
    (cond
      (*print-readably*
       (call-next-method))
      (t
       (format stream (s+ "~{"
			  (if *print-escape*
			      "~a"
			      "~s")
			  "~^ ~}")
	       (file-expansion-files object))))))

;; End
