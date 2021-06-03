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

;; End
