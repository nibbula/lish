;;
;; vars.lisp - Variables for Lish
;;

;; $Revision$

(in-package :lish)

;; We should almost always provide backwards compatibility as an option, but
;; releases with the same major version number should always be compatible
;; in the default configuration.
(defparameter *major-version* 0
  "Major version number. Releases with the same major version number should be
compatible in the default configuration.")
(defparameter *revision* "$Revision: 1.15 $"
  "Minor version number. This should change at least for every release,
probably for every commit to the master.")
(defparameter *version*
  (format nil "~d.~a" *major-version*
	  (subseq *revision* (1+ (position #\space *revision*))
		  (position #\space *revision* :from-end t))))
(defparameter *shell-name* "Lish"
  "The somewhat superfluous name of the shell.")

(defvar *shell* nil
  "The current shell instance.")

;; Like on windows this is #\; right? But not cygwin?
;; @@@ This should be in opsys 
(defvar *path-separator*
  #-windows #\:
  #+windows #\;
  "Separator in the PATH environement variable.")

;; @@@ Something else that should be in opsys
(defvar *buffer-size* (nos:getpagesize)
  "General buffer size for file or stream operations.")

;; EOF
