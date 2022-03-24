;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try to load QuickLisp, if it's not alread loaded.

(defparameter *quicklisp-fail*
  "Perhaps try installing a new QuickLisp by following the instructions at:
https://www.quicklisp.org/beta/")

(defparameter *not-strictly-necessary*
  "QuickLisp is not strictly necessary, but if you don't have it, you will have
to make sure all the dependencies are availible to be loaded by ASDF.

Since you don't seem to have a QuickLisp installation in ~/quicklisp,
we are taking the liberty of installing one for you. I'm sorry.

If you want to build with a custom QuickLisp installation, set the environment
variable LISH_QUICKLISP to the directory where it's installed.
")

(defparameter *proxy-host* "localhost"
  "The hostname for the proxy.")

(defparameter *proxy-port* 8080
  "The TCP port for the proxy.")

(defun proxy-url ()
  "Return the proxy URL."
  (format nil "http://~a:~a" *proxy-host* *proxy-port*))

(defun install-quicklisp ()
  (load "build/quicklisp.lisp")
  (handler-case
      (cond
	((sf-getenv "LISH_QL_PROXY")
	 (if (zerop (length (sf-getenv "LISH_QL_PROXY")))
	     (progn
	       (warn "!!!!! INSECURE QUICKLISP INSTALL !!!!!!")
	       (funcall (find-symbol "INSTALL" :quicklisp-quickstart)))
	     (funcall (find-symbol "INSTALL" :quicklisp-quickstart)
		      :proxy (sf-getenv "LISH_QL_PROXY"))))
	(t
	 (funcall (find-symbol "INSTALL" :quicklisp-quickstart)
		  :proxy (proxy-url))))
    (error (c)
      (print c)
      (fail "~
We failed to install QuickLisp.
This is probably because you don't have a proxy set up. Installing Quicklisp
without a proxy is insecure. You can either set up a proxy on the default URL,
which is ~s, For example, with the mitmproxy command:
  $ mitmproxy --listen-host localhost -p 8080 -M \"/^http:/https:\"
Or, if you don't want to use the default proxy, set LISH_QL_PROXY to a
different one, or if you REALLY want to do it insecurely set LISH_QL_PROXY
to empty.

Note that:
~s" (proxy-url) *not-strictly-necessary*))))
  
(when (not (find-package :quicklisp))
  (if (sf-getenv "LISH_QUICKLISP")
      (handler-case
      	  (load (sf-getenv "LISH_QUICKLISP"))
	(error (c)
	  (print c)
	  (fail "LISH_QUICKLISP is set to ~s.~%~
                 We failed to load the custom QuickLisp. ~a~a"
		*quicklisp-fail* *not-strictly-necessary*)))
      (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" *home*)))
	(if (probe-file quicklisp-init)
	    (block nil
	      (handler-case
		  (with-unicode-files ()
		    (load quicklisp-init))
		(error (c)
		  (print c)
		  (format t "~&/~v,,,va~%~
                             We failed to load QuickLisp from ~s.~%~a~%~a~
                             ~&\\~v,,,va~%"
			  40 #\- #\-
			  quicklisp-init
			  *quicklisp-fail*
			  *not-strictly-necessary*
			  40 #\- #\-)
		  (return nil))))
	    (progn
	      (format t "QuickLisp was not found at ~s.~%~a" quicklisp-init
		      *not-strictly-necessary*)
	      (install-quicklisp))))))
