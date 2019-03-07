;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try to load ASDF, if it's not already loaded.

(when (not (find-package :asdf))
  (if (sf-getenv "LISH_ASDF")
      (handler-case
      	  (load (sf-getenv "LISH_ASDF"))
	(error (c)
	  (print c)
	  (fail "LISH_ASDF is set to ~s.~%~
                 We failed to load the custom ASDF. You'll have to figure it ~
out, or maybe just upgrade your Lisp to one that comes with a working ASDF ~
built-in?" (sf-getenv "LISH_ASDF"))))
      (handler-case
	  (require :asdf)
	(error (c)
	  (print c)
	  (fail "We failed to (require :asdf), which probably means ASDF is ~
missing. You can upgrade your Lisp to one that comes with ASDF built-in, or ~
you can set the environment variable LISH_ASDF to the file name of an ASDF ~
to load, and try again.")))))

(when (not (find-package :uiop))
  (fail "Your ASDF is so old it doesn't have UIOP. Please get a newer ASDF."))
