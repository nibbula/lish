;;;
;;; build-deinit.lisp - Undo any initialization we should for the built Lish.
;;;

(in-package :build-init)

;; We turn this back off. If you want function arguments for your stuff,
;; I'm sorry, but it's on you to turn it back on, or have had it on in the first
;; place. This we don't mess with the normal traditional way.
(when *saved-debug-quality*
  (proclaim `(optimize (debug ,*saved-debug-quality*))))

(delete-package :build-init)

;; End
