;;;
;;; lish-config.lisp - Configuration for Lish
;;;

(defpackage :lish-config
  (:documentation "Configuration for Lish")
  (:use :cl :config)
  (:export))
(in-package :lish-config)

(defconfiguration
  (
   ;; Actually I don't need this now.
   ;; (use-threads boolean-feature
   ;;  "True if this implementation has usable threads."
   ;;  (feature-expression '(or (and sbcl sb-thread)
   ;; 			     (and ecl threads)
   ;; 			     (and clisp mt)
   ;; 			     mezzano
   ;; 			     lispworks
   ;; 			     (and allegro multiprocessing))))
   (optimization-settings list
    "Default optimization settings for each file/compilation unit."
    ;; If we don't have at least debug 2, then most compilers won't save
    ;; the function arguments, which wrecks discoverability among other things.
    `((debug 2)))))

(configure)

;; End
