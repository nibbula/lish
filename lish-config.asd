;;;								-*- Lisp -*-
;;; lish-config.asd - System definition for lish-config
;;;

(defsystem lish-config
  :name               "lish-config"
  :description        "Configuration for Lish"
  :version            "0.1.0"
  :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
  :license            "GPL-3.0-only"
  :source-control     :git
  :long-description   "Configuration for Lish"
  :depends-on (:config)
  :components
  ((:file "lish-config")))
