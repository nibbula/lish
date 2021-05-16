;;;								-*- Lisp -*-
;;; lish-stats.asd -- System definition for lish-stats
;;;

(defsystem lish-stats
    :name               "lish-stats"
    :description        "Command statistics for Lish."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description
    "Keeps track of when and how many times a command was invoked from Lish."
    :depends-on (:dlib :dlib-misc :lish)
    :components
    ((:file "lish-stats")))
