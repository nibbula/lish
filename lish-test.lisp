;;
;; lish-test.lisp - Tests for Lish
;;

(defpackage :lish-test
  (:documentation "Tests for Lish")
  (:use :cl :test :lish)
  (:export
   #:run
   ))
(in-package :lish-test)

(defun sort-of-equal (a b)
  "Like equalp, but ignoring symbol package differences."
  (typecase a
    (list
     (return-from sort-of-equal
       (not
	(loop :for aa :in a :for bb :in b
	   :unless (sort-of-equal aa bb)
	   :return t))))
    (symbol
     (equal (symbol-name a) (symbol-name b)))
    (t
     (equalp a b))))

(defun vuvu (str l-args)
  (let ((aa (command-to-lisp-args (command-arglist (get-command str)))))
    (format t "~w ~{~w ~}~%~w~%~%" str (command-arglist (get-command str)) aa)
    (assert (sort-of-equal aa l-args))))

;; You probably have to say: (with-package :lish (test-stla))
;; Unfortunately this may fail if not updated to reflect builtin changes.
(defun test-shell-to-lisp-args ()
  "Test COMMAND-TO-LISP-ARGS."
  (vuvu "cd"      '(&optional directory))
  (vuvu "pwd"     '())
  (vuvu "pushd"   '(&optional directory))
  (vuvu "popd"    '(&optional number))
  (vuvu "dirs"    '())
  (vuvu "suspend" '())
  (vuvu "history" '(&key clear write read append read-not-read filename
		    show-times delete))
  (vuvu ":"       '(&rest args))
  (vuvu "echo" 	  '(&key no-newline args))
  (vuvu "help" 	  '(&optional subject))
  (vuvu "alias"   '(&optional name expansion))
  (vuvu "unalias" '(name))
  (vuvu "exit" 	  '(&rest values))
  (vuvu "source"  '(filename))
  (vuvu "debug"   '(&optional (state :toggle)))
  (vuvu "export"  '(&optional name value))
  (vuvu "jobs" 	  '(&key long))
  (vuvu "kill" 	  '(&key list-signals (signal 15) pids))
  (vuvu "format"  '(format-string &rest args))
  (vuvu "read" 	  '(&key name prompt timeout editing))
  (vuvu "time" 	  '(&rest command))
  (vuvu "times"   '())
  (vuvu "umask"   '(&key print-command symbolic mask))
  (vuvu "ulimit"  '())
  (vuvu "wait" 	  '())
  (vuvu "exec" 	  '(&rest command-words))
  (vuvu "bind" 	  '(&key print-bindings print-readable-bindings query
		    remove-function-bindings	remove-key-binding key-sequence
		    function-name))
  (vuvu "hash" 	  '(&key rehash commands))
  (vuvu "type" 	  '(&key type-only path-only all names))
  (format t "SUCCEED!~%"))

(defun o-vivi (str &rest args)
  (format t "~w ~{~w ~}~%~w~%~%" str args
	  (posix-to-lisp-args (get-command str) args)))

(defun vivi (str p-args l-args)
  (let ((aa (posix-to-lisp-args
	     (get-command str)
	     (lish::expr-to-words (lish:shell-read p-args)))))
    (format t "~w ~{~w ~}~%~w~%~%" str p-args aa)
    (assert (equalp aa l-args))))

(defcommand tata
    (("one" boolean :short-arg #\1)
     ("two" string  :short-arg #\2))
  "Test argument conversion."
  (format t "one = ~s two = ~s~%" one two))

(defcommand gurp
    (("pattern" string   :optional nil)
     ("files"   pathname :repeating t)
     ("invert"  boolean  :short-arg #\i))
  "Test argument conversion."
  (format t "pattern = ~s files = ~s invert = ~s~%" pattern files invert))

(defcommand zurp
    (("section" string :short-arg #\s)
     ("entry"   string :optional t))
  "Test argument conversion."
  (format t "entry = ~s section = ~s~%" entry section))

(defun test-posix-to-lisp-args ()
  ;; (vivi ":" '() '())
  ;; (vivi ":"
  ;; 	'("(format t \"egg~a~%\" (lisp-implementation-type))")
  ;; 	'("(format t \"egg~a~%\" (lisp-implementation-type))"))
  ;; (vivi ":"
  ;; 	'("blah" "blah" "blah" "etc" "...")
  ;; 	'("blah" "blah" "blah" "etc" "..."))
  ;; This was before adding the -g option:
  ;; (vivi "alias" '("name") '("name"))
  ;; (vivi "alias" '("name" "expansion") '("name" "expansion"))
  ;; (vivi "alias"
  ;; 	'("name" "expansion" "extra" "junk")
  ;; 	'("name" "expansion"))

  (vivi "alias" "" '())
  (vivi "alias" "name" '(:name "name"))
  (vivi "alias" "name expansion" '(:name "name" :expansion "expansion"))
  (vivi "alias"
  	"name expansion extra junk" ;; Is this really right?
	'(:name "name" :expansion "expansion"))

  (vivi "bind" "" '())
  (vivi "bind" "-p" '(:print-bindings t))
  (vivi "bind" "-P" '(:print-readable-bindings t))
  (vivi "bind" "-r foo" '(:remove-key-binding "foo"))
  (vivi "cd" "" '())
  (vivi "cd" "dir" '("dir"))
  (vivi "debug" "" '())
  (vivi "debug" "on" '(t))
  (vivi "debug" "off" '(nil))
  ;; This is supposed to fail, since pecan isn't a boolean
;  (vivi "debug" '("pecan") '()) 
  (vivi "gurp"  "-i foo bar baz lemon"
	'("foo" :files ("bar" "baz" "lemon") :invert t))
  (vivi "zurp"  "-s 3 chflags" '(:entry "chflags" :section "3"))
)

;(with-dbug (lish::posix-to-lisp-args (lish::get-command "bind") '("-r" "foo")))
;(with-dbug (lish::posix-to-lisp-args (lish::get-command "find") '("--name" "txt$")))

;; @@@ This is an internal-ish way to test. Once we finish, we should test it
;; through a more external interface.
(deftests (expand-variables-1 :doc "Test variable expansion")
  :setup
  (progn
    (setf (nos:environment-variable "bar") nil
	  (nos:environment-variable "foo") "Yow$a!"))
  :takedown
  (setf (nos:environment-variable "foo") nil)
  (equal (lish::expand-variables "") "")
  (equal (lish::expand-variables "foo") "foo")
  (equal (lish::expand-variables "foo\\$bar") "foo\\$bar")
  (equal (lish::expand-variables "foobar\\") "foobar\\")
  (equal (lish::expand-variables "\\foo\\bar\\") "\\foo\\bar\\")
  (equal (lish::expand-variables "\\foo\\$bar\\") "\\foo\\$bar\\")
  (equal (lish::expand-variables "\\$foobar\\$") "\\$foobar\\$")
  (equal (lish::expand-variables "foo$bar") "foo")
  (equal (lish::expand-variables "$bar") "")
  (equal (lish::expand-variables "foo$bar.baz") "foo.baz")
  (equal (lish::expand-variables "foo$bar_baz") "foo")
  (equal (lish::expand-variables "foo$bar_baz.zeep") "foo.zeep")
  (equal (lish::expand-variables "$foo") "Yow$a!")
  (equal (lish::expand-variables "-$foo-") "-Yow$a!-")
  (equal (lish::expand-variables "\\$foo") "\\$foo")
  (equal (lish::expand-variables "-$foo$bar-") "-Yow$a!-")
  (equal (lish::expand-variables "$}") "$}")
  )

(deftests (lish-all :doc "All the tests.")
  expand-variables-1)
  
(defun run ()
  (test-shell-to-lisp-args)
  (test-posix-to-lisp-args)
  (run-group-name 'lish-all :verbose t))

;; EOF
