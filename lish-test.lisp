;;
;; lish-test.lisp - Tests for Lish
;;

;; @@@ These are so woefully incomplete.

(defpackage :lish-test
  (:documentation "Tests for Lish")
  (:use :cl :test :lish :dlib)
  (:export
   #:run
   ))
(in-package :lish-test)

(declaim (optimize (speed 0) (safety 3) (debug 3) (space 0)
		   (compilation-speed 0)))

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
     (and (symbolp b)
	  (equal (symbol-name a) (symbol-name b))))
    (t
     (equalp a b))))

(defun vuvu (str l-args)
  (let ((aa (command-to-lisp-args (command-arglist (get-command str)))))
    (format t "~w ~{~w ~}~%~w~%~%" str (command-arglist (get-command str)) aa)
    (sort-of-equal aa l-args)))

;; You probably have to say: (with-package :lish (test-stla))
;; Unfortunately this may fail if not updated to reflect builtin changes.

#| I changed it so everything is keywords!!!
(deftests (shell-to-lisp-args-1 :doc "Test COMMAND-TO-LISP-ARGS.")
  (vuvu "cd"      '(&optional directory))
  (vuvu "pwd"     '())
  (vuvu "pushd"   '(&optional directory))
  (vuvu "popd"    '(&optional number))
  (vuvu "dirs"    '())
  (vuvu "suspend" '())
  (vuvu "history" '(&key clear write read append read-not-read filename
		    show-times delete))
  ;; (vuvu ":"       '(&rest args))
  (vuvu "echo" 	  '(&key no-newline args))
  (vuvu "help" 	  '(&optional subject))
  (vuvu "alias"   '(&key global edit name expansion))
  (vuvu "unalias" '(name))
  (vuvu "exit" 	  '(&rest values))
  (vuvu "source"  '(filename))
  (vuvu "debug"   '(&optional (state :toggle)))
  (vuvu "export"  '(&key remove name value))
  (vuvu "jobs" 	  '(&key long))
  (vuvu "kill" 	  '(&key list-signals (signal "term") pids))
  ;; (vuvu "format"  '(format-string &rest args))
  ;; (vuvu "read" 	  '(&key name prompt timeout editing))
  (vuvu "time" 	  '(&rest command))
  (vuvu "times"   '())
  (vuvu "umask"   '(&key print-command symbolic mask))
  (vuvu "ulimit"  '())
  (vuvu "wait" 	  '())
  (vuvu "exec" 	  '(&rest command-words))
  (vuvu "bind" 	  '(&key print-bindings print-readable-bindings query
		    remove-function-bindings	remove-key-binding key-sequence
		    function-name))
  (vuvu "hash" 	  '(&key rehash packages commands))
  (vuvu "type" 	  '(&key type-only path-only all names))
  ;; (format t "SUCCEED!~%")
  )
|#

(deftests (shell-to-lisp-args-1 :doc "Test COMMAND-TO-LISP-ARGS.")
  (vuvu "cd"      '(&key directory))
  (vuvu "pwd"     '())
  (vuvu "pushd"   '(&key directory))
  (vuvu "popd"    '(&key number))
  (vuvu "dirs"    '())
  (vuvu "suspend" '())
  (vuvu "history" '(&key clear write read append read-not-read filename
		    show-times delete))
  ;; (vuvu ":"       '(&rest args))
  (vuvu "echo" 	  '(&key no-newline args))
  (vuvu "help" 	  '(&key subject))
  (vuvu "alias"   '(&key global edit name expansion))
  (vuvu "unalias" '(&key name))
  (vuvu "exit" 	  '(&key values))
  (vuvu "source"  '(&key filename))
  (vuvu "debug"   '(&key (state :toggle state-supplied-p)))
  (vuvu "export"  '(&key remove edit name value))
  (vuvu "jobs" 	  '(&key long all))
  (vuvu "kill" 	  '(&key list-signals interactive (signal "term") pids))
  ;; (vuvu "format"  '(format-string &rest args))
  ;; (vuvu "read" 	  '(&key name prompt timeout editing))
  (vuvu "time" 	  '(&key command))
  (vuvu "times"   '())
  (vuvu "umask"   '(&key print-command symbolic mask))
  (vuvu "ulimit"  '())
  (vuvu "wait" 	  '())
  (vuvu "exec" 	  '(&key command-words))
  (vuvu "bind" 	  '(&key print-bindings print-readable-bindings query
		    remove-function-bindings	remove-key-binding key-sequence
		    function-name))
  (vuvu "hash" 	  '(&key rehash packages commands))
  (vuvu "type" 	  '(&key type-only path-only all names))
  ;; (format t "SUCCEED!~%")
  )

(defun o-vivi (str &rest args)
  (format t "~w ~{~w ~}~%~w~%~%" str args
	  (posix-to-lisp-args (get-command str) args)))

(defun vivi (str p-args l-args)
  (let ((aa (posix-to-lisp-args
	     (get-command str)
	     ;; (lish::expr-to-words (lish:shell-read p-args)))))
	     (lish::shell-expr-words (lish:shell-read p-args)))))
    ;;(format t "~w ~{~w ~}~%~w~%~%" str p-args aa)
    (format t "~w ~w~%~w~%~%" str p-args aa)
    (equalp aa l-args)))

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

(defcommand clop
  ((from string :repeating t #| :optional nil |#)
   (to string :optional nil))
  "Test argument conversion."
  (format t "from = ~s to = ~a~%" from to))

(defcommand snerp
  ((from string :repeating t :optional nil)
   (to string :optional nil))
  "Test argument conversion."
  (format t "from = ~s to = ~a~%" from to))

(deftests (posix-to-lisp-args-1 :doc "Test POSIX-TO-LISP-ARGS.")
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

  (vivi "alias" "" '(:expansion nil))
  (vivi "alias" "name" '(:name "name":expansion nil))
  (vivi "alias" "name expansion" '(:name "name" :expansion ("expansion")))
  (vivi "alias"
	"name expansion extra junk"
	'(:name "name" :expansion ("expansion" "extra" "junk")))

  (vivi "bind" "" '())
  (vivi "bind" "-p" '(:print-bindings t))
  (vivi "bind" "-P" '(:print-readable-bindings t))
  (vivi "bind" "-r foo" '(:remove-key-binding "foo"))
  (vivi "cd" "" '())
  (vivi "cd" "dir" '(:directory "dir"))
  (vivi "debug" "" '(:state :toggle))
  (vivi "debug" "on" '(:state t))
  (vivi "debug" "off" '(:state nil))
  ;; This is supposed to fail, since pecan isn't a boolean
;  (vivi "debug" '("pecan") '()) 
  (vivi "gurp"  "-i foo bar baz lemon"
	'(:pattern "foo" :files ("bar" "baz" "lemon") :invert t))
  (vivi "zurp"  "-s 3 chflags" '(:entry "chflags" :section "3"))
  (vivi "clop" "foo the bar mmkay"  '(:from ("foo" "the" "bar") :to "mmkay"))
  (vivi "clop" "foo bar"            '(:from ("foo") :to "bar"))
  (vivi "clop" "zurpy"	            '(:to "zurpy"))
  (vivi "snerp" "foo the bar mmkay" '(:from ("foo" "the" "bar") :to "mmkay"))
  (vivi "snerp" "foo bar"           '(:from ("foo") :to "bar"))
  ;; This should get a: "Missing mandatory agrument: to" error
  ;; (vivi "snerp" "zurpy"
  ;; 	'(:to "zurpy"))
)

;(with-dbug (lish::posix-to-lisp-args (lish::get-command "bind") '("-r" "foo")))
;(with-dbug (lish::posix-to-lisp-args (lish::get-command "find") '("--name" "txt$")))

;; @@@ This is an internal-ish way to test. Once we finish, we should test it
;; through a more external interface. Or perhaps this suggests that an external
;; interface to these would be useful.
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

(deftests (expand-braces-1 :doc "Test brace expansion")
  (equal (lish::expand-braces "hi") '("hi"))
  (equal (lish::expand-braces "hi there") '("hi there"))
  (equal (lish::expand-braces "fooba{r,z}") '("foobar" "foobaz"))
  (equal (lish::expand-braces "foo{bar,baz}") '("foobar" "foobaz"))
  (equal (lish::expand-braces "{f,g,l}oobar{1,2,3}")
	 '("foobar1" "foobar2" "foobar3" "goobar1" "goobar2" "goobar3" "loobar1"
	   "loobar2" "loobar3"))
  (equal (lish::expand-braces "foo{b,f}a{r,z}")
	 '("foobar" "foobaz" "foofar" "foofaz"))
  (equal (lish::expand-braces "foo{ba{r,t},ga}") '("foobar" "foobat" "fooga"))
  (equal (lish::expand-braces "{foo,bar}") '("foo" "bar"))
  (equal (lish::expand-braces "{a,b,c}") '("a" "b" "c"))
  "Malformed expressions"
  (equal (lish::expand-braces "}") '("}"))
  (equal (lish::expand-braces "{") '("{"))
  (equal (lish::expand-braces "{}") '("{}"))
  (equal (lish::expand-braces "{ }") '("{ }"))
  (equal (lish::expand-braces "{foo}") '("{foo}"))
  (equal (lish::expand-braces "fooba{r,z") '("fooba{r,z"))
  (equal (lish::expand-braces "foobar,z") '("foobar,z"))
  "Escaping"
  (equal (lish::expand-braces "fooba{r\\,z}") '("fooba{r\\,z}"))
  (equal (lish::expand-braces "_\\{a,b,c}_") '("_\\{a,b,c}_"))
  (equal (lish::expand-braces "\\_{a,b,c}_") '("\\_a_" "\\_b_" "\\_c_"))
  (equal (lish::expand-braces "\\{a,b,c,d,e}") '("\\{a,b,c,d,e}"))
  (equal (lish::expand-braces "{foo,bar,\\{it's,the,blimp}}")
	 '("foo}" "bar}" "\\{it's}" "the}" "blimp}"))
  (equal (lish::expand-braces "{foo\\,bar,\\{zerp\\},quux}")
	 '("foo\\,bar" "\\{zerp\\}" "quux"))
  )

(deftests (expand-brace-sequences-1 :doc "Test brace sequence expansion")
  (equal (lish::expand-braces "{1..5}") '("1" "2" "3" "4" "5"))
  (equal (lish::expand-braces "{1..10}")
			      '("1" "2" "3" "4" "5" "6" "7" "8" "9" "10"))
  (equal (lish::expand-braces "{01..10}")
			      '("01" "02" "03" "04" "05" "06" "07" "08" "09"
				"10"))
  (equal (lish::expand-braces "{1..10..2}") '("1" "3" "5" "7" "9"))
  (equal (lish::expand-braces "{1..010..2}") '("001" "003" "005" "007" "009"))
  (equal (lish::expand-braces "{01..110..10}")
	 '("001" "011" "021" "031" "041" "051" "061" "071" "081" "091" "101"))
  (equal (lish::expand-braces "{-5..5}")
	 '("-5" "-4" "-3" "-2" "-1" "0" "1" "2" "3" "4" "5"))
  (equal (lish::expand-braces "{-20..20..5}")
	 '("-20" "-15" "-10" "-5" "0" "5" "10" "15" "20"))
  )

(deftests (lish-all :doc "Test all the things!.")
    shell-to-lisp-args-1 posix-to-lisp-args-1 expand-variables-1 expand-braces-1
    expand-brace-sequences-1
    ;; expand-brace-sequences-2 expand-brace-sequences-3
    )

(defun run ()
  (run-group-name 'lish-all :verbose t))

;; EOF
