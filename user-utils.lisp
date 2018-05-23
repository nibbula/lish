
(in-package :lish)

(defun getenv (name)
  "Get the environment variable NAME"
  (nos:environment-variable name))

(defun setenv (name value)
  "Set environment variable NAME to VALUE.
If VALUE is NIL then the environment variable NAME is removed.
If VALUE is non-NIL, princ-to-string is usedd to convert VALUE to a string."
  (if value
      (setf (nos:environment-variable name) (princ-to-string value))
      (setf (nos:environment-variable name) nil)))

(defmacro with-cwd (directory &body body)
  "Changes the current working directory to DIRECTORY,
and evaluates BODY. Uses unwind-protect to ensure that
the previous working directory is restored on exit

Example:

  (with-cwd \"/var/log\"
    (! \"ls\"))
"
  (let ((old-directory (gensym)))
    `(let ((,old-directory (nos:current-directory)))
       (unwind-protect
            (progn
              (nos:change-directory ,directory)
              (setf (nos:environment-variable "PWD") (nos:current-directory))
              ,@body)
         (nos:change-directory ,old-directory)
         (setf (nos:environment-variable "PWD") (nos:current-directory))))))

(defmacro with-env (bindings &body body)
  "Evaluates BODY inside an unwind-protected progn, setting environment variables
in BINDINGS and restoring the environment afterwards.

BINDINGS should be a list of lists, similar to LET

Example:

  (with-env ((\"TERM\" \"xterm\")
             (\"PATH\" \"/bin/\"))
    (! \"echo $TERM\"))

"
  ;; Check that BINDINGS has the expected structure
  (unless (listp bindings) (error "BINDINGS must be a list of lists of the form ((env1 value1) (env2 value2))"))

  (dolist (binding bindings)
    (unless (listp binding) (error "BINDINGS must contain lists (not ~a)" binding))
    (unless (= 2 (length binding))
      (error "BINDINGS must contain lists of length 2 (env value)")))
  
  ;; Generate symbols to bind to old environment values
  (let ((old-bindings (loop for i below (length bindings) collect (gensym))))
    
    ;; Save current bindings so they can be restored
    `(let ,(loop
              for binding in bindings
              for old-binding in old-bindings
              collect `(,old-binding (nos:environment-variable ,(first binding))))
       (unwind-protect
            (progn
              ;; Set new environment
              ,@(loop for binding in bindings
                   collect `(setf (nos:environment-variable ,(first binding)) ,(second binding)))
              ,@body)
         ;; Restore environment
         ,@(loop
              for binding in bindings
              for old-binding in old-bindings
              collect `(setf (nos:environment-variable ,(first binding)) ,old-binding))))))


                  
