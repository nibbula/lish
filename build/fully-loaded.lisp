;;;
;;; fully-loaded.lisp - Load shell commands before dumping Lish.
;;;

(let ((*load-verbose* build-init:*build-verbose*))
  (push (truename "../los") asdf:*central-registry*)
  (push (truename "../image/") asdf:*central-registry*)
  (loop :for s :in '("pager" "puca" "char-picker" "pick-list" "tree-viewer"
		     #+linux "view-html" ;; @@@ cl+ssl fails on mac & windows
		     "view-table" "print-table" "view-tree" "view-image"
		     "view-org" "view-lisp" "dired"
		     )
       ;; :do (asdf:load-system s :verbose nil))
     :do (ql:quickload s :verbose build-init:*build-verbose*))

  (let ((systems
	 (mapcar (dlib:_
		  (dlib:keywordify
		   (dlib:remove-prefix
		    (dlib:remove-suffix dlib:_ ".asd") "../los/")))
		 (remove-if (dlib:_
			      (not (equal (nos:path-to-absolute dlib:_)
					  (namestring (truename dlib:_)))))
			    (glob:glob "../los/*.asd")))))
    ;; @@@ stupid work around
    (setf systems (delete :unzip systems))
    (loop :for s :in systems
       ;; :do (asdf:load-system s :verbose nil)))
       :do (ql:quickload s :verbose build-init:*build-verbose*)))

  ;; view-image is useless if it can't view any image types.
  (dlib:symbol-call :image :load-known-formats)

  ;; Make sure the magic backend is loaded.
  (magic:ensure-database))

;; EOF
