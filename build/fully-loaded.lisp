;;;
;;; fully-loaded.lisp - Load shell commands before dumping Lish.
;;;

(let ((*load-verbose* nil))
  (push (truename "../los") asdf:*central-registry*)
  (push (truename "../image/") asdf:*central-registry*)
  (loop :for s :in '("pager" "puca" "char-picker" "pick-list" "tree-viewer"
		     #+linux "view-html" ;; @@@ cl+ssl fails on mac & windows
		     )
       ;; :do (asdf:load-system s :verbose nil))
     :do (ql:quickload s :verbose nil))

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
       :do (ql:quickload s :verbose nil))))

;; EOF
