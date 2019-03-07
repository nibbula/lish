;;
;; fully-loaded.lisp - Load shell commands before dumping Lish.
;;

(let ((*load-verbose* nil))
  (loop :for s :in '("pager" "puca" "char-picker" "pick-list" "tree-viewer"
		     "view-html")
       :do (asdf:load-system s :verbose nil))
  (loop :for s
     :in (mapcar (lambda (_)
		   (dlib:remove-prefix
		    (dlib:remove-suffix _ ".asd") "../los/"))
		 (remove-if (lambda (_)
			      (not (equal (nos:path-to-absolute _)
					  (namestring (truename _)))))
			    (glob:glob "../los/*.asd")))
       :do (asdf:load-system s :verbose nil)))

;; EOF
