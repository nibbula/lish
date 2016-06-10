;;
;; fully-loaded.lisp - load los before dumping lish
;;

(los)
(asdf:load-systems "pager" "puca" "char-picker" "pick-list" "tree-viewer"
		   "view-html")
(apply #'asdf:load-systems
       (mapcar (_ (remove-prefix (remove-suffix _ ".asd") "../los/"))
	       (glob:glob "../los/*.asd")))

;; EOF
