;;;Compile the system with:
;;;(asdf:load-system :rpg2)

(asdf:defsystem #:rpg2
  :components ((:file "variables")
	       (:file "functions"
		      :depends-on ("variables"))
	       (:file "methods")
	       (:file "UI")
	       (:file "rpg2")))
