;;;Compile the system with:
;;;(asdf:load-system :rpg2)

(asdf:defsystem #:rpg2
  :components ((:file "variables")
	       (:file "functions")
	       (:file "methods")
	       (:file "player")
	       (:file "UI")
	       (:file "rpg2")))
