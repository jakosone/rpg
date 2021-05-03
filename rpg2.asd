;;;Compile the system with:
;;;(asdf:load-system :rpg2)


(require "asdf")
(asdf:defsystem "rpg2"
  :description "RPG2 - a small game"
  :components ((:file "variables")
	       (:file "functions")
	       (:file "methods")
	       (:file "player")
	       (:file "UI")
	       (:file "rpg2"))
  :build-operation "program-op"
  :build-pathname "rpg-two"
  :entry-point "rpg2::rpg")
