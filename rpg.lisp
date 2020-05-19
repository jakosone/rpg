;Creating a character list
(defun newcharacter (name class)
  (list name class '0)
  )

;Getter for character name
(defun getname (character)
  (first character)
  )

;Getter for character class
(defun getclass (character)
  (second character)
  )
