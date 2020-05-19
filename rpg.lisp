;;;Creating a character (list)
(defun newcharacter (name class)
  (list name class '100 '0 (newinventory))
  )

;;;Getter for character name
(defun getname (character)
  (first character)
  )

;;;Getter for character class
(defun getclass (character)
  (second character)
  )

;;;Getter for character health
(defun gethealth (character)
  (third character)
  )

;;;Present health bar for a character
(defun healthbar (character)
  (format t "[")
  (dotimes (n (/ (gethealth character) 2))
	   (format t "|"))
  (format t "] ~D %" (gethealth character))
  )

;;;Getter for character XP points
(defun getxp (character)
  (fourth character)
  )

;;;Getter for character inventory (list)
(defun getinventory (character)
  (fifth character)
  )

;;;Increase XP points of a character
(defun addxp (character points)
  (setf (fourth character) (+ (fourth character) points))
  )

;;;Get level for a character
(defun getlevel (character)
  (cond ((< (getxp character) '7) (return-from getlevel 'Novice))
	((< (getxp character) '10) (return-from getlevel 'Warrior))
	(t (return-from getlevel 'Unknown))
	)
  )

;;;Creating an inventory
(defun newinventory ()
  (list ())
  )

;;;Adding an item in an inventory of a character
(defun additem (character item)
  (if (null (car (getinventory character)))
      (setf (fifth character) (list item))
      (setf (fifth character) (push item (fifth character)))
      )
  )

;;;Present character as text
(defun present (character)
  (format t "You are ~A, a ~A with ~D XP. "
	  (getname character) (getclass character) (getxp character))
  (terpri)
  (format t "You are carrying: ~A" (getinventory character))
  (terpri)
  (terpri)
  (format t "Health:")
  (terpri)
  (healthbar character)
  )

