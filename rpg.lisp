;;;Creating a character (list)
(defun newcharacter (name class)
  (list name class '0 (newinventory))
  )

;;;Getter for character name
(defun getname (character)
  (first character)
  )

;;;Getter for character class
(defun getclass (character)
  (second character)
  )

;;;Getter for character XP points
(defun getxp (character)
  (third character)
  )

;;;Getter for character inventory (list)
(defun getinventory (character)
  (fourth character)
  )

;;;Increase XP points of a character
(defun addxp (character points)
  (setf (third character) (+ (third character) points))
  )

;;;Creating an inventory
(defun newinventory ()
  (list ())
  )

;;;Adding an item in an inventory of a character
(defun additem (character item)
  (if (null (car (getinventory character)))
      (setf (fourth character) (list item))
      (setf (fourth character) (push item (fourth character)))
      )
  )
