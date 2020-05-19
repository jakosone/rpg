;;;Map lists
(defvar *mapx* '0)
(defvar *mapy* '0)

;;;Get map location
(defun getlocation ()
  (cond ((pcoordinates 0 0) (return-from getlocation 'Home))
        ((pcoordinates 0 1) (return-from getlocation 'Woods))
	((pcoordinates 1 0) (return-from getlocation 'Woods))
	(t (return-from getlocation 'Unknown))
	)
  )
	
;;;Coordinates
(defun pcoordinates (x y)
  (and (eql *mapx* x) (eql *mapy* y))
  )

;;;X
(defun gox ()
  (incf *mapx*)
  )

;;;Y
(defun goy ()
  (incf *mapy*)
  )
	
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
  (dotimes (n (truncate (/ (gethealth character) 2)))
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

;;;Decrease the health of a character
(defun losehealth (character health)
  (setf (third character) (- (third character) health))
  )

;;;Get level for a character
(defun getrank (character)
  (cond ((< (getxp character) '7) (return-from getrank 'Novice))
	((< (getxp character) '10) (return-from getrank 'Warrior))
	(t (return-from getrank 'Unknown))
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
  (format t "You are ~A, a ~A ~A with ~D XP. "
	  (getname character) (getrank character)
	  (getclass character) (getxp character))
  (terpri)
  (format t "You are carrying: ~A" (getinventory character))
  (terpri)
  (terpri)
  (format t "Health:")
  (terpri)
  (healthbar character)
  )

;;;Game main function
(defun game (character)
  (let ((input 1))
    (loop while (> input '0) do
	 (present character)
	 (terpri)
         (terpri)
	 (format t "Location: (~D,~D)" *mapx* *mapy*)
	 (terpri)
	 (format t "Enter number:")
	 (setf input (read))
	 )
    
    )
  )


