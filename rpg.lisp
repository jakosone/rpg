;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Global Variables;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Map lists
(defvar *mapx* '0)
(defvar *mapy* '0)

;;;Max values for map size
(defvar *xmax* 10)
(defvar *ymax* 10)

;;:List for items in the map
(defvar *itemlist* '())
(setf *itemlist* nil)


;;;Put items in coordinates
(defun createitems (count)
  (dotimes (n count)
    (setf *itemlist*
	  (push (list (random 10) (random 10)) *itemlist*))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Map Functions;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Get map location
(defun coordtoloc (x y)
  (let ((location nil))
    (cond ((peqcoords x y 0 0) (setf location 'Home))
        ((peqcoords x y 0 1) (setf location 'Woods))
        ((peqcoords x y 0 2) (setf location 'Field))
	((peqcoords x y 0 3) (setf location 'Cottage))
	((peqcoords x y 0 4) (setf location 'Road))

	((peqcoords x y 1 0) (setf location 'Woods))
	((peqcoords x y 1 1) (setf location 'Woods))
	((peqcoords x y 1 2) (setf location 'Woods))

	((peqcoords x y 2 0) (setf location 'Road))
	
	(t (setf location 'Unknown))
	)
    (return-from coordtoloc location)
  )
  )

;;;Predicate - are x=a and y=b?
(defun peqcoords (x y a b)
  (and (eql x a) (eql y b))
  )

;;;Go right
(defun goright ()
  (if (pcangoright)
      (incf *mapx*)
      (print "Error"))
  )

;;;Go left
(defun goleft ()
  (if (pcangoleft)
      (decf *mapx*)
      (print "Error"))
  )

;;;Go up
(defun goup ()
  (if (pcangoup)
      (incf *mapy*)
      (print "Error"))
  )

;;;Go down
(defun godown ()
  (if (pcangodown)
      (decf *mapy*)
      (print "Error"))
  )

;;;Predicate - can you go up?
(defun pcangoup ()
  (< *mapy* *ymax*)
  )

;;;Predicate - can you go down?
(defun pcangodown ()
  (> *mapy* '0)
  )

;;;Predicate - can you go right?
(defun pcangoright ()
  (< *mapx* *xmax*)
  )

;;;Predicate - can you go left?
(defun pcangoleft ()
  (> *mapx* '0)
  )

;;;Where are you now?
(defun wherenow ()
  (coordtoloc *mapx* *mapy*)
  )

;;;Peeker for up in the map
(defun whatup ()
  (if (pcangoup)
      (coordtoloc *mapx* (+ *mapy* 1))
      (format t "Cannot go there."))
  )

;;;Peeker for down in the map
(defun whatdown ()
  (if (pcangodown)
      (coordtoloc *mapx* (- *mapy* 1))
      (format t "Cannot go there."))
  )

;;;Peeker for right in the map
(defun whatright ()
  (if (pcangoright)
      (coordtoloc (+ *mapx* 1) *mapy*)
      (format t "Cannot go there."))
  )

;;;Peeker for left in the map
(defun whatleft ()
  (if (pcangoleft)
      (coordtoloc (- *mapx* 1) *mapy*)
      (format t "Cannot go there."))
  )

;;;Draw directions for current location
(defun drawdirections ()
  (if (pcangoup)
      (format t "      ^  ~A" (whatup))
      (format t "           "))
  (terpri)
  (if (pcangoleft)
      (format t "        <  ~A" (whatleft))
      (format t "             "))

  (if (pcangoright)
      (format t "           >  ~A" (whatright))
      (format t "                "))
  (terpri)
  (if (pcangodown)
      (format t "     v  ~A" (whatdown))
      (format t ""))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Creators, getters, setters;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Creating a character (list)
(defun newcharacter (name class)
  (list name class '100 '0 (newinventory))
  )

(defun makechar ()
  (let ((name nil) (class nil))
    (format t "Enter name:")
    (setf name (read))
    (format t "Enter class:")
    (setf class (read))
    (newcharacter name class)
    )
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

;;;Game Main function
(defun game (character)
  (setf *mapx* 0)
  (setf *mapy* 0)
  (createitems 7)
  (let ((input 1))
    (loop while (> input '0) do
	 (present character)
	 (terpri)
         (terpri)
	 (format t "Location: ~A" (wherenow))
	 (terpri)
	 (format t
		 "Enter number (1 up, 2 left, 3 right, 4 down:")
	 (setf input (read))

	 (cond ((eql input 1) (goup))
	       ((eql input 2) (goleft))
	       ((eql input 3) (goright))
	       ((eql input 4) (godown))
	       (t (format t "Error."))) 

	 (if (member input '(1 2 3 4))
	     (addxp character 1)
	 )
    
    )
    )
  )
