;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Global Variables;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Map lists
(defvar *mapx* '0)
(defvar *mapy* '0)

;;;Max values for map size
(defvar *xmax* 10)
(defvar *ymax* 10)

;;;Input error flag
(defvar *error* 0)

;;:List for items in the map
(defvar *itemlist* '())
(setf *itemlist* nil)

;;;Number of items
(defvar *itemnumber* 0)
(setf *itemnumber* 0)

;;;List for possible items
(defvar *items* '())
(setf *items* nil)
(push "Potion" *items*)
(push "Key" *items*)
(push "Sword" *items*)

;;;Generate item coordinates
(defun createitems (count)
  (setf *itemnumber* count)
  (dotimes (n count)
    (setf *itemlist*
	  (push (list (random 10) (random 10)) *itemlist*))
    )
  )

;;;Set random items to coordinates
(defun setitems ()
  (let ((i 0) (l (list-length *itemlist*)) )
    (loop while (< i l) do
	 ;Append random item to itemlist
	 (setf *itemlist* (append *itemlist* (list (nth (random (list-length *items*)) *items*))))
	 ;i++
	 (incf i)
	 )
    )
  )

;;;Get item for point in map if exists
(defun iteminpoint (x y)
  ;Initialize iteration variable
  (let ((i 0))
    ;Number of loops: number of items
    (dotimes (n *itemnumber*)
      ;Is (x y) in current iteration's element
      (if (equal (list x y) (nth i *itemlist*))
	  (return-from iteminpoint (nth (+ i *itemnumber*) *itemlist*))
	  )
      (incf i)
      )
    )
  )
    

;;;Is there an item in current location?
(defun pitemhere ()
  (member (list *mapx* *mapy*) *itemlist* :test 'equal)
  )

;;;Symbol to string
(defun lengthreturn (value)
  (length (string value))
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
	((peqcoords x y 1 3) (setf location 'Road))
	((peqcoords x y 1 4) (setf location 'Road))

	((peqcoords x y 2 0) (setf location 'Road))
	((peqcoords x y 2 1) (setf location 'Road))
	((peqcoords x y 2 2) (setf location 'Road))
	((peqcoords x y 2 3) (setf location 'Road))
	((peqcoords x y 2 4) (setf location 'Road))

	((peqcoords x y 3 0) (setf location 'Cave))
	((peqcoords x y 3 1) (setf location 'Cave))
	((peqcoords x y 3 2) (setf location 'Woods))
	((peqcoords x y 3 3) (setf location 'Road))

	((peqcoords x y 4 0) (setf location 'Cave))
	((peqcoords x y 4 1) (setf location 'Cave))
	((peqcoords x y 4 2) (setf location 'Cave))
	((peqcoords x y 4 3) (setf location 'Road))

	
	((peqcoords x y 5 0) (setf location 'Cave))
	((peqcoords x y 5 1) (setf location 'Cave))
	
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
      (setf *error* 1))
  )

;;;Go left
(defun goleft ()
  (if (pcangoleft)
      (decf *mapx*)
      (setf *error* 1))
  )

;;;Go up
(defun goup ()
  (if (pcangoup)
      (incf *mapy*)
      (setf *error* 1))
  )

;;;Go down
(defun godown ()
  (if (pcangodown)
      (decf *mapy*)
      (setf *error* 1))
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
      (return-from whatup ""))
  )

;;;Peeker for down in the map
(defun whatdown ()
  (if (pcangodown)
      (coordtoloc *mapx* (- *mapy* 1))
      (return-from whatdown ""))
  )

;;;Peeker for right in the map
(defun whatright ()
  (if (pcangoright)
      (coordtoloc (+ *mapx* 1) *mapy*)
      (return-from whatright ""))
  )

;;;Peeker for left in the map
(defun whatleft ()
  (if (pcangoleft)
      (coordtoloc (- *mapx* 1) *mapy*)
      (return-from whatleft ""))
  )

;;;Draw directions for current location
(defun drawdirections ()
  (if (pcangoup)
      (format t "                  ^  ~A" (whatup))
      (format t "                       "))
  (terpri)

  ;"1"-line in map screen
  (dotimes (n 18)
    (format t " ")
    )
  (format t "1")

  (terpri)

  ;"left"-portion in map screen
  
  (if (pcangoleft) (format t "< ") (format t "  "))

  (format t "~A" (whatleft))
  
  (dotimes (n (- 13 (lengthreturn (whatleft))))
    (format t " ")
    )
  
  (format t "2  @  3")
  
  (if (pcangoright)
      (format t "    >  ~A" (whatright))
      (format t "         "))
  (terpri)
  (format t "                  4")
  (terpri)
  (if (pcangodown)
      (format t "                  v  ~A" (whatdown))
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

;;;Reset XP points of a character
(defun resetxp (character)
  (setf (fourth character) '0)
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
  (format t "~A, ~A ~A with ~D XP. "
	  (getname character) (getrank character)
	  (getclass character) (getxp character))
  (terpri)
  (if (not (getinventory character))
      (format t "You are carrying: ~A" (getinventory character))
      (format t "You are not carrying anything."))
  (terpri)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Monster functions;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Random monster
(defun getmonster (character)
  (let ((value (random 100)))
    (cond ((> value 92)
	   (losehealth character 15)
	   (return-from getmonster "Legendary")
	   )
	  ((> value 80)
	   (losehealth character 10)
	   (return-from getmonster "Rare")
	   )
	  ((> value 60)
	   (losehealth character 5)
	   (return-from getmonster "Magic")
	   )
	  (t (return-from getmonster "No monster")))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Game Main function
(defun game (character)
  (setf *mapx* 0)
  (setf *mapy* 0)
  (resetxp character)
  (createitems 11)
  (setitems)
  
  (let ((input 1))
    ;Game loop starts
    (loop while (> input '0) do
	 (setf *error* 0)
         (format t "**************************************")
         (terpri)
	 ;Show the status of the character
	 (present character)
	 (terpri)
         (terpri)
	 ;Print current location
	 (format t "Location: ~A" (wherenow))
	 (terpri)
	 (format t "Items: ~A" (iteminpoint *mapx* *mapy*))
	 (terpri)
	 (format t "Monsters: ~A" (getmonster character))
	 (terpri)
	 (healthbar character)
	 (terpri)
	 (terpri)
	 ;Draw what is around you
	 (drawdirections)
	 (terpri)
	 (format t
		 "Enter number:")
         (terpri)
	 (setf input (read))

	 (cond ((eql input 1) (goup))
	       ((eql input 2) (goleft))
	       ((eql input 3) (goright))
	       ((eql input 4) (godown))
	       ((eql input 0) (format t "Game over."))
	       (t (setf *error* 1))) 

	 (if (and (eql *error* 0) (not (eql input 0)))
	     (addxp character 1)
	     )

	 (terpri)
	 
	 ) ;Game loop ends

    (format t "XP result: ~D" (getxp character))
    )
  )
