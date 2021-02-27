;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Global Variables;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;Live-evaluation input variable
(defvar *input* T)

;;;Map lists
(defvar *mapx* '0)
(defvar *mapy* '0)

;;;Max values for map size
(defvar *xmax* 5)
(defvar *ymax* 9)

;;;Input error flag
(defvar *error* 0)

;;:List for items in the map
(defvar *itemlist* nil)
(setf *itemlist* nil)

;;;Number of items
(defvar *itemnumber* 0)
(setf *itemnumber* 0)

;;;List for possible items
(defvar *items* nil)
(setf *items* nil)
(push "Potion" *items*)
(push "Key" *items*)
(push "Sword" *items*)

;;;List for magic item (weapon) categories
(defvar *itemcat* '())
(setf *itemcat* nil)
(push 'Sword *itemcat*)
(push 'Knife *itemcat*)
(push 'Mace *itemcat*)
(push 'Wand *itemcat*)
(push 'Staff *itemcat*)

;;;List for magic item (weapon) suffixes
(defvar *itemsuffix* '())
(setf *itemsuffix* nil)
(push 'Healing *itemsuffix*)
(push 'Teleportation *itemsuffix*)
(push 'Security *itemsuffix*)
(push 'Protection *itemsuffix*)
(push 'Regeneration *itemsuffix*)

;;;List of magic item types
(defvar *itemrarity* '())
(setf *itemrarity* nil)
(push "()" *itemrarity*)
(push "(M)" *itemrarity*)
(push "(R)" *itemrarity*)
(push "(L)" *itemrarity*)
(push "(A)" *itemrarity*)

;;;List for monster types
(defvar *monstercat* '())
(setf *monstercat* (list 'Dragon 'Demon 'Zombie 'Angel))

;;;Get random element from a list
(defun getrand (list)
  (nth (random (list-length list)) list)
  )

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


(defun item-here-p ()
  "Is here an item in current location?"
  (member (list *mapx* *mapy*) *itemlist* :test 'equal)
  )

(defun itemhere ()
  "Returns the item in current location"
  (iteminpoint *mapx* *mapy*)
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
    (cond ((eq-coords-p x y 0 0) (setf location 'Home))
        ((eq-coords-p x y 0 1) (setf location 'Woods))
        ((eq-coords-p x y 0 2) (setf location 'Field))
	((eq-coords-p x y 0 3) (setf location 'Cottage))
	((eq-coords-p x y 0 4) (setf location 'Road))

	((eq-coords-p x y 1 0) (setf location 'Woods))
	((eq-coords-p x y 1 1) (setf location 'Woods))
	((eq-coords-p x y 1 2) (setf location 'Woods))
	((eq-coords-p x y 1 3) (setf location 'Road))
	((eq-coords-p x y 1 4) (setf location 'Road))

	((eq-coords-p x y 2 0) (setf location 'Road))
	((eq-coords-p x y 2 1) (setf location 'Road))
	((eq-coords-p x y 2 2) (setf location 'Road))
	((eq-coords-p x y 2 3) (setf location 'Road))
	((eq-coords-p x y 2 4) (setf location 'Road))

	((eq-coords-p x y 3 0) (setf location 'Cave))
	((eq-coords-p x y 3 1) (setf location 'Cave))
	((eq-coords-p x y 3 2) (setf location 'Woods))
	((eq-coords-p x y 3 3) (setf location 'Road))

	((eq-coords-p x y 4 0) (setf location 'Cave))
	((eq-coords-p x y 4 1) (setf location 'Cave))
	((eq-coords-p x y 4 2) (setf location 'Cave))
	((eq-coords-p x y 4 3) (setf location 'Road))
	((eq-coords-p x y 4 4) (setf location 'Road))
	((eq-coords-p x y 4 5) (setf location 'Woods))

	
	((eq-coords-p x y 5 0) (setf location 'Cave))
	((eq-coords-p x y 5 1) (setf location 'Cave))
	((eq-coords-p x y 5 2) (setf location 'Cave))
	((eq-coords-p x y 5 3) (setf location 'Cave))
	((eq-coords-p x y 5 4) (setf location 'Road))
	((eq-coords-p x y 5 5) (setf location 'Woods))
	((eq-coords-p x y 5 6) (setf location 'Castle))
	((eq-coords-p x y 5 7) (setf location 'Castle))
	((eq-coords-p x y 5 8) (setf location 'Castle))
	((eq-coords-p x y 5 9) (setf location 'Castle))
	
	
	(t (setf location 'Unknown))
	)
    (return-from coordtoloc location)
  )
  )

(defun eq-coords-p (x y a b)
  "Returns true if x=a AND y=b"
  (and (eql x a) (eql y b))
  )

;;;Go right
(defun goright ()
  "Advance to right by one on map"
  (if (can-go-right-p)
      (incf *mapx*)
      (setf *error* 1)) ;Raise error flag
  )

;;;Go left
(defun goleft ()
  "Advance to left by one on map"
  (if (can-go-left-p)
      (decf *mapx*)
      (setf *error* 1)) ;Raise error flag
  )

(defun goup ()
  "Advance up by one on map"
  (if (can-go-up-p)
      (incf *mapy*)
      (setf *error* 1)) ;Raise error flag
  )

(defun godown ()
  "Advance down by one on map"
  (if (can-go-down-p)
      (decf *mapy*)
      (setf *error* 1)) ;Raise error flag
  )

(defun can-go-up-p ()
  "Returns true if possible to go up"
  (< *mapy* *ymax*)
  )

(defun can-go-down-p ()
  "Returns true if possible to go down"
  (> *mapy* '0)
  )

(defun can-go-right-p ()
  "Returns true if possible to go right"
  (< *mapx* *xmax*)
  )

(defun can-go-left-p ()
  "Returns true if possible to go left"
  (> *mapx* '0)
  )

(defun wherenow ()
  "Returns current coordinates"
  (coordtoloc *mapx* *mapy*)
  )

(defun whatup ()
  "Returns the location above"
  (if (can-go-up-p)
      (coordtoloc *mapx* (+ *mapy* 1))
      (return-from whatup ""))
  )

(defun whatdown ()
  "Returns the location below"
  (if (can-go-down-p)
      (coordtoloc *mapx* (- *mapy* 1))
      (return-from whatdown ""))
  )

(defun whatright ()
  "Returns the location on the right"
  (if (can-go-right-p)
      (coordtoloc (+ *mapx* 1) *mapy*)
      (return-from whatright ""))
  )

(defun whatleft ()
  "Returns the location on the left"
  (if (can-go-left-p)
      (coordtoloc (- *mapx* 1) *mapy*)
      (return-from whatleft ""))
  )

;;;Draw directions for current location
(defun drawdirections ()
  (if (can-go-up-p)
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
  
  (if (can-go-left-p) (format t "< ") (format t "  "))

  (format t "~A" (whatleft))
  
  (dotimes (n (- 13 (lengthreturn (whatleft))))
    (format t " ")
    )
  
  (format t "2  @  3")
  
  (if (can-go-right-p)
      (format t "    >  ~A" (whatright))
      (format t "         "))
  (terpri)
  (format t "                  4")
  (terpri)
  (if (can-go-down-p)
      (format t "                  v  ~A" (whatdown))
      (format t ""))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Creaters, getters, setters;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Character class
(defclass gamecharacter ()
  (
   (name
    :initarg :name)
   (class
    :initarg :class)
   (health
    :initarg :health
    :initform '100)
   (experience
    :initarg :experience
    :initform '0)
   (inventory
    :initarg :inventory
    :initform '())
   )
  )

(defun newcharacter (name class)
  "Creates a new character object"
  (make-instance 'gamecharacter :name name :class class)
  )
  
(defun getname (character)
  "Returns the name of the character"
  (slot-value character 'name)
  )

(defun getclass (character)
  "Returns the class of the character"
  (slot-value character 'class)
  )

(defun gethealth (character)
  "Returns health of the character"
  (slot-value character 'health)
  )

(defun healthbar (character)
  "Visual representation of the character's health"
  (format t "[")
  (dotimes (n (truncate (/ (gethealth character) 2)))
	   (format t "|"))
  (format t "] ~D %" (gethealth character))
  )

(defun getxp (character)
  "Returns XP points for the character"
  (slot-value character 'experience)
  )

;;;Getter for character inventory (list)
(defun getinventory (character)
  (slot-value character 'inventory)
  )

;;;Increase XP points of a character
(defun addxp (character points)
  (setf (slot-value character 'experience) (+ (slot-value character 'experience) points))
  )

;;;Reset XP points of a character
(defun resetxp (character)
  (setf (slot-value character 'experience) '0)
  )

;;;Decrease the health of a character
(defun losehealth (character health)
  (setf (slot-value character 'health) (- (slot-value character 'health) health))
  (if (< (gethealth character) 0) (setf (slot-value character 'health) 0))
  )

;;;Get level for a character
(defun getrank (character)
  (cond ((< (getxp character) '7) (return-from getrank 'Novice))
	((< (getxp character) '10) (return-from getrank 'Warrior))
	((< (getxp character) '20) (return-from getrank 'Champion))
	((< (getxp character) '30) (return-from getrank 'Master))
	(t (return-from getrank 'Unknown))
	)
  )

;;;Creating an inventory
(defun newinventory ()
  (list ())
  )

;;;Adding an item to the inventory of a character
(defun additem (character item)
  (if (null (car (getinventory character)))
      (setf (slot-value character 'inventory) (list item))
      (setf (slot-value character 'inventory) (push item (slot-value character 'inventory)))
      )
  )

(defun item-prompt (character)
  "Asks if player wants to pick up item if exists"
  (if (item-here-p)
      (progn
	(format t "Will you pick up ~A?" (itemhere))
	(let ((input (read)))
	  (if (equal input "y") (additem character (itemhere)))
	  )
	)
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
	   (return-from getmonster "Legendary - critical hit!")
	   )
	  ((> value 80)
	   (losehealth character 10)
	   (return-from getmonster "Rare - you lost health.")
	   )
	  ((> value 60)
	   (losehealth character 5)
	   (return-from getmonster "Magic - some health lost.")
	   )
	  (t (return-from getmonster "No monster")))
    )
  )

(defun magic-weapon ()
  "Generate a random magic weapon"
  (let* ((item nil))
    ;Select category at random
    (setf item (append item (list (nth (random (list-length *itemcat*)) *itemcat*))))
    (setf item (append item (list 'of)))
    ;Select suffix/feature at random
    (setf item (append item (list (nth (random (list-length *itemsuffix*)) *itemsuffix*))))
    )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;'
;;;;;;;BOSS FUNCTIONS;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Make an AI monster (boss)
(defun makeboss ()
  ;List with monster type, x coordinate, y coordinate
  (list (getrand *monstercat*) (random 10) (random 10))
  )

;;;Get boss location
(defun bossloc (boss)
  ;;Get boss's location
  (format t "Location of boss: ~D, ~D" (second boss) (third boss))
  )

(defun meetboss (boss character)
  "Checks if the boss is in your location and makes you lose health"
  (if (and (eql (second boss) *mapx*) (eql (third boss) *mapy*))
      (progn
	(losehealth character '40)
	(format t "You met the boss and lost health!")
	(terpri))))

;;;Move boss to a random direction
(defun moveboss (boss)
  (let ((number (random 4))
	(currx (second boss))
	(curry (third boss)))
    (cond
      ;;Case left
      ((eql number 0)
       (if (> currx 0) (decf (second boss))))
      ;;Case right
      ((eql number 1)
       (if (< currx *xmax*) (incf (second boss))))
      ;;Case down
      ((eql number 2)
       (if (> curry 0) (decf (third boss))))
      ;;Case up
      ((eql number 3)
       (if (< curry *ymax*) (incf (third boss))))
      )))   



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun liveval ()
  "Read-eval loop for live evaluation"
  (setq *input* T)
  (let ((commands nil))
  (loop while *input* do
       (clear-input)
       (if (listen) (append commands (read-char-no-hang))))
  (print commands)
  ))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Game Main function
(defun game (character)
  (setf *mapx* 0)
  (setf *mapy* 0)
  (setf (slot-value character 'health) 100)
  (resetxp character)
  (createitems 11)
  (setitems)

  (let ((input 1) (boss nil))
    (setf boss (makeboss))
    (format t "Monster spawned in: ~D, ~D" (second boss) (third boss))
    (terpri)
    ;Game loop starts
    (loop while (> input '0) do
	 (moveboss boss)
	 (meetboss boss character)
	 (setf *error* 0)
         (format t "**************************************")
         (terpri)
	 ;Show the status of the character
	 (present character)
	 (terpri)
	 ;Print current location
	 (format t "Location: ~A (~D,~D)" (wherenow) *mapx* *mapy*)
	 (terpri)
	 (format t "Items: ~A" (iteminpoint *mapx* *mapy*))
	 (terpri)
	 (item-prompt character)
	 ;;;(random-item)
	 (terpri)
	 (format t "Monsters: ~A" (getmonster character))
	 (terpri)
	 (if (eql (gethealth character) 0)
	     (progn
	       (format t "You died - Game over. ")
	       (return)))
	 (healthbar character)
	 (terpri)
	 (terpri)
	 ;Draw what is around you
	 (drawdirections)
	 (terpri)
	 (terpri)
	 ;;Print the location of the boss character
	 (bossloc boss)
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

    (terpri)
    (format t "XP result: ~D" (getxp character))
    )
  )
