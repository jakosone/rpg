;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;CLOS methods;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Monster methods;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-monster ((obj monster))
  "Get information of monster"
  (format t "Monster: ~A; Level: ~D; Health: ~D; Location: ~A" (get-avatar obj) (get-level obj) (get-health obj) (get-location obj)))

(defmethod get-location ((obj monster))
  "Get location of a monster"
  (location obj))

(defmethod x-loc ((obj monster))
  "Get X coordinate of a monster"
  (car (location obj)))

(defmethod y-loc ((obj monster))
  "Get Y coordinate of a monster"
  (cdr (location obj)))

(defmethod get-health ((obj monster))
  "Get health of a monster"
  (health obj))

(defmethod set-health ((obj monster) change)
  "Set health for a monster object"
  (with-slots (health) obj
    (setq health (+ health change))))

(defmethod get-avatar ((obj monster))
  "Get the avatar of a monster"
  (avatar obj))

(defmethod get-level ((obj monster))
  "Get level of a monster"
  (level obj))

(defmethod playerp ((obj monster))
  "Returns true if a player object"
  (player obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;Locational methods;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-location ((obj monster) loc)
  "Set location of a monster"
  (with-slots (location) obj
    (setq location loc)))

(defmethod loc-difference ((obj1 monster) (obj2 monster))
  "Get the difference of two monsters' locations"
  (+
   (abs (- (car (get-location obj1)) (car (get-location obj2))))
   (abs (- (cdr (get-location obj1)) (cdr (get-location obj2))))))

(defmethod player-direction ((mon monster) (player monster))
  "What direction the player is in from the monster"
  (cond
    ;;;X is the same, monster's Y higher
    ((and (equal (x-loc mon) (x-loc player))
	  (> (y-loc mon) (y-loc player))) 'down)
    ;;;X is the same, player's Y higher
    ((and (equal (x-loc mon) (x-loc player))
	  (< (y-loc mon) (y-loc player))) 'up)
    ;;;Y is the same, monster's X higher
    ((and (> (x-loc mon) (x-loc player))
	  (equal (y-loc mon) (y-loc player))) 'left)
    ;;;Y is the same, player's X higher
    ((and (< (x-loc mon) (x-loc player))
	  (equal (y-loc mon) (y-loc player))) 'right)
    (T "not in a straight line or in same position")))
     
(defmethod confront ((mon monster) (player monster))
  "Hurt player if in same position with a monster"
  (if (same-loc-p (get-location mon) (get-location player))
      (set-health player -5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;Directional methods;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod available-dir ((obj monster))
  "Available directions for a monster"
  (cond
    ;;; x=0 & y=0
    ((and (zerop (x-loc obj)) (zerop (y-loc obj))) '(up right))
    ;;; x=max & y=max
    ((and (equal (x-loc obj) (xmax)) (equal (y-loc obj) (ymax))) '(down left))
    ;;; x=0 & y=max
    ((and (zerop (x-loc obj)) (equal (y-loc obj) (ymax))) '(down right))
    ;;; x=max & y=0
    ((and (equal (x-loc obj) (xmax)) (zerop (y-loc obj))) '(up left))
    ;;; x=max
    ((equal (x-loc obj) (xmax)) '(up down left))
    ;;; y=max
    ((equal (y-loc obj) (ymax)) '(down left right))
    ;;; x=0
    ((zerop (x-loc obj)) '(up right down))
    ;;; y=0
    ((zerop (y-loc obj)) '(up right left))
    (t '(up right down left))))

(defmethod random-dir ((obj monster))
  "Select a direction randomly (for simulation etc.)"
  (if
   ;;;If distance is under 5 and only on every other time...
   (and (< (loc-difference obj *player*) 5) (equal 0 (mod *rpg-iter* 2)))
   ;;;...move to player's direction
   (player-direction obj *player*)
   ;;;Else move randomly
   (let* ((available-dirs (available-dir obj))
	  (rand-dir (nth (random (length available-dirs)) available-dirs)))
     rand-dir)))

(defmethod advance-to-dir ((obj monster) direction)
  "Advance monster to a direction"
  (cond
    ((equal direction 'up) (set-location obj (y+1 (get-location obj))))
    ((equal direction 'down) (set-location obj (y-1 (get-location obj))))
    ((equal direction 'right) (set-location obj (x+1 (get-location obj))))
    ((equal direction 'left) (set-location obj (x-1 (get-location obj))))))

(defmethod advance-player ((player monster) input)
  "Advance player according to user input"
  (cond
    ((and (equal input 'w) (member 'up (available-dir player)))
     (advance-to-dir player 'up))
    ((and (equal input 'a) (member 'left (available-dir player)))
     (advance-to-dir player 'left))
    ((and (equal input 's) (member 'down (available-dir player)))
     (advance-to-dir player 'down))
    ((and (equal input 'd) (member 'right (available-dir player)))
     (advance-to-dir player 'right))
    (t "invalid direction")))
