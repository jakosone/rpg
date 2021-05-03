;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;CLOS methods;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Monster and Item methods;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rpg2)

(defmethod get-monster ((obj monster))
  "Get information of monster"
  (format t "Monster: ~A; Level: ~D; Health: ~D; Location: ~A" (get-avatar obj) (get-level obj) (get-health obj) (get-location obj)))

(defmethod get-location ((obj game-object))
  "Get location of a game object"
  (location obj))

(defmethod get-item-type ((obj item))
  "Get the type of an item object"
  (item-type obj))

(defmethod x-loc ((obj monster))
  "Get X coordinate of a monster"
  (car (location obj)))

(defmethod y-loc ((obj monster))
  "Get Y coordinate of a monster"
  (cdr (location obj)))

(defmethod get-health ((obj monster))
  "Get health of a monster"
  (health obj))

(defmethod get-health ((obj item))
  "Health of an item is always nil"
  nil)

(defmethod set-health ((obj monster) change)
  "Set health for a monster object"
  (with-slots (health) obj
    (if (> (+ health change) 0) ;prevent negative health
	(setq health (+ health change))
	(setq health 0))))

(defmethod get-avatar ((obj game-object))
  "Get the avatar of a game object"
  (avatar obj))

(defmethod get-level ((obj monster))
  "Get level of a monster"
  (level obj))

(defmethod get-status ((obj monster))
  "Get status of a monster"
  (status obj))

(defmethod playerp ((obj monster))
  "Returns true if a player object"
  (player obj))

(defmethod playerp ((obj item))
  "Always nil for item objects"
  nil)

(defmethod visiblep ((obj game-object))
  "Is the object visible in the game"
  (visible obj))

(defmethod make-invisible ((obj game-object))
  "Make object inivisible in the game"
  (with-slots (visible) obj
    (setq visible nil)))

(defmethod make-visible ((obj game-object))
  "Make object visible in the game"
  (with-slots (visible) obj
    (setq visible T)))

(defmethod poison ((monster monster))
  "Set monster's status as poisoned"
  (with-slots (status) monster
    (setq status 'poisoned)))

(defmethod poison-hurt ((monster monster))
  "Hurt the monster/player if poisoned"
  (if (poisoned-p monster) (set-health monster -2)))

(defmethod poisoned-p ((obj monster))
  "Is monster e.g. Player, poisoned"
  (eql (get-status obj) 'poisoned))

(defmethod monster-p ((obj game-object))
  "Is object a monster object"
  (eql (class-name (class-of obj)) 'monster))

(defmethod npc-p ((obj game-object))
  "Is object non-player"
  (and (monster-p obj)
       (not (playerp obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;Locational methods;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-location ((obj monster) loc)
  "Set location of a game object"
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
    ;;X is the same, monster's Y higher
    ((and (equal (x-loc mon) (x-loc player))
	  (> (y-loc mon) (y-loc player))) 'down)
    ;;X is the same, player's Y higher
    ((and (equal (x-loc mon) (x-loc player))
	  (< (y-loc mon) (y-loc player))) 'up)
    ;;Y is the same, monster's X higher
    ((and (> (x-loc mon) (x-loc player))
	  (equal (y-loc mon) (y-loc player))) 'left)
    ;;Y is the same, player's X higher
    ((and (< (x-loc mon) (x-loc player))
	  (equal (y-loc mon) (y-loc player))) 'right)
    (T "not in a straight line or in same position")))
     
(defmethod confront ((mon monster) (player monster))
  "Hurt player if close to a monster"
  ;; If same location, hurt by 10 HP
  (if (same-loc-p (get-location mon) (get-location player))
      (set-health player -10))
  ;; If next to monster, hurt by 5 HP
  (if (eql (loc-difference mon player) 1)
      (set-health player -5)))

(defmethod confront ((item item) (player monster))
  "Affect player's health if in same position with an item"
  (if (same-loc-p (get-location item) (get-location player))
      (cond
	;;Item is a Potion
	((eql (get-item-type item) 'potion)
	 ;;Give the player 5 Hit Points
	 (progn
	   (set-health player 5)
	   (make-invisible item)))
	;;Item is a Poison
	((eql (get-item-type item) 'poison)
	 ;;Poison the player
	 (progn
	   (poison player)
	   (make-invisible item))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;Directional methods;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod available-dir ((obj monster))
  "Available directions for a monster"
  (cond
    ;; x=0 & y=0
    ((and (zerop (x-loc obj)) (zerop (y-loc obj))) '(up right))
    ;; x=max & y=max
    ((and (equal (x-loc obj) (xmax)) (equal (y-loc obj) (ymax))) '(down left))
    ;; x=0 & y=max
    ((and (zerop (x-loc obj)) (equal (y-loc obj) (ymax))) '(down right))
    ;; x=max & y=0
    ((and (equal (x-loc obj) (xmax)) (zerop (y-loc obj))) '(up left))
    ;; x=max
    ((equal (x-loc obj) (xmax)) '(up down left))
    ;; y=max
    ((equal (y-loc obj) (ymax)) '(down left right))
    ;; x=0
    ((zerop (x-loc obj)) '(up right down))
    ;; y=0
    ((zerop (y-loc obj)) '(up right left))
    (t '(up right down left))))

(defmethod random-dir ((obj monster))
  "Select a direction randomly (for simulation etc.)"
  (if
   ;;If distance is under 5 and only on every other time...
   (and (< (loc-difference obj *player*) 10) (equal 0 (mod *rpg-iter* 2)))
   ;;...move to player's direction
   (player-direction obj *player*)
   ;;Else move randomly
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
    ((equal input 'i) (swing-sword))
    ((and (equal input 'w) (member 'up (available-dir player)))
     (advance-to-dir player 'up))
    ((and (equal input 'a) (member 'left (available-dir player)))
     (advance-to-dir player 'left))
    ((and (equal input 's) (member 'down (available-dir player)))
     (advance-to-dir player 'down))
    ((and (equal input 'd) (member 'right (available-dir player)))
     (advance-to-dir player 'right))
    (t "invalid direction")))

(defmethod monster-alive-p ((monster monster))
  "Is a monster or Player alive?"
  (> (get-health monster) 0))
