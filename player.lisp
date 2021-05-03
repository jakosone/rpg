;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Player functions;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :rpg2)

(defun get-player-class ()
  "Get Player's class based on score"
  (cond
    ((< *score* 1000) 'Rookie)
    ((< *score* 5000) 'Apprentice)
    ((< *score* 10000) 'Fighter)
    ((< *score* 15000) 'Warrior)
    ((< *score* 20000) 'Commander)
    ((< *score* 25000) 'Knight)
    ((< *score* 50000) 'Master)
    (T 'Champion)))

(defun swing-sword ()
  "Hurt nearby monsters (with the sword)"
  (if (near-monsters)
      (loop for monster in (near-monsters) ;monsters near player
	 do (progn
	      (set-health monster -20) ;hurt monster by 20HP
	      (if (monster-alive-p monster)
		  (setq *message* "You hurt the monster!")
		  (setq *message* "You killed the monster!"))))	  
      (setq *message* "You swung thin air!"))) 
