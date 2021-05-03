;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Player functions;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-player-class ()
  "Get Player's class based on score"
  (cond
    ((< *score* 1000) 'Rookie)
    ((< *score* 5000) 'Apprentice)
    ((< *score* 10000) 'Fighter)
    ((< *score* 15000) 'Warrior)
    ((< *score* 20000) 'Commander)
    ((< *score* 25000) 'Knight)
    (T 'Champion)))
