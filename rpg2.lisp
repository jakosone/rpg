;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                    ;;;
;;;   ====   ====    ====    ========  ;;;
;;;   I   I  I   I  I   \\    II  II   ;;;
;;;   ====   I===   I  ___    II  II   ;;;
;;;   I \\   I      I   II    II  II   ;;;
;;;   I  \\  I       ===//   ========  ;;;
;;;                                    ;;;
;;;The refactored version;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Assign objects to global variables
;;;Items also added to monsters list
;; (setq *game-objects* nil)
;; (setq *game-objects* (append2 *game-objects* *player*))
;; (setq *game-objects* (append2 *game-objects* *mon1*))
;; (setq *game-objects* (append2 *game-objects* *mon2*))
;; (setq *game-objects* (append2 *game-objects* *mon3*))
;; (setq *game-objects* (append2 *game-objects* *item1*))
;; (setq *game-objects* (append2 *game-objects* *item2*))

(defun advance-monsters ()
  "Advance/move all monsters in the game"
  (loop for mon in *game-objects*
     do ;only if a monster and not the player
       (if (and (eql (type-of mon) 'monster)
		(not (playerp mon)))
	   (advance-to-dir mon (random-dir mon)))))

(defun confront-all ()
  "Confront monsters and items with the player"
  (loop for obj in *game-objects*
     do
       (if (not (playerp obj))
	   (confront obj *player*))))

(defun run-game-round ()
  "Run one round of game"
  (advance-monsters)
  (confront-all)
  (setq *score* (* 100 *rpg-iter*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                 ;;;
;;;        Functions                ;;;
;;;                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rpg ()
  "Main function"
  (initialize-game)
  (create-n-monsters 9)
  (create-n-potions 4)
  (let ((input nil))
    (loop while (and (>= *rpg-iter* 0) (monster-alive-p *player*)) do
	 (incf *rpg-iter*)
	 (run-game-round)
	 (print-map)(terpri)
	 (setq input (read))
	 (advance-player *player* input)
	 (if (equal input 'Q) (setq *rpg-iter* -1)))))
 
