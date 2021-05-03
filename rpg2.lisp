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

(in-package :rpg2)

(defun advance-monsters ()
  "Advance/move all monsters in the game"
  (loop for mon in *game-objects*
     do
       (if (npc-p mon) ;only if a non-player monster (NPC)
	   (if (monster-alive-p mon) ;if alive
	       (advance-to-dir mon (random-dir mon)) ;move
	       (make-invisible mon))))) ;else remove

(defun confront-all ()
  "Confront monsters and items with the Player"
  (loop for obj in *game-objects*
     do
       (if (not (playerp obj))
	   (confront obj *player*))))

(defun near-monsters ()
  "List monsters near the Player, distance < 2"
  (let ((near-monsters nil))
    (loop for obj in *game-objects*
       do
	 (if (and (npc-p obj)
		  (< (loc-difference obj *player*) 2))
	       (setq near-monsters (append2 near-monsters obj))))
    near-monsters))

(defun run-game-round ()
  "Run one round of game"
  (advance-monsters) ;move all the monsters
  (confront-all) ;confront monsters and items with player
  (poison-hurt *player*)
  (setq *score* (+ *score* 100)))

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
  (create-n-poisons 6)
  (let ((input nil))
    (loop while (and (>= *rpg-iter* 0) (monster-alive-p *player*)) do
	 (incf *rpg-iter*)
	 (run-game-round)
	 (print-map)(terpri)
	 (setq input (read))
	 (advance-player *player* input)
	 (if (equal input 'Q) (setq *rpg-iter* -1)))))
 
