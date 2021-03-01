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

(defun run-game-round ()
  "Run one round of game"
  (advance-to-dir *mon1* (random-dir *mon1*)) (terpri)
  (advance-to-dir *mon2* (random-dir *mon2*)) (terpri)
  (advance-to-dir *mon3* (random-dir *mon3*)) (terpri)
  (confront *mon1* *player*)
  (confront *mon2* *player*)
  (confront *mon3* *player*)
  (confront *item1* *player*)
  (confront *item2* *player*)
  (setq *score* (* 100 *rpg-iter*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                 ;;;
;;;        Functions                ;;;
;;;                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rpg ()
  "Main function"
  (initialize-game)
  (let ((input nil))
    (loop while (>= *rpg-iter* 0) do
	 (incf *rpg-iter*)
	 (run-game-round)
	 (print-map)(terpri)
	 (setq input (read))
	 (advance-player *player* input)
	 (if (equal input 'Q) (setq *rpg-iter* -1)))))
 
