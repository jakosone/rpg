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
(setq *game-objects* nil)
(setq *game-objects* (append2 *game-objects* *player*))
(setq *game-objects* (append2 *game-objects* *mon1*))
(setq *game-objects* (append2 *game-objects* *mon2*))
(setq *game-objects* (append2 *game-objects* *mon3*))
(setq *game-objects* (append2 *game-objects* *item1*))
(setq *game-objects* (append2 *game-objects* *item2*))

(defun debug-rpg ()
  "Debug function"
  ;(get-monster *mon1*) (terpri)
  ;(get-monster *mon2*) (terpri)
					;(format t "Distance: ~A" (loc-difference *mon1* *mon2*)) (terpri)
  (advance-to-dir *mon1* (random-dir *mon1*)) (terpri)
  (advance-to-dir *mon2* (random-dir *mon2*)) (terpri)
  (advance-to-dir *mon3* (random-dir *mon3*)) (terpri)
  (confront *mon1* *player*)
  (confront *mon2* *player*)
  (confront *mon3* *player*)
  (confront *item1* *player*)
  (confront *item2* *player*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                 ;;;
;;;        Functions                ;;;
;;;                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rpg ()
  "Main function"
  (let ((input nil))
    (loop while (>= *rpg-iter* 0) do
	 (incf *rpg-iter*)
	 (debug-rpg)
	 (print-map)(terpri)
	 (setq input (read))
	 (advance-player *player* input)
	 (if (equal input 'Q) (setq *rpg-iter* -1)))))
 
