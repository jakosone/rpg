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
(setq *monsters* nil)
(setq *monsters* (append2 *monsters* *player*))
(setq *monsters* (append2 *monsters* *mon1*))
(setq *monsters* (append2 *monsters* *mon2*))
(setq *monsters* (append2 *monsters* *mon3*))

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
  (confront *mon3* *player*))

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
 
