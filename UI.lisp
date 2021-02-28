;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                  ;;;
;;;    User interface functions      ;;;
;;;                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun monsters-in-point (xy-point)
  "Return monsters in point (x . y)"
  (let ((list nil))
    (loop for mon in *game-objects* do
	 (if (same-loc-p (get-location mon) xy-point)
	     (setq list (append2 list mon))))
    list))

(defun visible-monsters-in-point (xy-point)
  "Return the visible monsters in point (x . y)"
  (let ((list nil))
    (loop for mon in *game-objects* do
	 (if (and
	      (same-loc-p (get-location mon) xy-point)
	      (visiblep mon))
	     (setq list (append2 list mon))))
    list))

(defun 1st-monster-in-point (xy-point)
  "Return the first (car) monster in point (x . y)"
  (car (monsters-in-point xy-point)))
       
(defun print-map ()
  "Print the map with monsters"
  (loop repeat (+ 2 *mapdim*) do (format t "=")) ;top frame
  (terpri)
  (dotimes (i *mapdim*) ;iterate rows
    (format t "I")
    (dotimes (j *mapdim*) ;iterate columns inside a row
      ;;Here -1 for the Y coordinate because mapdim is 30
      ;;and the indices go 0...29
      ;;Note: the logic here is based on only the first (car)
      ;;object in the position.
      (if (visible-monsters-in-point (cons j (- *mapdim* i 1)))
	  (format t "~A" (get-avatar (car (visible-monsters-in-point (cons j (- *mapdim* i 1))))))
	  (format t " ")))
    (format t "I")
    (terpri))
  (loop repeat (+ 2 *mapdim*) do (format t "="))
  (format t " Player health: ~D%" (get-health *player*))) ;bottom frame
