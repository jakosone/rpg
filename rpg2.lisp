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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;CLOS objects;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass monster ()
  ((avatar
    :initarg :avatar
    :accessor avatar)
   (level
    :initarg :level
    :initform '1
    :accessor level)
   (health
    :initarg :health
    :initform '100
    :accessor health)
   (location
    :initarg :location
    :initform '(0 . 0)
    :accessor location)))

;;;;;Global variables;;;;;;

;;;Map size
(defvar *mapdim* nil)
(setq *mapdim* 30)

;;;Monster list
(defvar *monsters* nil)

;;;Monster objects for testing
(defvar *mon1* nil)
(defvar *mon2* nil)
(defvar *mon3* nil)
(setq *mon1* (make-instance 'monster :avatar '@ :location '(20 . 10)))
(setq *mon2* (make-instance 'monster :avatar '& :location '(5 . 20)))
(setq *mon3* (make-instance 'monster :avatar '% :location '(3 . 9)))
(setq *monsters* (cons *mon1* nil))
(setq *monsters* (cons *mon2* *monsters*))
(setq *monsters* (cons *mon3* *monsters*))

(defun debug-rpg ()
  "Debug function"
  ;(get-monster *mon1*) (terpri)
  ;(get-monster *mon2*) (terpri)
  ;(format t "Distance: ~A" (loc-difference *mon1* *mon2*)) (terpri)
  (advance-to-dir *mon1* (random-dir *mon1*)) (terpri)
  (advance-to-dir *mon2* (random-dir *mon2*)) (terpri)
  (advance-to-dir *mon3* (random-dir *mon3*)) (terpri))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                 ;;;
;;;        Functions                ;;;
;;;                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rpg ()
  "Main function"
  (let ((input nil) (iteration 0))
    (loop while (>= iteration 0) do
	 (incf iteration)
	 (debug-rpg)
	 (print-map)(terpri)
	 (setq input (read))
	 (if (equal input 'Q) (setq iteration -1)))))
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;UI functions;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun n-empty-rows (n)
  "Insert n empty rows with (terpri)"
  (loop repeat n do
       (terpri)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;Basic functions;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun x+1 (xy-loc)
  "Add 1 to x coordinate"
  (cons (+ (car xy-loc) 1) (cdr xy-loc)))

(defun x-1 (xy-loc)
  "Subtract 1 from x coordinate"
  (cons (- (car xy-loc) 1) (cdr xy-loc)))

(defun y+1 (xy-loc)
  "Add 1 to y coordinate"
  (cons (car xy-loc) (+ (cdr xy-loc) 1)))

(defun y-1 (xy-loc)
  "Subtract 1 from y coordinate"
  (cons (car xy-loc) (- (cdr xy-loc) 1)))

(defun negative-loc-p (xy-loc)
  "True if X or Y is negative"
  (or (minusp (car xy-loc)) (minusp (cdr xy-loc))))

(defun xmax ()
  "Max value of X coordinate"
  (- *mapdim* 1))

(defun ymax ()
  "Max value of Y coordinate"
  (- *mapdim* 1))

(defun same-loc-p (xy-loc1 xy-loc2)
  "Return true if two locations are the same"
  (and
   (equal (car xy-loc1) (car xy-loc2))
   (equal (cdr xy-loc1) (cdr xy-loc2))))

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

(defmethod get-avatar ((obj monster))
  "Get the avatar of a monster"
  (avatar obj))

(defmethod get-level ((obj monster))
  "Get level of a monster"
  (level obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;Locational methods;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-location ((obj monster) loc)
  "Set location of a monster"
  (with-slots (location) obj
    (setq location loc)))

(defmethod loc-difference ((obj1 monster) (obj2 monster))
  "Get the difference of two monsters' locations"
  (cons
   (abs (- (car (get-location obj1)) (car (get-location obj2))))
   (abs (- (cdr (get-location obj1)) (cdr (get-location obj2))))))

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
  (let* ((available-dirs (available-dir obj))
	(rand-dir (nth (random (length available-dirs)) available-dirs)))
    rand-dir))

(defmethod advance-to-dir ((obj monster) direction)
  "Advance monster to a direction"
  (cond
    ((equal direction 'up) (set-location obj (y+1 (get-location obj))))
    ((equal direction 'down) (set-location obj (y-1 (get-location obj))))
    ((equal direction 'right) (set-location obj (x+1 (get-location obj))))
    ((equal direction 'left) (set-location obj (x-1 (get-location obj))))))
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                  ;;;
;;;    User interface functions      ;;;
;;;                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun monsters-in-point (xy-point)
  "Return monsters in point"
  (let ((list nil))
    (loop for mon in *monsters* do
	 (if (same-loc-p (get-location mon) xy-point)
	     (setq list (append list mon))))
    list))
       
(defun print-map ()
  "Print the map with monsters"
  (loop repeat (+ 2 *mapdim*) do (format t "=")) ;top frame
  (terpri)
  (dotimes (i *mapdim*) ;iterate rows
    (format t "I")
    (dotimes (j *mapdim*) ;iterate columns inside a row
      (if (monsters-in-point (cons j (- *mapdim* i)))
	  (format t "~A" (get-avatar (monsters-in-point (cons j (- *mapdim* i)))))
	  (format t " ")))
    (format t "I")
    (terpri))
  (loop repeat (+ 2 *mapdim*) do (format t "="))) ;bottom frame
