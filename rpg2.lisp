;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;RPG 2;;;;;;;;;;;;;
;;;The refactored version;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;;Monster objects for testing
(defvar *mon1* nil)
(defvar *mon2* nil)
(setq *mon1* (make-instance 'monster :avatar '@ :location '(20 . 10)))
(setq *mon2* (make-instance 'monster :avatar '& :location '(40 . 30)))

(defun debug-rpg ()
  (get-monster *mon1*) (terpri)
  (get-monster *mon2*) (terpri)
  (format t "Distance: ~A" (loc-difference *mon1* *mon2*)) (terpri)
  (advance-to-dir *mon1* (random-dir *mon1*)) (terpri)
  (advance-to-dir *mon2* (random-dir *mon2*)) (terpri))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Functions;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rpg ()
  "Main function"
  (let ((input nil) (iteration 0))
    (loop while (>= iteration 0) do
	 (incf iteration)
	 (debug-rpg)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;CLOS methods;;;;;;;;;;;;;
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

(defmethod set-location ((obj monster) loc)
  "Set location of a monster"
  (with-slots (location) obj
    (setq location loc)))

(defmethod loc-difference ((obj1 monster) (obj2 monster))
  "Get the difference of two monsters' locations"
  (cons
   (abs (- (car (get-location obj1)) (car (get-location obj2))))
   (abs (- (cdr (get-location obj1)) (cdr (get-location obj2))))))

(defmethod available-dir ((obj monster))
  "Available directions for a monster"
  (cond
    ((and (zerop (x-loc obj)) (zerop (y-loc obj))) '(up right))
    ((zerop (x-loc obj)) '(up right down))
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
    
    
