;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;RPG 2;;;;;;;;;;;;;
;;;The refactored version;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;Global variables;;;;;;

;;;Main flag for the game's state
(defvar *game-on* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Functions;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rpg ()
  "Main function"
  (setq *game-on* T)
  (let ((input nil))
    (loop while *game-on* do
	 (setq input (read)))))
       
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;CLOS objects;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass monster ()
  ((name
    :initarg :name
    :accessor name)
   (level
    :initarg :level
    :initform '1
    :accessor level)
   (location
    :initarg :location
    :initform '(0 . 0)
    :accessor location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;CLOS methods;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-location ((obj monster))
  "Get location of a monster"
  (location obj))

(defmethod set-location ((obj monster) loc)
  "Set location of a monster"
  (with-slots (location) obj
    (setq location loc)))

(defmethod random-dir ((obj monster))
  "Select a direction randomly (for simulation etc.)"
  (let ((rand (random 4)))
    (cond
      ((equal rand 0) (set-location obj (x+1 (get-location obj))))
      ((equal rand 1) (set-location obj (x-1 (get-location obj))))
      ((equal rand 2) (set-location obj (y+1 (get-location obj))))
      ((equal rand 3) (set-location obj (y-1 (get-location obj)))))))
