;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;CLOS objects;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass game-object ()
  ((avatar
    :initarg :avatar
    :accessor avatar)
   (location
    :initarg :location
    :initform '(0 . 0)
    :accessor location)
   (floor-level
    :initarg :floor-level
    :initform '0
    :accessor floor-level)
   (visible
    :initarg :visible
    :initform T
    :accessor visible)))
  

(defclass monster (game-object)
  ((level
    :initarg :level
    :initform '1
    :accessor level)
   (health
    :initarg :health
    :initform '100
    :accessor health)
   (player
    :initarg :player
    :initform nil
    :accessor player)))

(defclass item (game-object)
  ((item-type
    :initarg :item-type
    :accessor item-type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Global variables;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Map size
(defvar *mapdim* nil)

;;;Game iterations
(defvar *rpg-iter* nil)

;;;Player score
(defvar *score* nil)

;;;Monster list
(defvar *game-objects* nil)

;;;Monster objects for testing
(defvar *player* nil)
(defvar *mon1* nil)
(defvar *mon2* nil)
(defvar *mon3* nil)

;;;Item objects for testing
(defvar *item1* nil)
(defvar *item2* nil)

;;;Initialize game environment
(defun initialize-game ()
  "Initialize the global variables"
  (progn
    (setq *mapdim* 26)
    (setq *rpg-iter* 0)
    (setq *score* 0)
    (setq *game-objects* nil)
    (setq *player* nil)
    (setq *mon1* nil)
    (setq *mon2* nil)
    (setq *mon3* nil)
    (setq *item1* nil)
    (setq *item2* nil)
    ;;Create player object
    (setq *player* (make-instance 'monster :avatar 'P :location '(0 . 0) :player T))
    ;;Create monster objects
    (setq *mon1* (make-instance 'monster :avatar '@ :location '(21 . 21)))
    (setq *mon2* (make-instance 'monster :avatar '& :location '(0 . 20)))
    (setq *mon3* (make-instance 'monster :avatar '% :location '(3 . 9)))
    ;;Create item objects
    (setq *item1* (make-instance 'item :avatar 'H :location '(15 . 15) :item-type 'potion))
    (setq *item2* (make-instance 'item :avatar 'H :location '(20 . 20) :item-type 'potion))
    (setq *game-objects* (append2 *game-objects* *player*))
    (setq *game-objects* (append2 *game-objects* *mon1*))
    (setq *game-objects* (append2 *game-objects* *mon2*))
    (setq *game-objects* (append2 *game-objects* *mon3*))
    (setq *game-objects* (append2 *game-objects* *item1*))
    (setq *game-objects* (append2 *game-objects* *item2*))))
  
