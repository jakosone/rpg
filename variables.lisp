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
    :accessor location)
   (player
    :initarg :player
    :initform nil
    :accessor player)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;Global variables;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;Map size
(defvar *mapdim* nil)
(setq *mapdim* 30)

;;;Game iterations
(defvar *rpg-iter* nil)
(setq *rpg-iter* 0)

;;;Monster list
(defvar *monsters* nil)

;;;Monster objects for testing
(defvar *player* nil)
(defvar *mon1* nil)
(defvar *mon2* nil)
(defvar *mon3* nil)
;;;Make class instances, monsters
(setq *player* (make-instance 'monster :avatar 'P :location '(0 . 0) :player T)) 
(setq *mon1* (make-instance 'monster :avatar '@ :location '(29 . 29)))
(setq *mon2* (make-instance 'monster :avatar '& :location '(0 . 20)))
(setq *mon3* (make-instance 'monster :avatar '% :location '(3 . 9)))
