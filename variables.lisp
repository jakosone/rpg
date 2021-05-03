;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;CLOS objects;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :rpg2 (:use :common-lisp))
(in-package :rpg2)


(defclass game-object ()
  ((avatar ;Avatar of the object visible in game
    :initarg :avatar
    :accessor avatar)
   (location
    :initarg :location ;(X . Y) cons pair of location
    :initform '(0 . 0)
    :accessor location)
   (floor-level ;Game floor/level on which the object is located
    :initarg :floor-level
    :initform '0
    :accessor floor-level)
   (visible ;Visible in game
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
   (status ;e.g. "poisoned"
    :initarg :status
    :initform 'normal
    :accessor status)
   (player
    :initarg :player ;the player object is also a Monster
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

;;;Messages for player
(defvar *message* nil)
(setq *message* " ")

;;;Current visible floor in the game
(defvar *floor-level* nil)

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

(defun create-n-monsters (number)
  "Create N monsters in the game"
  (dotimes (i number)
    (setq *game-objects* (cons (make-random-monster) *game-objects*))))

(defun create-n-potions (number)
  "Create N health items in the game"
  (dotimes (i number)
    (setq *game-objects* (cons (make-random-potion) *game-objects*))))

(defun create-n-poisons (number)
  "Create N poison items in the game"
  (dotimes (i number)
    (setq *game-objects* (cons (make-random-poison) *game-objects*))))

;;;Initialize game environment
(defun initialize-game ()
  "Initialize the global variables"
  (progn
    (setq *mapdim* 26)
    (setq *message* " ")
    (setq *floor-level* 0)
    (setq *rpg-iter* 0)
    (setq *score* 0)
    (setq *game-objects* nil)
    (setq *player* nil)

    ;;Create player object
    (setq *player* (make-instance 'monster :avatar '@ :location '(0 . 0) :player T))
    (setq *game-objects* (append2 *game-objects* *player*))))
  
