(defpackage :advent/2018/15 #.cl-user::*advent-use*)
(in-package :advent/2018/15)

;;;; Points -------------------------------------------------------------------
(defun p (row col) (complex row col))
(defun row (p) (realpart p))
(defun col (p) (imagpart p))

(defun p< (p1 p2)
  (or (< (row p1) (row p2))
      (and (= (row p1) (row p2))
           (< (col p1) (col p2)))))

(defun neighbors (p)
  (list (+ p #c(0 1))
        (+ p #c(1 0))
        (+ p #c(0 -1))
        (+ p #c(-1 0))))

(defun distance (p1 p2)
  (abs (- p1 p2)))


;;;; Loc ----------------------------------------------------------------------
(defvar *locations* nil)

(beast:define-aspect loc p)

(defun loc/row (entity) (row (loc/p entity)))
(defun loc/col (entity) (col (loc/p entity)))


(defun initialize-loc (rows cols)
  (setf *locations* (make-array (list rows cols) :initial-element nil)))

(defmethod beast:entity-created :after ((entity loc))
  (setf (aref *locations* (loc/row entity) (loc/col entity)) entity))

(defmethod beast:entity-destroyed :after ((entity loc))
  (when (loc/p entity)
    (setf (aref *locations* (loc/row entity) (loc/col entity)) nil)))


(defun move (entity p)
  (setf (aref *locations* (loc/row entity) (loc/col entity)) nil
        (loc/p entity) p
        (aref *locations* (loc/row entity) (loc/col entity)) entity))

(defun loc (p)
  (aref *locations* (row p) (col p)))

(defun loc< (e1 e2)
  (p< (loc/p e1) (loc/p e2)))


;;;; Other Aspects ------------------------------------------------------------
(beast:define-aspect living (hp :initform 200))
(beast:define-aspect fighter (attack-power :initform 3))
(beast:define-aspect renderable (glyph))


;;;; Entities -----------------------------------------------------------------
(defvar *dead* nil)

(beast:define-entity elf (loc living fighter renderable))
(beast:define-entity goblin (loc living fighter renderable))

(defmethod print-object ((o elf) s)
  (print-unreadable-object (o s :type t)
    (format s "~A ~D" (loc/p o) (living/hp o))))

(defmethod print-object ((o goblin) s)
  (print-unreadable-object (o s :type t)
    (format s "~A ~D" (loc/p o) (living/hp o))))


(defun create-elf (p)
  (beast:create-entity 'elf :loc/p p :renderable/glyph #\E))

(defun create-goblin (p)
  (beast:create-entity 'goblin :loc/p p :renderable/glyph #\G))


(defun entities-to-move ()
  (sort (beast:map-entities #'identity) #'loc<))

(defun enemiesp (e1 e2)
  (typep e2 (etypecase e1
              (goblin 'elf)
              (elf 'goblin))))

(defun targets (entity)
  (beast:map-entities #'identity (etypecase entity
                                   (goblin 'elf)
                                   (elf 'goblin))))

(defun attack (attacker defender)
  (decf (living/hp defender)
        (fighter/attack-power attacker))
  (unless (plusp (living/hp defender))
    (push defender *dead*)
    (beast:destroy-entity defender)))


;;;; Terrain ------------------------------------------------------------------
(defvar *terrain* nil)

(defun initialize-terrain (rows cols)
  (setf *terrain* (make-array (list rows cols)
                    :element-type 'character
                    :initial-element #\.)))


(defun terrain (p)
  (aref *terrain* (row p) (col p)))

(defun (setf terrain) (new-value p)
  (setf (aref *terrain* (row p) (col p)) new-value))


(defun passablep (p)
  (case (terrain p)
    (#\# nil)
    (t t)))


;;;; World --------------------------------------------------------------------
(defun openp (p)
  (and (null (loc p))
       (passablep p)))

(defun in-bounds-p (p)
  (array-in-bounds-p *terrain* (row p) (col p)))

(defun open-neighbors (p)
  (remove-if-not (alexandria:conjoin #'openp #'in-bounds-p)
                 (neighbors p)))

(defun adjacent-enemies (mob)
  (-<> mob
    loc/p
    neighbors
    (mapcar #'loc <>)
    (remove nil <>)
    (remove-if-not (curry #'enemiesp mob) <>)))

(defun adjacent-enemy (mob)
  (first (sort (adjacent-enemies mob) #'loc<)))

(defun target-squares (unit)
  (-<> unit
    targets
    (mapcan (compose #'open-neighbors #'loc/p) <>)
    (remove-duplicates <> :test #'=)))


(defun step-cost (start from to)
  ;; We adjust the cost of the first step of our path to account for the
  ;; bullshit reading order tie breaking (but never enough to send us the wrong
  ;; way).
  (if (= from start)
    (ecase (- to from)
      (#c(-1 0) 1)
      (#c(0 -1) 1.1)
      (#c(0 1) 1.2)
      (#c(1 0) 1.3))
    1))

(defun action (mob)
  (if-let ((enemy (adjacent-enemy mob)))
    (values :attack enemy)
    (iterate
      (with goals = (target-squares mob))
      (with start = (loc/p mob))
      (with best-goal = nil)
      (with best-path = nil)
      (with best-cost = nil)
      (for goal :in goals)
      (for path = (astar :start start
                         :neighbors #'open-neighbors
                         :goalp (curry #'= goal)
                         :cost (curry #'step-cost start)
                         :limit best-cost
                         :heuristic (curry #'distance goal)
                         :test #'eql))
      (when path
        (for cost = (length path))
        (when (or (null best-path) ; this is the first path
                  (< cost best-cost) ; this is a shorter path
                  (p< goal best-goal)) ; this is a better destination by reading order
          (setf best-path path
                best-goal goal
                best-cost cost)))
      (finally (return (if best-path
                         (values :move (second best-path))
                         (values :wait)))))))


(defun tick-mob (mob)
  (unless (member mob *dead*)
    (multiple-value-bind (action target) (action mob)
      (ecase action
        (:attack
          (pr mob 'attacking target)
          (attack mob target))
        (:move
          (pr mob 'moving 'to target)
          (move mob target))
        (:wait
          (pr mob 'waiting))))))


(defun tick-world ()
  (let ((*dead* nil))
    (map nil #'tick-mob (entities-to-move))))


;;;; World Generation ---------------------------------------------------------
(defun generate-world (lines)
  (removef lines "" :test #'string=)
  (beast:clear-entities)
  (let* ((rows (length lines))
         (cols (length (first lines))))
    (initialize-loc rows cols)
    (initialize-terrain rows cols)
    (iterate
      (for line :in lines)
      (for row :from 0)
      (iterate
        (for char :in-string line :with-index col)
        (for p = (p row col))
        (case char
          (#\E (create-elf p))
          (#\G (create-goblin p))
          (#\# (setf (terrain p) #\#))
          (t nil))))))

(defun print-world (&optional path)
  (iterate
    (for (char row col) :in-array *terrain*)
    (for p = (p row col))
    (for mob = (loc p))
    (when (zerop col)
      (terpri))
    (write-char (cond
                  ((member p path) #\x)
                  (mob (renderable/glyph mob))
                  (t char))))
  (terpri)
  (values))


(define-problem (2018 15) (data read-lines) ()
  (generate-world data)
  (print-world))
