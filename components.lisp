(in-package :roguelike-tutorial-cl)

(defclass component ()
  ((owner :initarg :owner :accessor component/owner)))

(defclass fighter (component)
  ((max-hp :initarg :max-hp :accessor fighter/max-hp :initform nil)
   (hp :initarg :hp :accessor fighter/hp)
   (defense :initarg :defense :accessor fighter/defense)
   (power :initarg :power :accessor fighter/power)))

(defgeneric take-damage (component amount))
(defmethod take-damage ((component fighter) amount)
  (decf (fighter/hp component) amount)
  (let ((results nil))
    (when (<= (fighter/hp component) 0)
      (setf results (list :dead (component/owner component))))
    results))

(defgeneric attack (component target))
(defmethod attack ((component fighter) (target entity))
  (let ((results nil)
        (damage (- (fighter/power component) (fighter/defense (entity/fighter target)))))
    (cond
      ((> damage 0)
       (setf results (append (list :message
                                   (format nil "~A attacks ~A, and deals ~A point in damage.~%"
                                             (entity/name (component/owner component))
                                             (entity/name target)
                                             damage))
                             (take-damage (entity/fighter target) damage))))

      (t
       (setf results (list :message (format nil "~A attacks ~A, but does no damage.~%"
                                            (entity/name (component/owner component))
                                            (entity/name target))))))
    results))

(defclass basic-monster (component) ())

(defgeneric take-turn (component target map entities))
(defmethod take-turn ((component basic-monster) target map entities)
  (let* ((results nil)
         (monster (component/owner component))
         (in-sight (tile/visible (aref (game-map/tiles map) (entity/x monster) (entity/y monster)))))
    (when in-sight
      (cond ((>= (distance-to monster target) 2)
             (move-towards monster (entity/x target) (entity/y target) map entities))
            ((> (fighter/hp (entity/fighter target)) 0)
             (setf results (attack (entity/fighter monster) target)))))
    results))

(defgeneric move-towards (e target-x target-y map entities))
(defmethod move-towards ((e entity) target-x target-y map entities)
  (with-slots (x y) e
    (let ((path (astar map (cons x y) (cons target-x target-y))))
      (when path
        (let ((next-location (nth 1 path)))
          (unless (blocking-entity-at entities (car next-location) (cdr next-location))
            (move e (- (car next-location) x) (- (cdr next-location) y))))))))
