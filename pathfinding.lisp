(in-package #:roguelike-tutorial-cl)

(defparameter *all-ditections*
  (list (cons 0 -1)
        (cons 0 1)
        (cons -1 0)
        (cons 1 0)
        (cons -1 -1)
        (cons -1 1)
        (cons 1 -1)
        (cons 1 1)))

(defclass node ()
  ((g :initform 0 :accessor node/g)
   (h :initform 0 :accessor node/h)
   (f :initform 0 :accessor node/f)
   (distance-from-parent :initarg :distance-from-parent :accessor node/distance-from-parent)
   (parent :initarg :parent :initform nil :accessor node/parent)
   (position :initarg :position :initform nil :accessor node/position)))

(defmethod print-object ((obj node) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (position parent) obj
      (format stream "~A, parent ~A" position parent))))

(defmethod node-equal ((n1 node) n2)
  "Return T if N1 and N2 have the same POSITION"
 (equal (node/position n1) (node/position n2)))

(defmethod node-compare ((n1 node) n2)
  "Return T if N1's F slot is less than N2's F slot"
  (< (node/f n1) (node/f n2)))

(defun find-in-queue (queue n)
  "Find the node N in the QUEUE by it's position.  If there are multiple
  nodes with the same position, return the LAST matching node."
  (let ((node nil))
    (queue:map-queue #'(lambda (item)
                         (when (node-equal n item)
                           (setf node item)))
                     queue)
    node))
