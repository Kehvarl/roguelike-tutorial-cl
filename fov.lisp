(in-package #:roguelike-tutorial-cl)

(defparameter *fov-distance* 5)

(defun reset-visibility (map)
  (map-tiles-loop (map tile)
    (setf (tile/visible tile) nil)))

(defun fov (map x y)
  (declare (ignorable x y))
  (reset-visibility map))
