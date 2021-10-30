#Part 5 - Placing enemies and kicking them (harmlessly)

## Placing Enemies
While we could come up with some complicated way to decide what enemies to place where, we'll start by placing a few randomly around the map.

First up, we a way to check for an entity already in a location, that way we don't stack them atop each other.   Over in our `game-map`, let's add a new function:
```lisp
(defun entity-at (entities x y)
  (dolist (entity entities)
    (if (and (= (entity/x entity) x)
             (= (entity/y entity) y))
      (return entity))))
```
This simply loops through the list of entities and returns the first one where the `x` and `y` positions match the target position.  In fact, it doesn't even use our map, and we could probably make it part of the `entity` file or even our `entity` class.  For now, it is fine in `game-map` where we'll be using it.

Now that we can check for conflicts, let's spawn a few beasts:
```lisp

(defmethod place-entities ((map game-map) (room rect) entities max-enemies-per-room)
  (let ((num-monsters (random max-enemies-per-room)))
    (dotimes (monster-index num-monsters)
      (let ((x (+ (random (round (/ (- (rect/x2 room) (rect/x1 room) 1) 2))) (1+ (rect/x1 room))))
            (y (+ (random (round (/ (- (rect/y2 room) (rect/y1 room) 1) 2))) (1+ (rect/y1 room)))))
        (unless (entity-at entities x y)
          (if (< (random 100) 80)
            (nconc entities (list (make-instance 'entity :x x :y y :color  (blt:green) :char #\o)))
            (nconc entities (list (make-instance 'entity :x x :y y :color  (blt:yellow) :char #\T)))))))))


```
There's nothing too fancy happening here:
* `(let ((num-monsters (random max-enemies-per-room)))` pick a random number of monsters between 0 and some max value.  Then loop that number of times
  * `(let ((x (+ (random ... (y (+ (random...` pick a random cell in the current room
  * `(unless (entity-at entities x y)`  If there's not already an enemy on the given tile
    * `(nconc entities (list (make-instance...` Create an entity and add it to that tile

Before we move this, let's make finding a random cell in a room part of the room itself:
```lisp
(defmethod random_cell ((rect rect))
  (with-slots (x1 x2 y1 y2) rect
    (let* ((w (- x2 x1 1))
           (h (- y2 y1 1))
           (x (+ (random w) (1+ x1)))
           (y (+ (random h) (1+ y1))))
      (values x y))))
```
It's a little more than 2 lines, but also a little easier to read

Using it is as simple as swapping our `let` for a `multiple-value-bind`:
```lisp
(defmethod place-entities ((map game-map) (room rect) entities max-enemies-per-room)
  (let ((num-monsters (random max-enemies-per-room)))
    (dotimes (monster-index num-monsters)
      (multiple-value-bind (x y) (random_cell room)
        (unless (entity-at entities x y)
          (if (< (random 100) 80)
            (nconc entities (list (make-instance 'entity :x x :y y :color  (blt:green) :char #\o)))
            (nconc entities (list (make-instance 'entity :x x :y y :color  (blt:yellow) :char #\T)))))))))

```
