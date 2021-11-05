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

Now we can find a random tile in a room and place an entity there!  Let's actually use these tools in our `make-map` function:
```lisp
(defgeneric make-map (map max-rooms room-min-size room-max-size map-width map-height player entities max-enemies-per-room))
(defmethod make-map ((map game-map) max-rooms room-min-size room-max-size map-width map-height player entities max-enemies-per-room)
  (do* ((rooms nil)
  ;;;... Existing code...
    (place-entities map new-room entities max-enemies-per-room)
    (if (null rooms)
      (setf rooms (list new-room))
      (push new-room (cdr (last rooms))))
    (incf num-rooms)))))
```
We've had to redefine our generic since we need 2 new variables: `entities` and `max-enemies-per-room`.   Then we just call our new method and when we test...   Oh right, we haven't updated our `main` to pass those new values to our `make-map`.  Let's do that now!

```lisp
(make-map map *max-rooms* *room-min-size* *room-max-size* *map-width* *map-height* player entities *max-enemies-per-room*)
```
And now let's define that new constant right up near the top of our main file:
```lisp
(defparameter *max-enemies-per-room* 5)
```

And voila!  Monsters abound!
![I was working in the lab](../screenshots/part-5-2-monsters.gif?raw=true "late one night")

## Colliding with the Enemy
So far we've found a way to draw on the screen, created an entity class, made a way for our player to move around, given the player a place to explore, and populated that place with horrible creatures out of the depths of our twisted imaginations.   Let's make it so the player can't walk through those enemies!

For starters, we'll make an adjustment to our `entity` class so that we can decide if they block a tile or not.
```lisp
(defclass entity ()
  ((x :initarg :x :accessor entity/x)
   (y :initarg :y :accessor entity/y)
   (char :initarg :char :accessor entity/char)
   (color :initarg :color :accessor entity/color)
   (blocks :initarg :blocks :accessor entity/blocks :initform nil)))
```
Here we're adding a `blocks` slot to our entities.  By default they're ghostly apparitions that we can walk right through.

While we're in here, let's add a routine to check if there is a blocking entity at a given location:
```lisp
(defun blocking-entity-at (entities x y)
  (dolist (entity entities)
    (if (and (= (entity/x entity) x)
             (= (entity/y entity) y)
             (entity/blocks entity))
      (return entity))))
```

Next we update the places where we call `make-instance 'entity`, such as our `main` function:
```lisp
;;...
(let* (( player (make-instance 'entity
                        :x (/ *screen-width* 2)
                        :y (/ *screen-height* 2)
                        :char #\@
                        :color (blt:white)
                        :blocks t))
       (entities (list player))
;;...
```
You'll note I've also finally taken away that NPC we don't need anymore.

We also need to update our calls where we create the monsters:
```lisp
;;...
(nconc entities (list (make-instance 'entity :x x :y y :color  (blt:green) :char #\o :blocks t)))
(nconc entities (list (make-instance 'entity :x x :y y :color  (blt:yellow) :char #\T :blocks t)))
;;...
```

And now every entity we currently spawn has a tag saying it should block movement.  The big problem now is that nothing implements that feature.  So let's pop over to our `game-tick` and change the player movement routine:

```lisp
(defun game-tick (player entities map)
  (render-all entities map)
  (let* ((action (handle-keys))
         (move (getf action :move))
         (exit (getf action :quit)))

    (when move
      (let ((destination-x (+ (entity/x player) (car move)))
            (destination-y (+ (entity/y player) (cdr move))))
        (unless (blocked-p map destination-x destination-y)
          (let ((target (blocking-entity-at entities destination-x destination-y)))
            (cond (target
                   (format t "You kick the enemy.~%"))
                  (t
                   (move player (car move) (cdr move))
                   (fov map (entity/x player) (entity/y player))))))))
    exit))
```

And now if you run the game and try to walk over an enemy, you get a message:
![These feet were made for walking](../screenshots/part-5-4-kick.gif?raw=true "And I'm all outta bubblegum.")

While running around and bullying enemies by kicking them has its high points, the message isn't that great.  For one thing we don't even know what enemy we kicked!  Let's fix that:

We'll start by modifying our `entity` class again:
```lisp
(defclass entity ()
  ((name :initarg :name :accessor entity/name)
   (x :initarg :x :accessor entity/x)
   (y :initarg :y :accessor entity/y)
   (char :initarg :char :accessor entity/char)
   (color :initarg :color :accessor entity/color)
   (blocks :initarg :blocks :accessor entity/blocks :initform nil)))
```
... and adding a slot for the entity name!
