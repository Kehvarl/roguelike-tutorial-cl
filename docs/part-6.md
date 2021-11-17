# Part 6 - Doing (and taking) some damage
So far we have created a window, figured out how to draw on it, created a random map, created a generic entity type which we can place anywhere on the map, and added turns for the player and the other entities.

Now we will add some interaction by making it possible to attack those other entities and do some damage to them (and eventually allowing those monsters to do damage right back and move around on their own).

## Components
The key to our new goal is the Component.  A component is a feature that our entities might have, but which not all entities actually have.  This allows us to use "composition" to build our entities out of the bits and pieces we want while giving us the flexibility to have only those pieces we want.

### New file
We will start with creating a new file named `components.lisp`  Don't forget to `in-package` it and add it to our .asd

### The Component class
For starters we'll create a generic `component` class that our other components can inherit from, so we can give them some consistency:
```lisp
(defclass component ()
  ((owner :initarg :owner :accessor component/owner)))
```
No new concepts here, but this is the core of our entire new composition system.

### The Fighter Component: HP, Attack, and Defense
Next up we will define the first component we'll actually use:
```lisp
(defclass fighter (component)
  ((max-hp :initarg :max-hp :accessor fighter/max-hp :initform nil)
   (hp :initarg :hp :accessor fighter/hp)
   (defense :initarg :defense :accessor fighter/defense)
   (power :initarg :power :accessor fighter/power)))
```

### The AI Component:  Do that voodoo that you do
Let's also create a component to represent AI behavior and a take-turn method for it:
```Lisp
(defclass basic-monster (component) ())
date
man date

qsudo date 111117562021
sudo apt uupdatesudo apt upgraded-array-element-typeps -ae

(defgeneric take-turn (component))
(defmethod take-turn ((component basic-monster))
  (format t "The ~A wonders when it will get to move.~%" (entity/name (component/owner component))))
```

### Using Components
Let's set up our entity to handle components:
```lisp
(defclass entity ()
  ((name :initarg :name :accessor entity/name)
   (x :initarg :x :accessor entity/x)
   (y :initarg :y :accessor entity/y)
   (char :initarg :char :accessor entity/char)
   (color :initarg :color :accessor entity/color)
   (blocks :initarg :blocks :accessor entity/blocks :initform nil)
   (fighter :initarg :fighter :accessor entity/fighter :initform nil)
   (ai :initarg :ai :accessor entity/ai :initform nil)))
```
Here we've added 2 new slots to our entity, and initialized them to `nil`.  We'll also create an after-initialize method to make components more useful:

```lisp
(defmethod initialize-instance :after ((entity entity) &rest initargs)
  (declare (ignore initargs))
  (with-slots (fighter ai) entity
    (when fighter
      (setf (component/owner fighter) entity))
    (when ai
      (setf (component/owner ai) entity))))
```

Here, when we create an entity, if we've set a fighter or ai component, we make sure to initialize those component's `owner` reference to the entity we created.  We can use this later to test for things and make changes.

And now we can change our Player entity to include a fighter component, like so:
```lisp
(let* ((fighter-component (make-instance 'fighter
                                         :hp 30
                                         :defense 2
                                         :power 5))
       (player (make-instance 'entity
                       :name "Player"
                       :x (/ *screen-width* 2)
                       :y (/ *screen-height* 2)
                       :char #\@
                       :color (blt:white)
                       :blocks t
                       :fighter fighter-component))
```

We'll do the same for those monsters we've scattered around the map:
```lisp
(defmethod place-entities ((map game-map) (room rect) entities max-enemies-per-room)
  (let ((num-monsters (random max-enemies-per-room)))
    (dotimes (monster-index num-monsters)
      (multiple-value-bind (x y) (random_cell room)
        (unless (entity-at entities x y)
          (cond ((< (random 100) 80)
                 (let* ((fighter-component (make-instance 'fighter :hp 10 :defense 0 :power 3))
                        (ai-component (make-instance 'basic-monster))
                        (orc (make-instance 'entity :name "Orc" :x x :y y :color  (blt:green) :char #\o :blocks t
                                                    :fighter fighter-component :ai ai-component)))
                   (nconc entities (list orc))))

                (t
                  (let* ((fighter-component (make-instance 'fighter :hp 16 :defense 1 :power 4))
                         (ai-component (make-instance 'basic-monster))
                         (troll (make-instance 'entity :name "Troll" :x x :y y :color  (blt:yellow) :char #\T :blocks t
                                                       :fighter fighter-component :ai ai-component)))
                    (nconc entities (list troll))))))))))
```

Next up we'll modify our game-tick to call the AI routine on our monsters so they can do things:
```lisp
(when (eql game-state :enemy-turn)
  (dolist (entity (remove-if-not #'entity/ai entities))
    (take-turn (entity/ai entity)))
```
The new concept here is in `(dolist (entity (remove-if-not #'entity/ai entities))`.  Instead of letting `dolist` use our entire list of entities, we first run that list through a macro that gives us just the entities that have an `ai` set.

If we run our game now, all the enemies complain about their lack of features:
![Turnabout](../screenshots/part-6-2-turns.gif?raw=true "Is fair play")

## AI
The `basic-monster` AI that we have now can take turns, but can't actually do anything yet.  Let's work on improving that.

We will modify `basic-monster` to move towards the player and then attack.  First of all we need a function that will let us figure out how close we are to the player. Since this is used by an entity we'll make it a method in the Entity file:

```lisp
(defmethod distance-to ((e entity) (other entity))
  (let ((dx (- (entity/x other) (entity/x e)))
        (dy (- (entity/y other) (entity/y e))))
    (sqrt (+ (expt dx 2) (expt dy 2)))))
```

Now that we have that tool, we will create a `move-towards` method on `basic-monster` so that it will move the entity towards a target location, moving in a straight line.  If it hits a wall it will stop.
```lisp
(defgeneric move-towards (e target-x target-y map entities))
(defmethod move-towards ((e entity) target-x target-y map entities)
  (with-slots (x y) e
    (let* ((dx (- target-x x))
           (dy (- target-y y))
           (distance (sqrt (+ (expt dx 2) (expt dy 2)))))
      (setf dx (round (/ dx distance))
            dy (round (/ dy distance)))
      (unless (or (blocked-p map (+ x dx) (+ y dy)))
        (move e dx dy)))))
```

Fantastic!  We now have the ability to move towards a point, and the ability to determine the distance between two entities.  Let's use these and make `take-turn` more exciting:
```lisp

(defgeneric take-turn (component target map entities))
(defmethod take-turn ((component basic-monster) target map entities)
  (let* ((monster (component/owner component))
         (in-sight (tile/visible (aref (game-map/tiles map) (entity/x monster) (entity/y monster)))))
    (when in-sight
      (cond ((>= (distance-to monster target) 2)
             (move-towards monster (entity/x target) (entity/y target) map entities))
            ((> (fighter/hp (entity/fighter target)) 0)
             (format t "The ~A insults you! Your ego is damaged!~%" (entity/name (component/owner component))))))))

```
Obviously we've had to redefine our arguments, so we updated the generic first, then the actual method.
On the method we're not exploring any new features, we're just performing a couple of simple checks:
* If a monster is in the current Field of View, then the monster is allowed a turn
* If the monster is 2 or more spaces from their target, they move towards that target in as close to a straight line as possible.
* If the monster is next to the target, and the target still has HP, then the monster attacks (for now, just insults) the target.

This is marvelous, but it won't work yet.  Let's fix our call to `take-turn` over in `game-tick`:
```lisp
(when (eql game-state :enemy-turn)
  (dolist (entity (remove-if-not #'entity/ai entities))
    (take-turn (entity/ai entity) player map entities))
  (setf game-state :player-turn)))
```

Basically, we just try to move towards and attack the player.
![I'm gonna find ya](../screenshots/part-6-4-stalking.gif?raw=true "I'm gonna getcha getcha getcha getcha")

### Movement
Currently the player can move in the 4 orthogonal directions, while the monsters can move in all 8 directions including the diagonals.  There are three ways we could resolve this:
* Restrict the monsters to the cardinal directions.
* Allow the player to move in all 8 directions.
* Leave it as is with a bit of unfairness in the monster's favor.

In order to maximize flexibility, we'll go with option B: allow the player to move in any direction. If we look into our `handle-keys` function, we can simply add some more keys to allow diagonals.  

It's popular to use the "Vim" keys for this:
```
     ^
 \   |   /
   y k u
<- h   l ->
   b j n
 /  |  \
    v
```

Unfortunately, I find this to be awkward and uncomfortable on my specific keyboard.  We could use the tenkey/number-pad, but a lot of people use tenkeyless keyboards or laptops.  

For me, I've found it easier to expand the well-known WASD keys for movement:
```
     ^
 \   |   /
   q w e
<- a   d ->
   z s c
 /  |  \
    v
```

If you find this doesn't work for you, feel free to experiment with options that feel comfortable and fast!

Before we add the diagonals, let's make a change to how `handle-keys` works.  
```lisp
(defun handle-keys ()
  (when (blt:has-input-p)
    (blt:key-case (blt:read)
                  ((or :up :w) (list :move (cons 0 -1)))
                  ((or :down :s) (list :move (cons 0 1)))
                  ((or :left :a) (list :move (cons -1 0)))
                  ((or :right :d) (list :move (cons 1 0)))
                  (:escape (list :quit t))
                  (:close (list :quit t)))))
```
This has a few changes:
* When we first enter the function we check to see if there's any input.  If not, jump back out.  This allows the game do do stuff instead of freezing and waiting for input.
* Instead of setting and returning an `action` variable, we just immediately return the action to perform.  This isn't essential, but it does clean things up a little.
* While we were at it, we added WASD movement.

Now we'll add diagonals:
```lisp
(defun handle-keys ()
  (when (blt:has-input-p)
    (blt:key-case (blt:read)
                  ((or :up :w) (list :move (cons 0 -1)))
                  ((or :down :s) (list :move (cons 0 1)))
                  ((or :left :a) (list :move (cons -1 0)))
                  ((or :right :d) (list :move (cons 1 0)))
                  (:q (list :move (cons -1 -1)))
                  (:e (list :move (cons 1 -1)))
                  (:z (list :move (cons -1 1)))
                  (:c (list :move (cons 1 1)))
                  (:escape (list :quit t))
                  (:close (list :quit t)))))
```
The actual key implementation is pretty straightforward here.
![The Oblique Approach](../screenshots/part-6-6-diagonal.gif?raw=true "Unnatural movements")

## Pathfinding
The way our monsters move isn't very helpful. In fact, they can easily be trapped against walls and allow the player to escape.

To improve our AI we will upgrade how it determines what the next direction to move will be, a process know as "pathfinding".  There are actually several pathfinding algorithms we can use, but A* (A-Star) is very popular for these sorts of games because it determines a pretty effective path to the target.

Some reference material on A* pathfinding:
* [Introduction to A* (Red Blob Games)](https://www.redblobgames.com/pathfinding/a-star/introduction.html)
* [A* implementation guide (Red Blob Games)](https://www.redblobgames.com/pathfinding/a-star/implementation.html)
* [Easy A* Pathfinding (Nicholas Swift)](https://medium.com/@nicholas.w.swift/easy-a-star-pathfinding-7e6689c7f7b2)

If you've taken the chance to look over those resources, or you're familiar with A* already, or you just want to get on with it: let's get to coding!

### New dependencies
We could implement A* using just Lisp primitives, but why make our life too much harder than we need to?   We'll use a priority queue, and so we will add a dependency to a library that gives us those.  We'll open up our `.asd` file and update the `:depends-on` line like so:
```lisp
:depends-on (#:cl-blt #:queues.priority-queue)
```
And so we can use it starting right away, let's load the system in our REPL:
```Lisp
(ql:quickload :queues.priority-queue)
```
![Queuing up](../screenshots/part-6-7-repl.png?raw=true "Walk the line")

### A whole new file
Pathfinding will take a bit of space, so let's put it into its own file right from the start.  Create a new file named `pathfinding.lisp` and add it to your package.  Don't forget to put it into your `.asd` file as well:
`.asd` includes:
```Lisp
:components ((:file "package")
             (:file "game-map")
             (:file "pathfinding")
             (:file "entity")
             (:file "components")
             (:file "fov")
             (:file "roguelike-tutorial-cl")))
```

`pathfinding.lisp`:
```
(in-package #:roguelike-tutorial-cl)
```
