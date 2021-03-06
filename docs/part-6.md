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

```lisp
(in-package #:roguelike-tutorial-cl)
```

Our implementation will need to know what directions might be valid, so we'll create a parameter to hold that for us.   If we wanted to restrict our AI to only be allowed to move orthogonally we'd just leave off the diagonals in this list:

```lisp
(defparameter *all-directions*
  (list (cons 0 -1)
        (cons 0 1)
        (cons -1 0)
        (cons 1 0)
        (cons -1 -1)
        (cons -1 1)
        (cons 1 -1)
        (cons 1 1)))
```

Next up we'll create the basic class that A* uses:  The Node.  A node tracks a few pieces of information:
* g: The distance from the current node to the start node
* h: The estimated distance from the current node to the end node (Heuristic)
  * calculated as the total change in x (squared), plus the total change in y (squared), from this cell to the goal:  `h = dx*dx + dy*dy`
* f: The total cost of the node.  `g + h`
* distance-from-parent: How many moves are requured to reach this node from the parent node
* parent: A reference to the parent node
* position: The node's X/Y location on the map

That's enough to create our `node` class, so let's do that!
```Lisp

(defclass node ()
  ((g :initform 0 :accessor node/g)
   (h :initform 0 :accessor node/h)
   (f :initform 0 :accessor node/f)
   (distance-from-parent :initarg :distance-from-parent :accessor node/distance-from-parent)
   (parent :initarg :parent :initform nil :accessor node/parent)
   (position :initarg :position :initform nil :accessor node/position)))
```

### Node Methods
Our new class can store data that will be very useful for our implementation, but we can benefit from some tools:
* print-object - A method to nicely print our Node objects in case we need to do some debugging.
* node-equal - Compares two nodes to determine if they have the same position
* node-compare - Return T if n1 has a lower `F` slot value than n2
* find-in-queue - scan through a queue and find the last node with the same position as the given node.


```lisp
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
    (queues:map-queue #'(lambda (item)
                         (when (node-equal n item)
                           (setf node item)))
                     queue)
    node))
```

### A* Helpers
Now we will set up a few methods that our A* implementation will use to determine what path to take:

```lisp
(defun create-path (current-node)
  (do ((path nil)
       (current current-node (node/parent current)))
      ((null current) (reverse path))
    (setf path (append path (list (node/position current))))))

(defun make-node (parent-node node-x node-y direction-from-parent)
  (let ((distance 10))
    (if (and (not (zerop (car direction-from-parent)))
             (not (zerop (cdr direction-from-parent))))
      (setf distance 14))
    (make-instance 'node :parent parent-node
                         :position (cons node-x node-y)
                         :distance-from-parent distance)))    

(defun generate-node-cost (child current-node end-node)
  (with-slots (g h f position distance-from-parent) child
    (setf g (+ distance-from-parent (node/g current-node))
          h (+ (expt (- (car position) (car (node/position end-node))) 2)
               (expt (- (cdr position) (cdr (node/position end-node))) 2))
          f (+ g h))))

(defun update-open-queue (open-list child-node)
  (let ((existing-child (find-in-queue open-list child-node)))
    (cond ((and existing-child (< (node/g child-node) (node/g existing-child)))
           (queues:queue-change open-list
                                (queues:queue-find open-list existing-child)
                                child-node))
          (t
            (queues:qpush open-list child-node)))))

(defun generate-node-children (current-node map open-list closed-list end-node)
  (dolist (new-position *all-directions*)
    (let ((node-x (+ (car (node/position current-node))
                     (car new-position)))
          (node-y (+ (cdr (node/position current-node))
                     (cdr new-position))))
      (unless (or (> node-x (1- (game-map/w map)))
                  (< node-x 0)
                  (> node-y (1- (game-map/h map)))
                  (< node-y 0))
        (unless (tile/blocked (aref (game-map/tiles map) node-x node-y))
          (let ((child (make-node current-node node-x node-y new-position)))
            (unless (find child closed-list :test 'node-equal)
              (generate-node-cost child current-node end-node)
              (update-open-queue open-list child))))))))
```

Now that we have the necessary tools, let's implement A* itself.   
```lisp
(defun astar (map start end)
  (let ((start-node (make-instance 'node :position start))
        (end-node (make-instance 'node :position end))
        (open-list (queues:make-queue :priority-queue :compare #'node-compare))
        (closed-list nil))
    (queues:qpush open-list start-node)
    (do ((current-node (queues:qpop open-list) (queues:qpop open-list)))
        ((null current-node))
      (setf closed-list (append closed-list (list current-node)))

      ;;Found the Goal
      (when (node-equal current-node end-node)
        (return-from astar (create-path current-node)))

      (generate-node-children current-node map open-list closed-list end-node))))
```

Having built ourselves a series of routines that implement A* navigation, we can update the `move-towards` method to use that instead of the current naive solution.  This will make the monsters much more dangerous.  So over in our `components` file:

```Lisp
(defmethod move-towards ((e entity) target-x target-y map entities)
  (with-slots (x y) e
    (let ((path (astar map (cons x y) (cons target-x target-y))))
      (when path
        (let ((next-location (nth 1 path)))
          (unless (blocking-entity-at entities (car next-location) (cdr next-location))
            (move e (- (car next-location) x) (- (cdr next-location) y))))))))
```

If we make sure everything is compiled, and run our game, the monsters now move much more intelligently:

![Welcome to the society](../screenshots/part-6-8-pathfinder.gif?raw=true "There can be no escape.")

## Combat
We have improved our movement options.  We have made our monsters smarter.  Now: let's get dangerous!

### The tools
We'll start by adding some methods to our `fighter` class over in `components`.

```Lisp
(defgeneric take-damage (component amount))

(defmethod take-damage ((component fighter) amount)
  (decf (fighter/hp component) amount))
```
This simple method means we just tell the `fighter` class to take some damage and it does the appropriate subtraction for us.

```Lisp
(defgeneric attack (component target))
(defmethod attack ((component fighter) (target entity))
  (let ((damage (- (fighter/power component) (fighter/defense (entity/fighter target)))))
    (cond
      ((> damage 0)
       (take-damage (entity/fighter target) damage)
       (format t "~A attackt ~A, and deals ~A point in damage.~%"
               (entity/name (component/owner component))
               (entity/name target)
               damage))
      (t
       (format t "~A attackt ~A, but does no damage.~%"
               (entity/name (component/owner component))
               (entity/name target))))))
```
Here we make the act of attack and defense fully encapsulated within `fighter` as well.   No need for our game loop to do the work for us, we just pass in some values and get a result we can use to keep the player updated on what's happening.

### Playing with our new toys
Now that our `fighter` component can handle attack, defense, and damage; we can replace those place-holder messages which hurt our feelings and turn us into bullies with actual combat.   

First, over in our main file's `game-tick` we update the player-move to handle the case where we would run into a monster:
```Lisp
...
(unless (blocked-p map destination-x destination-y)
  (let ((target (blocking-entity-at entities destination-x destination-y)))
    (cond (target
           (attack (entity/fighter player) target))
          (t
           (move player (car move) (cdr move))
           (fov map (entity/x player) (entity/y player))))
    (setf game-state :enemy-turn)))))
...
```

Next we hop back to `components` and update `take-turn` to change what happens when a monster bumps into the Player:

```Lisp
(defmethod take-turn ((component basic-monster) target map entities)
  (let* ((monster (component/owner component))
         (in-sight (tile/visible (aref (game-map/tiles map) (entity/x monster) (entity/y monster)))))
    (when in-sight
      (cond ((>= (distance-to monster target) 2)
             (move-towards monster (entity/x target) (entity/y target) map entities))
            ((> (fighter/hp (entity/fighter target)) 0)
             (attack (entity/fighter monster) target))))))
```

As you can see: in both places we simple replaced our placeholder message with a call to `attack` with the appropriate arguments.

We can run our game now and witness the carnage:
![Scenes of horror](../screenshots/part-6-10-senseless-violence.gif?raw=true "Locked in combat until the end of time.")

## Messages, Death, and Corpses
We've built a lot of pieces:  maps, entities, a component system, field-of-view, exploration, advanced pathfinding, and some rudimentary combat.  We have just a few more really core pieces remaining that will make it easier for us to work with game messages and the results of combat.

### Messages
The first thing we're going to implement is a message system.  This will come in handy later when we want a more useful in-game log.

Fortunately, we don't need to build any new tools to get started with that!  Instead, we'll change our approach:   Any time we would have displayed a message (using `format`), we will instead return a result to our main loop that it can use to display messages in one consistent place and fashion.

We'll start over in out `components` file, specifically with the `attack` and `take-damage` methods.

`take-damage`:
```Lisp
(defmethod take-damage ((component fighter) amount)
  (decf (fighter/hp component) amount)
  (let ((results nil))
    (when (<= (fighter/hp component) 0)
      (setf results (list :dead (component/owner component))))
    results))
```
When an entity takes some damage, we check to see if its HP falls to 0 or below.  If so then we will return a list containing the keyword `:dead` and a reference to the entity.

`attack`:
```Lisp
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
```
Once we calculate the results of an attack, there are two possible outcomes:
* The attacker deals some damage, in which case we will return a list that contains 2 parts:
  * A list that contains the keyword `:message` and a pre-formatted string to indicate the results of the attack
  * The results of calling `take-damage` on the target with the amount of damage to deal.
* A list containing the keyword `:message` and a pre-formatted string indicating that no damage was dealt to the target.

Our `take-turn` method on the `basic-monster` component uses the `attack` method, so let's update it to pass those messages back to the main loop.

`take-turn`:
```Lisp
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
```
Just 3 modifications here: add a `results` variable to our `let`, change the attack call to store its output in that `results` variable, and return `results` for later use.

And now we're ready to modify our game loop to deal with all these messages we're sending around:

`game-tick`
```Lisp
(defun game-tick (player entities map game-state)
  (declare (type game-states game-state))
  (render-all entities map)
  (let* ((player-turn-results nil)
         (action (handle-keys))
         (move (getf action :move))
         (exit (getf action :quit)))

    (when (and move (eql game-state :player-turn))
      (let ((destination-x (+ (entity/x player) (car move)))
            (destination-y (+ (entity/y player) (cdr move))))
        (unless (blocked-p map destination-x destination-y)
          (let ((target (blocking-entity-at entities destination-x destination-y)))
            (cond (target
                   (setf player-turn-results (attack (entity/fighter player) target)))
                  (t
                   (move player (car move) (cdr move))
                   (fov map (entity/x player) (entity/y player))))
            (setf game-state :enemy-turn)))))
    (when exit
      (setf game-state :exit))

    (let ((message (getf player-turn-results :message))
          (dead-entity (getf player-turn-results :dead)))
      (when message
        (format t message))
      (when dead-entity))
        ;; Nothing yet

;;;... Continued Below
```
For our first change to `game-tick` made sure to capture the messages sent from attacking.  If a message is set, we print it at the end of the player's turn.   We've also set ourselves up to do something with the knowledge that an entity died, but we're not ready to use that yet.

`game-tick`
```Lisp
;;;... Continued From above

    (when (eql game-state :enemy-turn)
      (dolist (entity (remove-if-not #'entity/ai entities))
        (let* ((enemy-turn-results (take-turn (entity/ai entity) player map entities))
               (message (getf enemy-turn-results :message))
               (dead-entity (getf enemy-turn-results :dead)))
          (when message
            (format t message))
          (when dead-entity)))
            ;; Nothing yet
      (setf game-state :player-turn)))

  game-state)
```
Once again we're capturing the messages returned to us by each entity, and printing those if there are any.  We'll do something with those `:dead` results soon.

### Death
Our next modification to the game is to handle all those entities that fall below 0 HP.   We already identify them and pass them up to our `take-turn` method so we can react to them, so now we need to build some tools to handle those reactions:

#### New File
We'll put all this work into a new file named `death-functions.lisp`  Go ahead and create that file, then add it to your .asd file.

`kill-player`
```lisp
(defun kill-player (player)
  (setf (entity/char player) #\%
        (entity/color player) (blt:red))
  (values "You died!" :player-dead))
```
This method accepts an entity, specifically the `player` entity, and sets the display character and color to something new that indicates a dead entity.  It then returns some text to display, and a keyword that we can use to set the game-state and end the game.

`kill-monster`
```lisp
(defun kill-monster (monster)
  (with-slots (char color blocks ai name) monster
    (let ((message (format nil "~A has died!~%" name)))
      (setf char #\%
            color (blt:red)
            blocks nil
            ai nil
            name (format nil "remains of ~A" name))
      message)))
```
Similar to the `kill-player` function, this one changes the symbol and color before returning some message about the dead monster. Since the game isn't over, we also make sure that corpses don't block tiles, don't try to take turns, and we update their name in case we need that later.

#### Update Placeholders
Now we have our tools for handling entity death, so we can return to our main file and replace those placeholders with calls to our new functions.

`player-turn`
```Lisp
    (let ((message (getf player-turn-results :message))
          (dead-entity (getf player-turn-results :dead)))
      (when message
        (format t message))
      (when dead-entity
        ;;Death Function Call Here
        (cond ((equal dead-entity player)
               (setf (values message game-state) (kill-player dead-entity)))
              (t
               (setf message (kill-monster dead-entity))))
        (format t message)))
```

`enemy-turn`
```Lisp
(when (eql game-state :enemy-turn)
  (dolist (entity (remove-if-not #'entity/ai entities))
    (let* ((enemy-turn-results (take-turn (entity/ai entity) player map entities))
           (message (getf enemy-turn-results :message))
           (dead-entity (getf enemy-turn-results :dead)))
      (when message
        (format t message))
      (when dead-entity
        ;;Death Function Call Here
        (cond ((equal dead-entity player)
               (setf (values message game-state) (kill-player dead-entity)))
              (t
               (setf message (kill-monster dead-entity))))
        (format t message)

        (when (eql game-state :player-dead)
          (return-from game-tick game-state)))))
```
If we run this we will get some errors because we haven't set things up to use that new `player-dead` state.   For a quick fix we'll make 2 changes:

`game-states`
```Lisp
(deftype game-states () '(member :player-turn :enemy-turn :exit :player-dead))
```
First we add the new state to our allowed game-states list.

`main`
```Lisp
(do ((game-state :player-turn (game-tick player entities map game-state)))
    ((or (eql game-state :exit)(eql game-state :player-dead)))))))
```    
Then we make sure that the game knows to exit when it receives a `:player-dead` state.

Compile it all, run main and...
![Enter The Macabre](../screenshots/part-6-12-death.gif?raw=true "Actions have consequences.")

## Game State
Before we move on, we should change how we handle the game state.  If we go back to our source documents we can see that they've implemented a whole new state system that we glossed over, and it's time to rectify that.   The new system will give us some more control over the game in its various states and make future expansion easier.
