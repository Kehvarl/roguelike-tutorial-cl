# Part 3 - Generating a dungeon
 In the previous part we manually created a wall, and left the rest of the map open.  In this part we will perform some experiments that culminate in randomly generating a dungeon every time we start a new game!

## Changing Some Defaults
Instead of starting with an empty map and filling it with walls, we will start with a map completely made of walls and then carve our some open spaces to play in.
```lisp
(dotimes (y (game-map/h map))
  (dotimes (x (game-map/w map))
     (setf (aref (game-map/tiles map) x y) (make-instance 'tile :blocked t))))
```
This was a one-line change.   Specifically, we changed our `make-instance` and added the initarg `:blocked t` to initialize all tiles as `blocked` when we create a map.

If you compile this change and run your code, the player character will now be trapped in one place forever.

![All walls and nowhere to go](../screenshots/part-3-1-all-walls.png?raw=true "Instant Claustrophobia")

## Lisp's superpower
When we initialize our game-map, and then again when we render it, we're doing the same nested loop.   As we add more features, we're going to use this construct more in our program.
Rather than writing it every time, or creating a function that can only do one thing with our map, we're going to leverage Lisp's super power to generate code for us anytime we need it:  the macro.  

In our `game-map.lisp` file let's create a macro to handle iterating over tiles in our map.
```lisp

(defmacro map-tiles-loop ((map tile-val
                           &key (row-val (gensym)) (col-val (gensym))
                                (x-start 0) (y-start 0)
                                (x-end nil) (y-end nil))
                          &body body)
  `(loop :for ,col-val :from ,x-start :below (if (null ,x-end) (game-map/w ,map) ,x-end)
         :do
           (loop :for ,row-val :from ,y-start :below (if (null ,y-end) (game-map/h ,map) ,y-end)
                 :do
                    (let ((,tile-val (aref (game-map/tiles ,map) ,col-val ,row-val)))
                      (declare (ignorable ,tile-val))
                      ,@body))))
```

This arcane collection of words and symbols does some interesting magic.  Let's take it apart:
* `(defmaco map-tiles-loop ` Cerate a new macro named `map-tiles-loop`
  * `(map tile-val` Our macro expects a map and the variable name to use for tiles as we reference them
  * `&key` We also have some other arguments we pass in as keywords
    * `(row-val (gensym))` The variable name to use for the row iteration.  If empty, we'll use `gensym` to create a unique name
    * `(col-val (gensym))` The variable name to use for column iteration.
    * `(x-start 0) (y-start 0)` Our starting X and Y values, allowing us to address any arbitrary rectangular region in our map.  By default 0,0
    * `(x-end nil) (y-end nil))` The end x and y values.  By default `nil` which will prompt us to use the entire map size
  * `&body body` Capture the body that is passed to our macro and stor it in the variable `body` for later reference

* `\`(loop :for ,col-val :from ,x-start :below (if (null ,x-end) (game-map/w ,map) ,x-end)`  Here we use the `loop` construct instead of `dotimes`.  The very first character is a back-quote: "\`" which quotes the expression, but allows un-quoting when we want to reference a variable
  * `:for ,col-val` Our loop will use the `:for` method to iterate from a starting value to an ending value and keep our count in a provided variable.  In this case we "unquote" `col-val` using the comma operator.  This lets our macro use the value stored in `col-val` as the variable name for the iterator.
  * `:from ,x-start` Start our For loop at our `x-start` value  (0 by default)
  * `below (if (null ,x-end) (game-map/w ,map) ,x-end)`  If we have no `x-end` value, then count up to the map width.  Otherwise count up to the value provided.
  * `:do` The body of our loop

* `(loop :for ,row-val :from ,y-start :below (if (null ,y-end) (game-map/h ,map) ,y-end)` We're still inside our back-quoted form, so we just create a second loop using the same process as the `x` loop.

* `(let ((,tile-val (aref (game-map/tiles ,map) ,col-val ,row-val)))` Get the current tile at the X and Y position stored in the variables whose names we hid inside `col-val` and `row-val`, then store that tile reference in the variable whose name we hold in `tile-val`
* `(declare (ignorable ,tile-val))` If we don't ever actually use the variable holding our tile reference we don't want an error, so we tell Lisp that it's fine either way.
* `,@body))))` Paste in our body here at the innermost level of our loop.  

### Using our ~~Spell~~ Macro
Now we'll use our nice new macro to cleanup our `initialize-tiles` and get rid of the nested `dotimes`.
```lisp
(defmethod initialize-tiles ((map game-map))
  (map-tiles-loop (map tile :col-val x :row-val y)
    (setf (aref (game-map/tiles map) x y) (make-instance 'tile :blocked t))))
```
Other than our new macro, there's very little to see here:
* `(map-tiles-loop (map tile :col-val x :row-val y)` Call `map-tiles-loop`
  * `map` Pass in the game-map so we'll have something to work on
  * `tile` Tell the macro that we'll be using the name `tile` to refer to each tile in the map
  * `:col-val x` Tell the macro we'll use the variable `x` to refer to the x-position in our loop
  * `:row-val y` Tell the macro we'll use the variable `y` to refer to the y-position in our loop
* `(setf (aref (game-map/tiles map) x y) (make-instance 'tile :blocked t))))`  The body function to use inside our macro.  Basically, what we want to do inside our nested loops.

We will use this a lot more later, but for now we'll just make the one change.

## Defining Rooms and Tunnels
Now that our map is a solid, impassible mass; let us carve out some places to be.  First we're going to define a helper class that just holds the description of a room:  A rectangular region of our map that does not overlap with another rectangular region.

```lisp
(defclass rect ()
  ((x1 :initarg :x1 :accessor rect/x1)
   (x2 :initarg :x2 :accessor rect/x2)
   (y1 :initarg :y1 :accessor rect/y1)
   (y2 :initarg :y2 :accessor rect/y2)))

(defmethod initialize-instance :after ((rect rect) &key x y w h)
  (with-slots (x1 x2 y1 y2) rect
    (setf x1 x
          y1 y
          x2 (+ x w)
          y2 (+ y h))))
```
Once again there's nothing actually new being defined:  We create a class that lets us tract the corners of a rectangular region of our map, and we created an `initialize-instance` method that sets those corners, given a starting point along with a known width and height.

We'll also add some methods that will help us carve those rooms into our map.
First on the `tile` class:
```lisp
(defmethod set-tile-slots ((tile tile) &key (blocked nil blocked-supplied-p) (block-sight nil block-sight-supplied-p))
  (if blocked-supplied-p
      (setf (slot-value tile 'blocked) blocked))
  (if block-sight-supplied-p
      (setf (slot-value tile 'block-sight) block-sight)))
```
Here we introduce a special feature of keyword arguments:
* `(blocked nil blocked-supplied-p)`
  * `blocked` - The keyword argument.  Is set by using `:blocked value` when calling our method, and holds the given value.
  * `nil` - Our default value if we don't use the keyword argument.
  * `block-supplied-p` a predicate value that is set to `t` if we passed in a value, and `nil` if we're using the default value.   This is a way to tell the difference between a default `nil` and an intentional `nil`

Our second method is on `game-map`:
```lisp
(defmethod create-room ((map game-map) (room rect))
  (map-tiles-loop (map tile
                       :x-start (1+ (rect/x1 room)) :x-end (rect/x2 room)
                       :y-start (1+ (rect/y1 room)) :y-end (rect/y2 room))
    (set-tile-slots tile :blocked nil :block-sight nil)))
```
Nothing really new here.  We're using our magic macro to loop through just the tiles we're interested in and setting some values on them. There is one consideration to bear in mind:
* `:x-start (1+ (rect/x1 room)) :x-end (rect/x2 room)`  As you can see, we're adding 1 to the starting cell, and if you go review our macro, the loop counts up to the target number, but doesn't include it. In this way we end up with a 1-cell border around our room.

Now let's create a couple of test rooms:
```lisp
(defmethod make-map ((map game-map))
  (let ((room-1 (make-instance 'rect :x 20 :y 15 :w 10 :h 15))
        (room-2 (make-instance 'rect :x 35 :y 15 :w 10 :h 15)))
    (create-room map room-1)
    (create-room map room-2)))
```
Since our two rooms don't need to actually reference each other we can use `let` for our local variable binding instead of `let*`.

And now to use our map, we need to make a change in our `main` function
```lisp
(setf *map* (make-instance 'game-map :w *map-width* :h *map-height*))
(initialize-tiles *map*)
(make-map *map*)
```
Compile and load our changes, and we now how a pair of rooms on our map!

![Miners need not apply](../screenshots/part-3-2-rooms.gif?raw=true "Making maps the hard way")

## Some cleanup and improvements
Our `main` function is running the risk of getting unmanageably large, and we're relying on a lot of the loop's special syntax.  Before we move on, we'll take a few moments to clean up our code and make continued work easier.

First up we'll move the innards of our loop to a new function named `game-tick`:
```lisp
(defun game-tick (player entities map)
  (render-all entities map)
  (let* ((action (handle-keys))
         (move (getf action :move))
         (exit (getf action :quit)))

    (when move
      (unless (blocked-p map
                         (+ (entity/x player) (car move))
                         (+ (entity/y player) (cdr move)))
        (move player (car move) (cdr move))))

    exit))
```

Now that we have a function to handle each frame of our world's existence, we modify `main` to use that.  We'll also lose our complicated `loop` since it's not easy to maintain:
```lisp
(defun main()
  (blt:with-terminal
    (config)
    (let* (( player (make-instance 'entity
                            :x (/ *screen-width* 2)
                            :y (/ *screen-height* 2)
                            :char #\@
                            :color (blt:white)))
           (npc  (make-instance 'entity
                                :x (- (/ *screen-width* 2) 5)
                                :y (/ *screen-height* 2)
                                :char #\@
                                :color (blt:yellow)))
           (entities (list player npc))
           (map (make-instance 'game-map :w *map-width* :h *map-height*)))
      (initialize-tiles map)
      (make-map map)
      (do ((exit nil (game-tick player entities map)))
          (exit)))))
```

The new `main` is much streamlined:  We're using `let*` to define some local variables, and then inside that we're using `do` to run the actual game

* `(do ((exit nil (game-tick player entities map))) (exit))`
  * `do` Perform a loop until some exit condition happens
  * `((exit nil (game-tick player entities map)))` - the "varlist" holding all the variables the loop will work with.
    * `exit nil (game-tick player entities map)`
      * Define a variable named `exit`
      * `exit` starts out as `nil`
      * Each cycle through the loop, set `exit` to the outcome of calling `game-tick`.
  * (exit) - The "endlist", a list of conditions and values at the end of the loop.  The very first value in this list is the "end-test-form", when it is `T`, the loop ends.

Next, we'll update our `game-map`'s `initialize-instance` to set up the tiles for us.  We could just make that function call `initialize-tiles` like so:
```lisp
(defmethod initialize-instance :after ((map game-map) &rest initargs)
  (declare (ignore initargs))
  (setf (game-map/tiles map) (make-array (list (game-map/w map)
                                               (game-map/h map))))
  (initialize-tiles map))
```
However, as `initialze-tiles` is a relatively small function, we'll just inline it into our function like so:
```lisp
(defmethod initialize-instance :after ((map game-map) &rest initargs)
  (declare (ignore initargs))
  (setf (game-map/tiles map) (make-array (list (game-map/w map)
                                               (game-map/h map))))
  (map-tiles-loop (map tile :col-val x :row-val y)
    (setf (aref (game-map/tiles map) x y) (make-instance 'tile :blocked t))))
```

And just to make it a little more flexible, we'll make the initial tile state an argument:
```lisp
(defmethod initialize-instance :after ((map game-map)
                                       &rest initargs
                                       &key (initial-blocked-value t))

  (declare (ignore initargs))
  (setf (game-map/tiles map) (make-array (list (game-map/w map)
                                               (game-map/h map))))
  (map-tiles-loop (map tile :col-val x :row-val y)
    (setf (aref (game-map/tiles map) x y)
          (make-instance 'tile :blocked initial-blocked-value))))
```
Now we automatically initialize the tiles when we create the map.  By default they're initialized as blocking tiles, but we can just pass in the keyword `:initial-blocked-value` to force them to start in either state.

## Corridors
Unless our rooms end up overlapping, we currently have no way to move from room to room.  We will implement some functions to carve corridors that will link rooms for us:
```lisp
(defmethod create-h-tunnel ((map game-map) x1 x2 y)
  (let ((start-x (min x1 x2))
        (end-x (max x1 x2)))
    (map-tiles-loop (map tile
                     :x-start start-x :x-end (1+ end-x)
                     :y-start y :y-end (1+ y))
      (set-tile-slots tile :blocked nil :block-sight nil))))

(defmethod create-v-tunnel ((map game-map) y1 y2 x)
  (let ((start-y (min y1 y2))
        (end-y (max y1 y2)))
    (map-tiles-loop (map tile
                     :x-start x :x-end (1+ x)
                     :y-start start-y :y-end (1+ end-y))
      (set-tile-slots tile :blocked nil :block-sight nil))))
```

Using these methods we can add tunnels to connect our rooms. Let's test it in our `make-map` method
```lisp
(defmethod make-map ((map game-map))
  (let ((room-1 (make-instance 'rect :x 20 :y 15 :w 10 :h 15))
        (room-2 (make-instance 'rect :x 35 :y 15 :w 10 :h 15)))
    (create-room map room-1)
    (create-room map room-2))
  (create-h-tunnel map 25 40 23))
```

After we compile all our changes and run our game, we can now move between our two rooms!

![Corridors](../screenshots/part-3-4-exploring.gif?raw=true "Giving us more to explore")

### Keeping Track
Now we can carve rooms and corridors into our map.  There are several ways we can keep track of what tiles make up a given room, but one of the most flexible will be to have the tile itself hold an identifier telling us what room it's part of.

To that end, we'll jump into our `game-map.lisp` file and make some adjustments
First to our `tile` definition, we add a `room-index`
```lisp
(defclass tile ()
  ((room-index :initarg :room-index
               :accessor tile/room-index
               :initform nil)
   (blocked :initarg :blocked
            :accessor tile/blocked
            :initform nil)
   (block-sight :initarg :block-sight
                :accessor tile/block-sight
                :initform nil)))
```

And we allow our `set-tile-slots` method to modify the `room-index`
```lisp
(defmethod set-tile-slots ((tile tile) &key (blocked nil blocked-supplied-p)
                                            (block-sight nil block-sight-supplied-p)
                                            (room-index nil room-index-supplied-p))
  (if blocked-supplied-p
      (setf (slot-value tile 'blocked) blocked))
  (if block-sight-supplied-p
      (setf (slot-value tile 'block-sight) block-sight))
  (if room-index-supplied-p
      (setf (slot-value tile 'room-index) room-index)))
```

And then a quick change to `create-room` to allow us to give each room it's index, like so:
```lisp
(defgeneric create-room (map rect room-index))
(defmethod create-room ((map game-map) (room rect) room-index)
  (map-tiles-loop (map tile
                       :x-start (1+ (rect/x1 room)) :x-end (rect/x2 room)
                       :y-start (1+ (rect/y1 room)) :y-end (rect/y2 room))
    (set-tile-slots tile :blocked nil :block-sight nil :room-index room-index)))
```
Since we're changing the number of arguments that a method expects, we either need to restart Lisp and reload our project, or we can redefine the generic function that our method implements.
* `(defgeneric create-room (map rect room-index))` define a generic function that expects 3 parameters.

![Redefining Generic](../screenshots/part-3-5-redefine-generic.png?raw=true "Changing the nature of reality.")
When we compile this new defgeneric (ALT+c), we'll be prompted with an error message.  Select the option to "Remove all methods", then compile the generic again, and it will succeed.
Now we can compile our upgraded `create-room` without issue.

## Random Generation
Now that we have the tools to carve out rooms and tunnels, we're nearly ready to create some random dungeons to explore.

We'll start by creating a couple more utility methods, this time on `rect`.  Specifically we want to be able to fine the center of any rect as well as determine if two rects would intersect.

```lisp
(defmethod center ((rect rect))
  (with-slots (x1 x2 y1 y2) rect
    (let ((center-x (round (/ (+ x1 x2) 2)))
          (center-y (round (/ (+ y1 y2) 2))))
      (values center-x-center-y))))
```
This simple method introduces the `values` function, which lets us return multiple values from a method instead of just one

```lisp
(defmethod intersect ((rect rect) (other rect))
  "Returns T if this RECT intersects with OTHER"
  (and (<= (rect/x1 rect) (rect/x2 other))
       (>= (rect/x2 rect) (rect/x1 other))
       (<= (rect/y1 rect) (rect/y2 other))
       (>= (rect/y2 rect) (rect/y1 other))))
```

Now we're finally ready to build a simple random dungeon generator.

Over in our main file, we'll set up some global parameters:
First: find and remove `(defparameter *map* nil)` since we no longer need a global map.
Next add the following map-generation parameters to our main file.
```lisp
(defparameter *room-max-size* 10)
(defparameter *room-min-size* 6)
(defparameter *max-rooms* 30)
```

Now we're ready to implement a simple generator.  Over in our `game-map` file, let's update `make-map`:
```lisp
(defmethod make-map ((map game-map) max-rooms room-min-size room-max-size map-width map-height player)
  (do* ((rooms nil)
        (num-rooms 0)
        (room-index 0 (1+ room-index))
        (w (+ (random (- room-max-size room-min-size)) room-min-size)
           (+ (random (- room-max-size room-min-size)) room-min-size))
        (h (+ (random (- room-max-size room-min-size)) room-min-size)
           (+ (random (- room-max-size room-min-size)) room-min-size))
        (x (random (- map-width w))
           (random (- map-width w)))
        (y (random (- map-height h))
           (random (- map-height h)))
        (new-room (make-instance 'rect :x x :y y :w w :h h)
                  (make-instance 'rect :x x :y y :w w : h h))
        (can-place-p t t))
       ((>= room-index max-rooms))
       ()))
```     
First we redefine our method to take some extra parameters.  Then we're going to use a `do*` loop to create rooms up to our maximum number.  For each room we'll pick a random position on the map, and some dimensions for the room itself, and store that in a `rect` object.  We're also defining a `can-place` variable which will eventually help us determine legal room placement before we write them to the map.

Next we'll expand on our loop:
```lisp
(defmethod make-map ((map game-map) max-rooms room-min-size room-max-size map-width map-height player)
  (do* ((rooms nil)
        (num-rooms 0)
        (room-index 0 (1+ room-index))
        (w (+ (random (- room-max-size room-min-size)) room-min-size)
           (+ (random (- room-max-size room-min-size)) room-min-size))
        (h (+ (random (- room-max-size room-min-size)) room-min-size)
           (+ (random (- room-max-size room-min-size)) room-min-size))
        (x (random (- map-width w))
           (random (- map-width w)))
        (y (random (- map-height h))
           (random (- map-height h)))
        (new-room (make-instance 'rect :x x :y y :w w :h h)
                  (make-instance 'rect :x x :y y :w w : h h))
        (can-place-p t t))
       ((>= room-index max-rooms))
       (dolist (other room rooms)
          (if (intersect new-room other-room)
            (setf can-place-p nil)))))
```
We'll loop through our list of rooms, and if our new room intersects with any, it's an invalid room.  If the room is valid though, let's plot it and draw some tunnels!

```lisp
(defgeneric make-map (map max-rooms room-min-size room-max-size map-width map-height player))
(defmethod make-map ((map game-map) max-rooms room-min-size room-max-size map-width map-height player)
  (do* ;;existing code...
       (dolist (other-room rooms)
          (if (intersect new-room other-room)
            (setf can-place-p nil)))
       (when can-place-p
         (create-room map new-room room-index)
         (multiple-value-bind (new-x new-y) (center new-room)
           (if (zerop num-rooms)
               (setf (entity/x player) new-x
                     (entity/y player) new-y)
               (multiple-value-bind (prev-x prev-y) (center (car (last rooms)))
                  (cond ((= (random 2) 1)
                         (create-h-tunnel map prev-x new-x prev-y)
                         (create-v-tunnel map prev-y new-y new-x))
                        (t
                         (create-v-tunnel map prev-y new-y new-x)
                         (create-h-tunnel map prev-x new-x prev-y)))))
           (if (null rooms)
             (setf rooms (list new-room))
             (push new-room (cdr (last rooms))))
           (incf num-rooms)))))
```

A quick update to our `main` loop's call to `make-map`:
```lisp
(make-map map *max-rooms* *room-min-size* *room-max-size* *map-width* *map-height* player)
```

And we have a random dungeon!

![aMAZEing](../screenshots/part-3-6-mazes.gif?raw=true "Hope you brought a ball of string.")
