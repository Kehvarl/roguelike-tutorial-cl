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
Nothing really new here.  We're using our magic macro to loop through just the tiles we're interested in and setting some values on them.