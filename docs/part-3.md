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
