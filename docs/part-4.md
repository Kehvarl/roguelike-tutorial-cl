# Part 4 - Field of view
In this part we're going to worry about the player's Field of View (FOV).  Which is the part of the map the player can actually see during any given round.  This will make the map itself more of a mystery and as we add enemies give them the chance to skulk around in the shodows.

## Calculating the Field of View
There are several ways to calculate what a player can see, and keep track of the results of those calculations.  For simplicity's sake, we're going to let the map itself keep track of those outcomes by storing them in the Tile.

### Update Tile Class
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
                :initform nil)
   (visible :initarg :visible
            :accessor tile/visible
            :initform nil)))
```
There are no new tricks here, we're just adding one more slot to our `tile` class and making sure it's set to `nil` by default.

### Organization
While FoV uses the game-map, we're not going to put it in the game-map file; both because that file is getting a little crowded, and because it's not necessarily part of the map.  If anything it's part of the player.

* Create a new file: `fov.lisp` and make sure its functions are included in our project
  * `(in-package #:roguelike-tutorial-cl) ;; First Line in all our project files`
* Add our file to our `.asd` so it will be loaded properly
  * `(:file "fov") ;; Insert after our game-map reference`

And now we're ready to get to writing code!

### Clearing the FoV
Before calculating a new Field of View, it's important to clear out the old one so that areas drop out of sight as the player moves away from them:
```lisp
(defparameter *fov-distance* 5)

(defun reset-visibility (map)
  (map-tiles-loop (map tile)
    (setf (tile/visible tile) nil)))

(defun fov (map x y)
  (declare (ignorable x y))
  (reset-visibility map))
```
Nothing unusual here:
* Define a global parameter to hold our visibility range
* Create a function that uses our magic macro to reset what has been seen
* Create a new function that will eventually calculate the FoV given an map and the player's position.  Since we're not using that position yet we tell Lisp it can be ignored so that everything compiles cleanly.


Next up we need some functions to handle our math:
```lisp
(defun degree-to-radian (degree)
  (* degree (/ pi 180)))

(defun diagonal-distance (x0 y0 x1 y1)
  (let ((dx (- x0 x1))
        (dy (- y0 y1)))
    (max (abs dx) (abs dy))))

(defun lerp (start end time)
  (+ start (* time (- end start))))
```

Here we're implementing some tools that we'll use in our field-of-view calculator:
* `degree-to-radian` - converts a value in degrees (0 to 360) into the number of radians around the arc of a circle that angle represents.
* `diagonal-distance` - A quick-and-dirty way to get the distance between 2 points on a grid is to simply find which axis had the most change and that's your distance.
* `lerp` - [The Linear Interpolation](https://en.wikipedia.org/wiki/Linear%5Finterpolation) function is used to take steps along a line on a grid and determine what cell we're actually in.


Now let's calculate our player's field ov view:
```Lisp
(defun fov (map x y)
  (reset-visibility map)

  (dotimes (degree 360)
    (let* ((rad (degree-to-radian degree))
           (nx (round (+ (* (cos rad) *fov-distance*) x)))
           (ny (round (+ (* (sin rad) *fov-distance*) y)))
           (d (diagonal-distance x y nx ny)))
      (dotimes (tile d)
        (let ((tx (round (lerp x nx (/ tile d))))
              (ty (round (lerp y ny (/ tile d)))))
          (if (or (< tx 0) (> tx (game-map/w map)))
            (return))
          (if (or (< ty 0) (> ty (game-map/h map)))
            (return))

          (when (tile/block-sight (aref (game-map/tiles map) tx ty))
            (setf (tile/visible (aref (game-map/tiles map) tx ty)) t)
            (return))

          (setf (tile/visible (aref (game-map/tiles map) tx ty)) t))))))
```

The algorithm itself is relatively simple, especially now that we have some helpers for the math:
* Reset all visibility
* For each of 360 degrees
  * Use some trigonometry to find the `nx,ny` coordinate of the tile on the outer edge of the circle representing `degree` degrees around the circle centered on `x,y`
  * Get the number of tiles from the player to that calculated position
  * For each of the count of tiles
    * Use our Linear Interpolation to determine the tile `tile` cells out along the line from `x,y` to `nx,ny`
    * If we run into a border of the map, exit our inner loop
    * If we run into a wall, make it visible, then exit our inner loop
    * Otherwise mark the tile visible and continue along the line.

### Rendering our Field of view
Now that we can calculate the FoV, we will adapt the renderer to do something with that new feature.
First, we define a couple of new colors in our color-map:
```lisp
(defparameter *color-map* (list :dark-wall (blt:rgba 0 0 100)
                                :dark-ground (blt:rgba 50 50 150)
                                :light-wall (blt:rgba 130 110 50)
                                :light-ground (blt:rgba 200 180 50)))
```

Next, we modify `render-all` to track the visibility of a tile:
```lisp
(defun render-all (entities map)
  (blt:clear)
  (dotimes (y (game-map/h map))
    (dotimes (x (game-map/w map))
       (let* ((tile (aref (game-map/tiles map) x y))
              (wall (tile/blocked tile))
              (visible (tile/visible tile)))
         (if visible
           (if wall
             (setf (blt:background-color) (getf *color-map* :light-wall))
             (setf (blt:background-color) (getf *color-map* :light-ground)))

           (if wall
            (setf (blt:background-color) (getf *color-map* :dark-wall))
            (setf (blt:background-color) (getf *color-map* :dark-ground)))))
       (setf (blt:cell-char x y) #\Space)))

  (mapc #'draw entities)

  (setf (blt:background-color) (blt:black))
  (blt:refresh))
```

We've only made a few changes here.
* `(visible (tile/visible tile))` Check the visibilty of each tile as we iterate
* `(if visible` Use that visibility state to change how we draw the tiles.

Lastly, but possibly the most important, we tell our `game-tick` to update the FoV every move:
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
        (move player (car move) (cdr move))
        (fov map (entity/x player) (entity/y player))))

    exit))
```

Extra credit:
If you ran the code already, you'll have noticed that there's no FoV drawn until the player moves for the first time.   To resolve that, we'll make a change to our `main`:
```lisp
(defun main()
;;; ...Lots of existing code...
      (fov map (entity/x player) (entity/y player))
      (do ((exit nil (game-tick player entities map)))
          (exit)))))
```

![Light up your life](../screenshots/part-4-2-fov.gif?raw=true "What a glowing personality you have.")

#Exploration
Now that we can light up the area around our player, let's make a few more changes.  Our players can still see the entire map right from the start, and monsters are still visible even if they're not in our Field of View.

## Fog of War
To fix the map visibility issue, let's implement some Fog of War!   Areas that haven't been explored will not be drawn at all, and areas we have explored, but which are outside of our field of view won't show if there are monsters or other things there.

### Tracking our Exploration
Fog of War requires some way for us to keep track of everything the player has seen.  To store this information, we'll make a new adjustment to our `tile` class:
```lisp
(defclass tile ()
  (;;... Existing code...
   (explored :initarg :explored
             :accessor tile/explored
             :initform nil)))
```
We'll just create a new slot to track the `explored` state of every tile, and start it out as `nil`.

Now that we can track the explored state, we need to make sure we maintain it.  Fortunately there's an easy way to do that!  We're already setting the state of every tile the player can see when they can see it.  We'll just update the explored state at the same time, like so:

```lisp
(defun fov (map x y)
  (reset-visibility map)

  (dotimes (degree 360)
        ;; ...Existing code...

        (when (tile/block-sight (aref (game-map/tiles map) tx ty))
          (setf (tile/visible (aref (game-map/tiles map) tx ty)) t)
          (setf (tile/explored (aref (game-map/tiles map) tx ty)) t)
          (return))

        (setf (tile/visible (aref (game-map/tiles map) tx ty)) t)
        (setf (tile/explored (aref (game-map/tiles map) tx ty)) t))))))
```

And finally, we need to draw the explored parts of the map while hiding those unexplored and mysterious regions.   To the `render-all` function!
```lisp
(defun render-all (entities map)
  (blt:clear)
  (dotimes (y (game-map/h map))
    (dotimes (x (game-map/w map))
       (let* ((tile (aref (game-map/tiles map) x y))
              (wall (tile/blocked tile))
              (visible (tile/visible tile))
              (explored (tile/explored tile)))
         (cond
           (visible
            (if wall
              (setf (blt:background-color) (getf *color-map* :light-wall))
              (setf (blt:background-color) (getf *color-map* :light-ground)))
            (setf (blt:cell-char x y) #\Space))

           (explored
            (if wall
             (setf (blt:background-color) (getf *color-map* :dark-wall))
             (setf (blt:background-color) (getf *color-map* :dark-ground)))
            (setf (blt:cell-char x y) #\Space))))))

  (mapc #'draw entities)

  (setf (blt:background-color) (blt:black))
  (blt:refresh))
```

Just a quick update to change what we draw based on what we can see and what we have seen.   And now, the map is much more mysterious!
![The Age Of Exploration](../screenshots/part-4-4-fog.gif?raw=true "Seek and ye shall fret")
