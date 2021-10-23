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
