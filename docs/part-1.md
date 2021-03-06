# Part 1 - Drawing the @ symbol and moving it around

## Loading our project
We will start with getting the project connected to SLIMA so we have some assistnace from our IDE.
* Open your project folder in Atom.
  * If SLIMA doesn't automatically start, select "Packages" -> "SLIMA" -> "Lisp Process" -> "Start"
  * Once an Atom-Friendly lisp instance is running, it should automatically connect.  If not select "Packages" -> "SLIMA" -> "Lisp Process" -> "Connect".
* At the bottom of your Atom screen you should either have an open REPL, or a pull-out holding that Repl.
* Place your cursor in the REPL after the `CL-USER>` prompt and type the following commands to load your project.
  * `(ql:quickload :rogulike-tutorial-cl)`
  * `(in-package :roguelike-tutorial-cl)`
* If there were any errors, please make sure that you've followed all the steps in part-0, including the symlink into your quicklisp directory.  Also double-check that you've spelled the name of your project correctly.
* ![Loading your project](../screenshots/part-1-0-load-package.png?raw=true "Loading Package in REPL")

## Opening a Window
QuickProject created a main file for us.  In this tutorial that file is named `roguelike-tutorial-cl.lisp`.  If you have a different name for your main file, make note of it and use that anywhere we reference the above filename.

Open your `roguelike-tutorial-cl.lisp` file and ensure it has the following contents:
```lisp
(in-package #:roguelike-tutorial-cl)

(defparameter *game-name* "Common Lisp Roguelike Tutorial")
(defparameter *screen-width* 80)
(defparameter *screen-height* 50)

(defun draw()
  (blt:clear)
  (blt:refresh))

(defun config ()
  (blt:set "window.resizeable = true")
  (blt:set "window.size = ~Ax~A" *screen-width* *screen-height*)
  (blt:set "window.title = ~A" *game-name*))

(defun main()
  (blt:with-terminal
    (config)
    (loop :do
      (draw)
      (blt:key-case (blt:read)
                    (:escape (return))
                    (:close (return))))))
```

Before we test our code, we need to make our lisp instance aware of it.  Common Lisp doesn't reload the file every time we try to run it, and instead expects we've already applied all our changes to the running system represented by our REPL.

In Atom, we can use the keyboard shortcut `ALT+k` (Sometimes written `M-k`) to "Compile and Load File".  Alternatively, if we've made a change to a single function the command `ALT+c` to "Compile Function".

* Press `ALT+K`
* In your REPL, type `(main)` and press RETURN

![The basic window](../screenshots/part-1-2-blank-window.png?raw=true "Running our project for the first time.")

## Drawing the @ symbol
A blank screen is a boring screen, so now we will place something on it to represent our player.

Edit the `draw` function to match the following
```lisp
(defun draw()
  (blt:clear)
  (setf (blt:color) (blt:white)
        (blt:cell-char 10 10) #\@)
  (blt:refresh))
```

As you can see we've added a couple of new lines. Specifically, we are using `setf` to  set the value of two things.  In fact this could be done as two separate setf calls like so:
```lisp
(defun draw()
  (blt:clear)
  (setf (blt:color) (blt:white))
  (setf (blt:cell-char 10 10) #\@)
  (blt:refresh))
```

This may make it a little more clear what we're setting.
* `(setf (blt:color) (blt:white))` Informs BLT that until otherwise noted, we will draw all characters and symbols in white.
* `(setf (blt:cell-char 10 10) #\@)` Tells BLT to update the character at cell 10,10 to our new symbol "@".   In Common Lisp, #\@ refers to the @ character itself instead of to a string containing that character.

* Press `ALT+c` (Compile function)
* In your REPL, type `(main)` and press RETURN

![The first @](../screenshots/part-1-3-first-@.png?raw=true "Drawing a symbol on our screen.")

## Keyboard Input

In addition to providing the terminal, BLT can monitor the keyboard for user input for us.  It's actually already doing that in our existing demo:
```lisp
(blt:key-case (blt:read)
  (:escape (return))
  (:close (return)))
```
This fragment does a few things:
* First, we make a call to `(blt:read)` to get any currently pressed keys
* Next we pass that result to `(blt:key-case` which is a `case` statement specifically for handling BLT's key events.
* `blt:key-case` then compared the currently pressed key to our various tests until either one passes or all fail. (eg: `:escape` which represents the ESC key)
* If one test case passes, the expression after the test is evaluated. (eg: `(return)` to exit our current function).

While it was convenient enough to check for the exit key in our main function, let's move that our to its own `handle-keys` function so we can modify it later.

```Lisp
(defun handle-keys ()
  (let ((action nil))
    (blt:key-case (blt:read)
                  (:escape (setf action (list :quit t)))
                  (:close (setf action (list :quit t))))
    action))
```

Remember to press `ALT+c` to compile our new function

You will have noticed that this function doesn't just call `(return)` when we hit ESC, instead we return a list containing 2 values:  The keyword `:quit` and the value `t`.  We will modify our `main` function to work with this returned message.

```lisp
(defun main()
  (blt:with-terminal
    (config)
    (loop :do
         (draw)
         (let* ((action (handle-keys))
                (exit (getf action :quit)))
           (if exit
             (return))))))
```

Be sure you press `ALT+c` to compile our changed function, or `ALT+k` to compile and load the entire file.

Here we have expanded our main loop.  We draw our terminal, then check for a keypress.  If `handle-keys` returns a message of `(:quit t)` then  we exit the loop which nicely terminates the program

Specifically what we are doing is:
* `(let*`  Bind some variables.  the * version of `let` allows the variables to reference each other.
*  `(action (handle-keys))`  Assign the output of `handle-keys` to the variable `action`
* `(exit (getf action :quit))` If our `action` variable contains the `:quit` keyword, get it's value and assign it to our new variable `exit`.  If not then assign `nil` to `exit`.
* `(if exit (return))` If our `exit` variable is set to `t`, then exit our loop which will end the program.

## Moving Around

### Making Draw position-aware
Our project can open a terminal, draw a character on the screen, and receive input from our player.   Now it's time to make something happen so our player doesn't get too bored.

To start with, we'll modify our `draw` function to accept the position at which to place the player character.

```lisp
(defun draw (player-x player-y)
  (blt:clear)
  (setf (blt:color) (blt:white)
        (blt:cell-char player-x player-y) #\@)
  (blt:refresh))
```
Don't forget to use `ALT+c` to compile our updated function

We've made 2 changes to the draw function:
* `(defun draw (player-x player-y)`  instead of receiving no parameters, we now expect both the player-x and player-y parameters
* `(blt:cell-char player-x player-y) #\@)` we have also replaced our hard-coded position with the passed-in parameters.  The draw function will now put the player wherever we want them each frame.

### Tracking position and giving Draw what it needs
Next up we must modify our `main` function to use that new `draw` function.

```lisp
(defun main()
  (blt:with-terminal
    (config)
    (loop :with player-x = (/ *screen-width* 2)
          :and  player-y = (/ *screen-height* 2)
          :do
         (draw player-x player-y)
         (let* ((action (handle-keys))
                (exit (getf action :quit)))
           (if exit
             (return))))))
```
As ever, `ALT+c` is your friend when you make a change.

In Common Lisp, `loop` is extremely powerful, it's practically a miniature programming language all its own.  The important part for what we're doing is that we can have it initialize and keep track of variables for us: `:with player-x = (/ *screen-width* 2)` creates a variable named `player-x` and starts it out at half our screen width.  `:and  player-y = (/ *screen-height* 2)`  creates another variable, this one named `player-y` with a value of half our screen height.

In the loop's body we have updated our `draw` function to reference the two variables our loop macro is creating for us: `(draw player-x player-y)`.  Now every pass through the loop we'll call draw with whatever the current values of both these variables are.

![The centered @](../screenshots/part-1-5-centered-@.png?raw=true "Using our enhanced draw function")

### Finally, Movement!
Now we can get keyboard input, we can draw a character wherever we want, and we can track their position on the screen.   The last part is to bring these together.

First, we change the `handle-keys` function to watch the arrows.
```lisp
(defun handle-keys ()
  (let ((action nil))
    (blt:key-case (blt:read)
                  (:up (setf action (list :move (cons 0 -1))))
                  (:down (setf action (list :move (cons 0 1))))
                  (:left (setf action (list :move (cons -1 0))))
                  (:right (setf action (list :move (cons 1 0))))
                  (:escape (setf action (list :quit t)))
                  (:close (setf action (list :quit t))))
    action))
```
`ALT+c` will make this modification available to your program.

The only real change here is to add 4 new key checks.  For example: `(:up (setf action (list :move (cons 0 -1))))` watches for the up-arrow key on the keyboard.  If that's pressed we set our action to a list containing the keyword `:move` and a cons-cell holding the desired change in X and Y position.

Now our `main` function needs to do something with this new input possibility
```lisp
(defun main()
  (blt:with-terminal
    (config)
    (loop :with player-x = (/ *screen-width* 2)
          :and  player-y = (/ *screen-height* 2)
          :do
         (draw player-x player-y)
         (let* ((action (handle-keys))
                (move (getf action :move))
                (exit (getf action :quit)))

           (if exit
             (return))

           (when move
             (incf player-x (car move))
             (incf player-y (cdr move)))))))
```
Once more with feeling: `ALT+c`

If you're keeping track, you'll have noticed that this is a simple change.  We added another variable in our `let*` and we created a block to deal with that variable.
* `(move (getf action :move))` works exactly like the `:exit` one does.  If there's a list in the message that starts with the given keyword, it returns the rest of the message, otherwise it gives us `nil`.
* `(when move` - If Move is set to something, do the rest:
* `(incf player-x (car move))` Add the first part of our Move cons to the player's X coordinate.
* `(incf player-y (cdr move))` Add the second part of our Move cons to the player's Y coordinate.

And that's it!  Running the game now gives us the exciting opportunity to wander around in the dark.

![Action!](../screenshots/part-1-6-moving-around.gif?raw=true "Interactive at last")

![Complete Part-1 Code listing](https://github.com/Kehvarl/roguelike-tutorial-cl/blob/part-1/roguelike-tutorial-cl.lisp)
