## Part 0
## Project Setup
### Install Common Lisp
* For this tutorial, I an using Steel Bank Common Lisp (SBCL)
  * http://sbcl.org/getting.html
  * Binaries for multiple architectures are available.  If you have a package manager, use the latest version of SBCL available from that source.
    * Ubuntu Linux: `apt install sbcl`
    * Windows 10: Download the 2.0.0 installer from the SBCL website
      * http://prdownloads.sourceforge.net/sbcl/sbcl-2.0.0-x86-64-windows-binary.msi

### Install the editor of your choice
  * In this instance I am using Atom
    * https://itsfoss.com/install-atom-ubuntu/
    * I personally prefer to add the PPA and install using apt
  * Atom needs a way to interface with Lisp.  For that I have chosen to use SLIMA
    * https://github.com/neil-lindquist/slima
    * In Atom's Packages Installer, locate and install the following:
      * SLIMA
      * language-lisp
      * lisp-paredit
      * parinfer
      * rainbow-delimiters
  * Atom + SLIMA requires a recent version of SLIME
    * If you have recently installed Emacs, then you probably already have SLIME
    * For this example I am using 2.26.1 from github
    * https://github.com/slime/slime/releases
  * Configure Atom's SLIMA settings with the path to SBCL and your newly downloaded SLIME instance
    * ![SLIMA Settings](./screenshots/part-0-slima-settings.png "SLIMA Settings")

### Quicklisp lets us manage libraries easily
  * Install Quicklisp (https://www.quicklisp.org/beta/)
  * `curl -O https://beta.quicklisp.org/quicklisp.lisp`
  * `sbcl --load quicklisp.lisp`
  * Register quicklisp in your init file
  * `(ql:add-to-init-file)`
  * exit SBCL
  * `(quit)`

### We will use QuickProject to create our project skeleton
  * Install QuickProject (In SBCL repl)
  * `(ql:quickload :quickproject)`
  * Create Lisp Project (In SBCL repl)
  * `(quickproject:make-project #p"~/roguelike-tutorial-cl" :depends-on '(cl-blt))`
  * Symlink your new project into quicklisp's local-projects
  * `ln -s ~/roguelike-tutorial-cl/ ~/quicklisp/local-projects/roguelike-tutorial-cl`

### Install BeatLibTerminal
  * https://github.com/tommyettinger/BearLibTerminal
  * Known working Windows and Ubuntu Linux builds available in this project's releases
    * https://github.com/Kehvarl/roguelike-tutorial-cl/releases/tag/BearLibTerminal
  * Download and unpack latest release
  * For Linux: Copy libBearLibTerminal.so to /usr/lib/libBearLibTerminal.so
  * For Windows: Copy BearLibTerminal.dll into the same folder as SBCL

### Download cl-blt and add to quicklisp local-projects
  * `git clone https://github.com/sjl/cl-blt.git ~/quicklisp/local-projects/cl-blt`

### Attempt to load your package
  * `sbcl` ;; I used `rlwrap sbcl` to give me history in the repl
  * `(ql:quickload :roguelike-tutorial-cl)`
  * `(in-package :roguelike-tutorial-cl)`
