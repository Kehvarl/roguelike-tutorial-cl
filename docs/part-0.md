# Part 0 - Setting Up

## Prior Knowledge
This tutorial assumes some basic familiarity with programming in general, and with Common Lisp in particular.  There are numerous resources online for getting started with Common Lisp, and Common Lisp debugging that are definitely recommended before diving into this.

## Project Setup
### Install Common Lisp
This tutorial uses Steel Bank Common Lisp (SBCL) which currently is one of the more popular Lisp environments

  * SBCL Installation Guide: http://sbcl.org/getting.html
  * Binaries for multiple architectures are available.  If you have a package manager, use the latest version of SBCL available from that source.
    * Ubuntu Linux: `apt install sbcl`
    * Windows 10: Download the 2.0.0 installer from the SBCL website
      * http://prdownloads.sourceforge.net/sbcl/sbcl-2.0.0-x86-64-windows-binary.msi

### Install the editor of your choice
Any screenshots or keyboard shorcuts referenced in this tutorial will be drawn from Atom with the SLIMA "Superior Lisp Interaction Mode for Atom" plugin.  If you have a preferred Common Lisp editing environment, feel free to use that.  If not, then this may be a good starting point.

  * Install Atom using your preferred method.
    * Some Installation Options: https://itsfoss.com/install-atom-ubuntu/
    * This tutorial uses Atom installed on Ubuntu Linux using a PPA and `apt install atom`
  * As mentioned, Atom will interface with our Common Lisp environment using SLIMA
    * Details installation instructions available at: https://github.com/neil-lindquist/slima
    * In Atom's Packages Installer, locate and install the following:
      * SLIMA
      * language-lisp
      * lisp-paredit
      * parinfer
      * rainbow-delimiters
  * Atom + SLIMA requires a recent version of SLIME
    * If you have recently installed Emacs, then you probably already have SLIME
    * All the examples herein were done using  SLIME 2.26.1 from github
    * https://github.com/slime/slime/releases
  * Configure Atom's SLIMA settings with the path to SBCL and your newly downloaded SLIME instance
    * ![SLIMA Settings](../screenshots/part-0-slima-settings.png?raw=true "SLIMA Settings")

### Quicklisp lets us manage libraries easily
Quicklisp is a library manager for Common Lisp.  It will simplify installation of all dependencies needed for the tutorial.

  * Install Quicklisp (https://www.quicklisp.org/beta/)
  * `curl -O https://beta.quicklisp.org/quicklisp.lisp`
  * `sbcl --load quicklisp.lisp`
  * `(quicklisp-quickstart:install)` ;; Install Quicklisp system
  * `(ql:add-to-init-file)`          ;; Register Quicklisp in your SBCL init file
  * `(quit)`                         ;; Exit SBCL

### Create a project skeleton using QuickProject
Quickproject is a useful tool for starting a Common Lisp project.  It will create the necessary `package.lisp` file as well as an appropriate `<project-name>.lisp` file and `<project-name>.asd` file.  The `*.asd` file is critical for describing dependencies and laying out the order in which project files will be loaded.

  * `sbcl` or `rlwrap sbcl` Start a lisp environment. The `rlwrap` is useful for adding history and command recall to the REPL
  * Install quickproject, and use it to create the tutorial project
    * `(ql:quickload :quickproject)`
    * `(quickproject:make-project #p"~/roguelike-tutorial-cl" :depends-on '(cl-blt))`
    * `(exit)`

  * Open the newly created `<project-name>.asd` file and update the Description, Author, and License fields to your liking.

  * Quicklisp won't search arbitrary directories for projects, so in this instance we will add a link to the project in the quicklisp local-projects directory.  
    * `ln -s ~/roguelike-tutorial-cl/ ~/quicklisp/local-projects/roguelike-tutorial-cl`

### Install BeatLibTerminal
In order to create a terminal-like interface, we need the appropriate library.  The reference tutorial uses libtcod.  However, at the time of creation of the tutorial, there do not appear to be complete bindings for the tcod library in Common Lisp.   Instead we will utilize Bear Lib Terminal, which will require us to implement some features that would be built into libtcod.

  * Bear Lib Terminal github repo: https://github.com/tommyettinger/BearLibTerminal
  * If you don't want to build from source, or have issues downloading the precompiled version of BLT linked to in the repository, there is a known-working set of binaries as part of this repository.
    * https://github.com/Kehvarl/roguelike-tutorial-cl/releases/tag/BearLibTerminal
  * For Linux: Copy libBearLibTerminal.so to /usr/lib/libBearLibTerminal.so
  * For Windows: Copy BearLibTerminal.dll into the same folder as SBCL

### Download cl-blt and add to quicklisp local-projects
To interface with Bear Lib Terminal, thus tutorial uses the cl-blt project.  This project is not considered complete, but does contain the components that are needed to implement a functional Roguelike in line with the original tutorial.

  * `git clone https://github.com/sjl/cl-blt.git ~/quicklisp/local-projects/cl-blt`

### Attempt to load your package
  * `sbcl` or `rlwrap sbcl`
  * `(ql:quickload :roguelike-tutorial-cl)`
  * `(in-package :roguelike-tutorial-cl)`
