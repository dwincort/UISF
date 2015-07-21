  _    _ _____  _____ ______ 
 | |  | |_   _|/ ____|  ____|
 | |  | | | | | (___ | |__   
 | |  | | | |  \___ \|  __|  
 | |__| |_| |_ ____) | |     
  \____/|_____|_____/|_|     
-----------------------------

The UISF package provides an arrowized FRP library for graphical user 
interfaces.  UISF stems from work done on Euterpea (http://haskell.cs.yale.edu/).

See License for licensing information.


============================
==== Getting the Source ====
============================

Currently (7/20/2015), the most up-to-date version of UISF is 
available through GitHub at:

    https://github.com/dwincort/UISF

When we reach milestones, we will release stable versions to Hackage.


============================
======= Installation =======
============================

Installing From Hackage RECOMMENDED
    cabal install UISF

Installing from source

  1) Clone the source from github
     git clone https://github.com/dwincort/UISF

  2) cd into the UISF directory
     cd UISF

  3) install UISF with cabal
     cabal install

Note: If you get errors about pacakges not being installed make sure that cabal binaries are in your `$PATH`.
To add cabal binaries to your path first add 
export PATH=$HOME/.cabal/bin:$PATH to your .bashrc
then run 
source ~/.bashrc.
Now you should be able to successfully cabal install

This will install UISF locally for GHC.  As noted on the Haskell wiki:

    One thing to be especially aware of, is that the packages are installed 
    locally by default, whereas the commands

        runhaskell Setup configure
        runhaskell Setup build
        runhaskell Setup install

    install globally by default. If you install a package globally, the 
    local packages are ignored. The default for cabal-install can be 
    modified by editing the configuration file.

    Help about cabal-install can be obtained by giving commands like:

        cabal --help
        cabal install --help

(http://www.haskell.org/haskellwiki/Cabal-Install - Accessed on 12/15/2012)


============================
======== Information =======
============================

UISF was created by:
    Dan Winograd-Cort <dwc@cs.yale.edu>
as a branch of work on Euterpea (http://haskell.cs.yale.edu/), created by:
    Paul Hudak <paul.hudak@cs.yale.edu>, 
    Eric Cheng <eric.cheng@aya.yale.edu>,
    Hai (Paul) Liu <hai.liu@aya.yale.edu>
