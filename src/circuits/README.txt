How to run a Sigma16 program on the M1 CPU circuit
This file is part of Sigma16.  See Sigma16/README and Sigma16/LICENSE

tl;dr Quick start
~~~~~~~~~~~~~~~~~
  $ cd Sigma16/src/circuits
  $ ghci
  ghci> :load M1/Run
  ghci> :main programs/Add
  Sigma16.M1> run
  Sigma16.M1> quit
  ghci> :quit

More detailed version
~~~~~~~~~~~~~~~~~~~~~

Install ghc and Hydra

Files

You may wish to keep the circuits directory and your Sigma16 programs
directory in different locations in your file system.  If you do that,
you can end up with very long file paths to your object code.  The M1
driver has a facility to make that easier.

If the file circuits/fileprefix.txt exists, then the text in its first
line is attached to the beginning of the :main argument.  For example,
if your object code file is
/some/very/long/path/to/your/code/prog.obj.txt you can put
/some/very/log/path/to/your/code/ into fileprefix.txt, and then just
run the program with :main myprog.

If you don't want anything prefixed to your file arguments, either set
fileprefix.txt to a blank line, or simply delete it.

$ cd Sigma16/src/circuits   (you need to be in this directory)

The M1 circuit is a complete CPU circuit for Sigma16 Core.

1. Translate program from assembly language to machine language:
   $ sigma16 assemble programs/Add
   ALternatively, use the Sigma16 app, copy the object code to a text
   editor, and save it in a file.  The object code file needs to end
   in .obj.txt (for example, Add.obj.txt)

2. Simulate the circuit as it executes the machine language program:
   $ ghci                      start Haskell
   ghci> :load M1.Run          load Hydra and the M1 system
   ghci> :main programs/Add    boot Add.obj.txt and run on the circuit

Some tips
~~~~~~~~~

When you start the system (e.g. :main Arrays/ArrayMax) it will take a
log of clock cycles to boot the machine language program.  If there
are n words of instructions and data, it will take n clock cycles.
It's useful to enter these commands:

  break reset
  run

This will run the simulation without stopping, until the reset signal
becomes 1, and then it will stop.  That way you can start single
stepping through the program, but don't have to single step through
the boot.

Reference: prompts and commands
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Prompts
   $            is the bash shell prompt
   ghci:        is the ghci prompt
   Sigma16.M1>  is the circuit prompt

Useful ghci commands (see ghc User Guide for full documentationO
   :r         reload after editing any of the code
   :q         exit ghci, go back to shell
   ^C         stop and return to ghci prompt
   uparrow    repeat previous command
