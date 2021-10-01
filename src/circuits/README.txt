Sigma16/src/circuits/README.txt -- how to run circuit simulations in a shell
This file is part of Sigma16.  See Sigma16/README and Sigma16/LICENSE

Quick start
~~~~~~~~~~~
  $ cd src/circuits
  $ ghci
  ghci> :load M1/Run
  ghci> :main programs/Add
  Sigma16.M1> run
  Sigma16.M1> quit
  ghci> :q

Running the M1 CPU circuit
~~~~~~~~~~~~~~~~~~~~~~~~~~

Install ghc and Hydra

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

Prompts
   $            is the bash shell prompt
   ghci:        is the ghci prompt
   Sigma16.M1>  is the circuit prompt

Useful ghci commands (see ghc User Guide for full documentationO
   :r         reload after editing any of the code
   :q         exit ghci, go back to shell
   ^C         stop and return to ghci prompt
   uparrow    repeat previous command
