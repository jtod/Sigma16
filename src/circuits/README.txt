Sigma16/src/circuits/README.txt -- how to run circuit simulations in a shell
This file is part of Sigma16.  See Sigma16/README and Sigma16/LICENSE

QUICK START
~~~~~~~~~~~

See Installation section in Sigma16 User Guide
$ cd Sigma16/src/circuits   (you need to be in this directory)
$ is the bash shell prompt, and : is the ghci prompt

The Circuits directory contains a collection of small to medium
examples.  To run the reg1 circuit, using driver Reg1Run:

   $ ghci                              start Haskell and load Hydra
   ghci> :load Circuits/Reg1Run        load the reg1 circuit and its driver
   ghci> :main                         run interactive simulation, entter h for help

The M1 circuit is a complete CPU circuit for Sigma16 Core.

  1. Translate program from assembly language to machine language:
        $ sigma16 assemble $COREPROGS/Arrays/ArrayMax

  2. Simulate the circuit as it executes the machine language program:
        $ ghci                       start Haskell and load Hydra
        ghci> :load M1.Run                 load M1 and its driver
        ghci> :main batch Arrays/ArrayMax  boot ArrayMax.obj.txt

Useful ghci commands (see ghc User Guide for full documentationO
   :r         reload after editing any of the code
   :q         exit ghci, go back to shell
   ^C         stop and return to ghci prompt
   uparrow    repeat previous command


OVERVIEW
~~~~~~~~

This directory contains a digital circuits which is a full processor
for the Core subset of the Sigma16 architecture.  It also contains a
circuit simulator that will run the circuit.  You can make the circuit
boot a machine language program; as the circuit runs it will execute
the program.

-- M1 is a circuit "Machine 1" that implements the Core subset of
   Sigma16

-- HDL/Hydra is a functional computer hardware description language
   (HDL).  This is a text language that allows you to decribe digital
   circuits very precisely, and it also has the ability to simulate
   the circuits.  All the circuits in M1 are specified using Hydra.  A
   copy of Hydra in included in Sigma16/src/circuits.  This means you
   don't need to download and install Hydra, and it ensures that the
   version of Hydra is compatible with the other software toolss.

-- Sigma16 is a folder containing sofware tools that can read and
   parse object files produced by the Sigma16 assembler.  This data is
   used to provide inputs to the circuits

The commands should be executed from within the circuits directory;
this enables ghc to find the source files.

The assembler reads in a source file (e.g. Add.asmltxt).  It writes
out several files: the machine language code (Add.obj.txt), the
assembly listing file (Add.lst.txt), and a metadata file (Add.md.txt).
You can ignore the metadata file, but it's worth looking at the obj
and lst files.  The object code file Add.obj.txt contains the actual
machine language program which the circuit can execute.

Naturally, if you modify the Sigma16 assembly language program, you
need to assemble it again before running the machine language on the
circuit.

When you run a program in ghci with :main Simple/Add, the program is
not being emulated.  The M1 circuit is a digital circuit consisting of
logic gates and flip flops, and nothing else.  This circuit is
relatively large and complex, and it is actually a full CPU for the
Sigma16 Core instruction set architecture.  The simulation tools read
in the machine language (from Add.obj.txt) and store it into the
memory, and the circuit then executes the program.

In the commands, you just specify the file basename "Add".  The
assembler will automatically append ".asm.txt", and the interface to
the circuit simulator will automatically append ".obj.txt".

FURTHER INFORMATION

  Sigma16 User Guide
     file: Sigma16/docs/UserGuide/Sigma16UserGuide.html

  Sigma16 Home Page
     Latest version
     https://jtod.github.io/home/Sigma16/
  
  Hydra Home Page
     https://github.com/jtod/Hydra

  GHC User Guide
     https://downloads.haskell.org/ghc/latest/docs/html/users_guide/index.html
