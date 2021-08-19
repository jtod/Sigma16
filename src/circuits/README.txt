The Sigma16 circuits
~~~~~~~~~~~~~~~~~~~~

Quick start
~~~~~~~~~~~

See the Installation section in the User Guide.  Check that the
software is installed and working:

$ node --version
v16.5.0
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.0.1
$ alias sigma16
alias sigma16='node PATH/TO/YOUR/INSTALLATION/Sigma16/src/cli/sigma16.mjs'
$

It's convenient to define an alias to the directory where you have the
example programs.  For example, in .bashrc:

   export COREPROGS="../../examples/Core"

Open a shell and cd to directory Sigma16/src/circuits:

   $ sigma16 assemble $COREPROGS/Simple/Add
   $ runghc M1/Tools/Run $COREPROGS/Simple/Add.obj.txt

Alternative:
   runghc M1/Tools/Run M1/Programs/Add.obj.txt

Introduction
~~~~~~~~~~~~

This directory contains some machines defined as digital circuits
(M1), and software tools that run machine language programs directly
on the circuits (Sigma16, HDL).  The software tools use a command line
interface in a shell.  You need to have node.js and ghc installed, and
the sigma16 alias defined.  See the Installation section of the User
Guide.

-- M1 is a circuit "Machine 1" that implements the Core subset of
   Sigma16

-- HDL/Hydra is a functional computer hardware description language
   (HDL).  This is a text language that allows you to decribe digital
   circuits very precisely, and it also has the ability to simulate
   the circuits.  All the circuits in M1 are specified using Hydra.

-- Sigma16 is a folder containing sofware tools that can read and
   parse object files produced by the Sigma16 assembler.  This data is
   used to provide inputs to the machines.

The commands should be executed from within the circuits directory;
this enables ghc to find the source files.  You can run the examples
directly in Sigma16/src/circuits, but if you're experimenting with
modifications to the circuits, it's a good idea to copy the entire
"circuits" folder to somewhere in your workspace.


How to assemble a program
~~~~~~~~~~~~~~~~~~~~~~~~~

The following command will read Add.asm.txt and write the object file
Add.obj.txt.  You just specify the file basename "Add", and the
assembler will automatically append ".asm.txt".  The assembler will
write out the object file Add.obj.txt, a listing file Add.lst.txt, and
a metadata file Add.md.txt.  You can ignore the metadata file, but
it's worth looking at the obj and lst files.  The object code file
Add.obj.txt contains the actual machine language program which the
circuit can execute.

   sigma16 assemble M1/Programs/Add


How to run an object program on the circuit
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This command will run the Add machine language program on the M1
circuit:

   runghc M1/Tools/Run M1/Programs/Add.obj.txt

-- M1/Tools/Run is a program that reads in an object code file and
   uses it to prepare input to the simulation driver
-- M1/Programs/Add.obj.txt is a text file giving the object code for
   the Add.asm.txt program.
