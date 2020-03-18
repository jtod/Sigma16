<!--
Sigma16: index.md
Copyright (C) 2019, 2020 John T. O'Donnell
email: john.t.odonnell9@gmail.com
License: GNU GPL Version 3 or later.  Sigma16/LICENSE.txt, Sigma16/NOTICE.txt

This file is part of Sigma16.  Sigma16 is free software: you can
redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation, either
version 3 of the License, or (at your option) any later version.
Sigma16 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.  You should have received
a copy of the GNU General Public License along with Sigma16.  If
not, see <https://www.gnu.org/licenses/>.

------------------------------------------------------------------------------
index.md is the source for the index of this testing directory
------------------------------------------------------------------------------
-->

# Testing

These files are for testing the implementation of Sigma16.  They also
provide examples of all the assembly language statements and the
instruction set.

* [AsmCodeGen](AsmCodeGen.asm.txt) Examples of each assembly language
  statement, including instructions and directives.

* [AsmErrMessages](AsmErrMessages.asm.txt) Examples of errors that are
  detectable by the assembler.

* [StoreLoop](StoreLoop.asm.txt) While loop containing a store instruction

* [Arithmetic](Arithmetic.asm.txt) Test arithmetic operations.

* [Logic](Logic.asm.txt) Test the operations on bits, fields, and
  words.

* [Memory](Memory.asm.txt)  Loads, stores, stack operations, saving
  and restoring registers.

* [CmpJump](CmpJump.asm.txt) Test the comparisons, jumps, and
  conditional jumps.

* [Performance](Performance.asm.txt) A long nested loop that executes
  a large number of instructions, allowing measurements of the
  perfornace of an emulator or other implementation.

* Modules: [Mod1](Mod1.asm.txt) Mod1 imports Mod2 and Mod3,
  [Mod2](Mod2.asm.txt) Mod1 imports Mod2 and Mod3, and
  [Mod3](Mod3.asm.txt) Mod1 imports Mod2 and Mod3

* [PrimarySecondary](PrimarySecondary.asm.txt)

* [RFwrite](RFwrite.asm.txt)



