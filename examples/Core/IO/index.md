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
index.md is the source for the index of this examples directory
------------------------------------------------------------------------------
-->



# Input/Output

These programs illustrate how to do Input/Output.  This is done by
using a trap instruction to request the I/O.  The trap instruction is
RRR, and it takes three register operands.  The first register
(specified by the d field) contains a code indicating what operation
is being requested (0=halt, 1=read, 2=write, and so on).  The second
register (specified by the a field) gives the address of a buffer (an
array of characters to be read or written, and the third register
(specified by the b field) gives the number of characters to read or
write.

* [Write](Write.asm.txt) Demonstrate how to output a character string.

* [Read](Read.asm.txt) Demonstrate how to read some input from the user.

* [ReadWrite](ReadWrite.asm.txt) Read some input from the user and
  then write it back out.

* [WriteChunks](WriteChunks.asm.txt) Write several short strings on
  the same line, and write several lines.
