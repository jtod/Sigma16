/* Sigma16: Sigma16em.c
Copyright (C) 2020 John T. O'Donnell
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

-------------------------------------------------------------------------------
Sigma16em.c is the main program for the emulator
-------------------------------------------------------------------------------
*/

#include <stdio.h>

void hi () {
  printf ("hi there\n");
}

int main() {
  printf ("Sigma16 emulator\n");
  hi ();
  printf ("Sigma16 emulator terminating");
  return 0;
}

