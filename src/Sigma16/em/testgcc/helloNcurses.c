#include <stdio.h>
#include <ncurses.h>

define NLINES 10
define ncols  40
define x0     5
define y0     7


int main() {
  WINDOW * win = newwin (nlines, ncols, y0, x0);
  
   printf("Hello world, with ncurses!");
   initscr();
   cbreak();
   wmove (win, 10, 20); // move to y=10, x=20
   waddch (win, 'X');
   c = getchar();
   
   endwin();
   return 0;
}


// In git bash:
//     gcc -o helloNC helloNcurses.c
//    ./helloNC.exe
