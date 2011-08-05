#include <ncurses.h>

char hline_string[] = 
	"+-------------------"
	"--------------------"
	"--------------------"
	"-------------------+";

int
main(int argc, char** argv)
{	
	initscr();			/* Start curses mode 		  */

	int x = 0, y = 0;
	for (y=1; y<24; y++) {
		x = 0;
		mvprintw(y, x, "|");
		x = 79;
		mvprintw(y, x, "|");
	}


	mvprintw(0, 0, hline_string);
	mvprintw(1, 2,"Mark6 Net2Disk v0.1");
	mvprintw(2, 0, hline_string);

	mvprintw(22, 0, hline_string);
	mvprintw(23, 2,"Copyright 2011 MIT Haystack Observatory");
	mvprintw(24, 0, hline_string);

	refresh();			/* Print it on to the real screen */
	getch();			/* Wait for user input */
	endwin();			/* End curses mode		  */

	return 0;
}
