#include <stdio.h>

char cell1[] = "|         ", cell2[] = "|         ", cell3[] = "|         |", cell4[] = "|         ", cell5[] = "|         ", cell6[] = "|         |", cell7[] = "|         ", cell8[] = "|         ", cell9[] = "|         |";
const char spacer[] = "|         |         |         |";
const char line[] = "|---------|---------|---------|";
char * cells[3][3] = {{cell1 + 5, cell2 + 5, cell3 + 5}, {cell4 + 5, cell5 + 5, cell6 + 5}, {cell7 + 5, cell8 + 5, cell9 + 5}};

void PrintBoard() {
    printf("%s\n%s\n%s%s%s\n%s\n%s\n%s\n%s%s%s\n%s\n%s\n%s\n%s%s%s\n%s\n%s\n", line, spacer, cell7, cell8, cell9, spacer, line, spacer, cell4, cell5, cell6, spacer, line, spacer, cell1, cell2, cell3, spacer, line);
}
void Input(char player) {
    int x, y;
    printf("Player %c, what is the x value of the board where you want to place your token: ", player);
    scanf("%i", &x);
    printf("Player %c, what is the y value of the board where you want to place your token: ", player);
    scanf("%i", &y);
    *cells[x][y] = player;

}


int main() {
    PrintBoard();
    Input('O');
    PrintBoard();
    return 0;
}