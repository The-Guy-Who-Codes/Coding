#include <stdio.h>
#define bool int

char cell1[] = "|         ", cell2[] = "|         ", cell3[] = "|         |", cell4[] = "|         ", cell5[] = "|         ", cell6[] = "|         |", cell7[] = "|         ", cell8[] = "|         ", cell9[] = "|         |";
const char spacer[] = "|         |         |         |";
const char line[] = "|---------|---------|---------|";
char * cells[3][3] = {{cell1 + 5, cell2 + 5, cell3 + 5}, {cell4 + 5, cell5 + 5, cell6 + 5}, {cell7 + 5, cell8 + 5, cell9 + 5}};

void PrintBoard() {
    printf("%s\n%s\n%s%s%s\n%s\n%s\n%s\n%s%s%s\n%s\n%s\n%s\n%s%s%s\n%s\n%s\n", line, spacer, cell7, cell8, cell9, spacer, line, spacer, cell4, cell5, cell6, spacer, line, spacer, cell1, cell2, cell3, spacer, line);
}
void Input(char player) {
    int x, y;
    bool accept = 0;
    do {
        printf("Player %c, what is the x value of the board where you want to place your token: ", player);
        scanf("%i", &x);
        printf("Player %c, what is the y value of the board where you want to place your token: ", player);
        scanf("%i", &y);
        if (*cells[x][y] != ' ') {
            printf("A token is already in that position, try again!\n");
        } else {
            *cells[x][y] = player;
            accept = 1;
        }

    } while (accept == 0);


}


int main() {
    while (1) {
        PrintBoard();
        Input('Z');
    }
    return 0;
}