#include <stdio.h>

char cell1[] = "|         ", cell2[] = "|         ", cell3[] = "|         |", cell4[] = "|         ", cell5[] = "|         ", cell6[] = "|         |", cell7[] = "|         ", cell8[] = "|         ", cell9[] = "|         |";
char * cells[3][3] = {{cell1 + 5, cell2 + 5, cell3 + 5}, {cell4 + 5, cell5 + 5, cell6 + 5}, {cell7 + 5, cell8 + 5, cell9 + 5}};
//int *cells[9] = {&(cell1 + 4), &(cell2 + 4), &(cell3 + 4), &(cell4 + 4), &(cell5 + 4), &(cell6 + 4), &(cell7 + 4), &(cell8 + 4), &(cell9 + 4)};
const char spacer[] = "|         |         |         |";
const char line[] = "|---------|---------|---------|";
int x, y;

/*int EndGame(char player, char * coord) {
    int dxy[8][2] ={{1, 0}, {-1, 0}, {0, 1}, {0, -1}, {1, 1}, {-1, 1}, {1, -1}, {-1, -1}};
    int pair[8];
    int pairCount = 0;
    for(int x = 0; x < 8; x++) {
        //printf("debug\n");
        int checkLoc[2] = {*coord + dxy[x][0], *(coord + 1) + dxy[x][1]};
        printf("-----%i, %i\n", checkLoc[0], checkLoc[1]);
        if(checkLoc[0] >= 0 && checkLoc[0] <= 2 && checkLoc[1] >= 0 && checkLoc[1] <= 2) {
            if(*cells[checkLoc[0]][checkLoc[1]] == player) {
                pair[pairCount] = x;
                pairCount++;
            }
        }
    }
    for(int y = 0; y < 8; y++) {
        //printf("debug\n");
        int newCheckLoc[] = {*coord + 2 * dxy[pair[y]][0], *(coord + 1) + 2 * dxy[pair[y]][1]};
        printf("%i, %i\n", newCheckLoc[0], newCheckLoc[1]);
        if(newCheckLoc[0] >= 0 && newCheckLoc[0] <= 2 && newCheckLoc[1] >= 0 && newCheckLoc[1] <= 2) {
            printf("debug\n");
            if(*cells[newCheckLoc[0]][newCheckLoc[1]] == player) {
                return 1;
            }
        }
        //printf("debug\n");
    return 0;

    }
}*/

void input(char player) {
    int legal = 0;
    while(legal == 0) {
        int range = 0;
        while(range == 0) {
            printf("%c what is the x value of your token: ", player);
            scanf("%i", &x);
            printf("%c what is the y value of your token: ", player);
            scanf("%i", &y);
            if(x > 2 || x < 0 || y > 2 || y < 0) {
                printf("One of your values is out of range of the board, try again!!!\n");
            } else {
                range = 1;
            }
        }
        if(*cells[x][y] != ' '){
            printf("A token is already in that location, try again!!!\n");
        } else {
            *cells[x][y] = player;
            legal = 1;
        }

    }
}

void PrintBoard() {
    printf("%s\n%s\n%s%s%s\n%s\n%s\n%s\n%s%s%s\n%s\n%s\n%s\n%s%s%s\n%s\n%s\n", line, spacer, cell1, cell2, cell3, spacer, line, spacer, cell4, cell5, cell6, spacer, line, spacer, cell7, cell8, cell9, spacer, line);
}

int main() {
    char player;
    for(int rounds = 0; rounds < 9; rounds++) {
        PrintBoard();
        if(rounds % 2 == 0) {
            player = 'X';
        } else {
            player = 'O';
        }
        input(player);
        /*if(EndGame(player, cells[x][y]) == 1) {
            printf("Game Ended, player %c wins!!!\n", player);
            break;
        }*/

    }
    return 0;
}