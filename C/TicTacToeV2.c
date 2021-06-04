#include <stdio.h>
#define bool int

// x and y coordinated have been flipped in their representation in arrays i.e [y, x] instead of [x, y], too lazy to fix (due to how cells 2D array is set up)

const int BoardX = 3;
const int BoardY = 3;
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
        scanf("%i", &y);
        printf("Player %c, what is the y value of the board where you want to place your token: ", player);
        scanf("%i", &x);
        if (x >= BoardX || y >= BoardY || x < 0 || y < 0) {
            printf("Coordinates out of range, try again!\n");
        } else {
            if (*cells[x][y] != ' ') {
                printf("A token is already in that position, try again!\n");
            } else {
                *cells[x][y] = player;
                accept = 1;
            }
        }

    } while (accept == 0);


}

int ThreeInARow(char player) {
    // gets all the coordinates of the player tokens
    int tokens[6][2] = {{-1, -1}, {-1, -1}, {-1, -1}, {-1, -1}, {-1, -1}, {-1, -1}};
    int tokenIter = 0;
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            if (*cells[i][j] == player) {
                tokens[tokenIter][0] = i;
                tokens[tokenIter][1] = j;
                tokenIter++;
            }
        }
    }
    for (int z = 0; z < 7; z++) {
        printf("%i, %i\n", tokens[z][0], tokens[z][1]);
    }
    // test for 3 in a row in a column
    int XGroup[3][3] = {{-1, -1, -1}, {-1, -1, -1}, {-1, -1, -1}};
    int X0, X1, X2;
    X0 = X1 = X2 = 0;
    for (int i = 0; i < 6; i++) {
        // groups player tokens based on similar x coordinates
        switch (tokens[i][1]) {
            case 0:
                XGroup[0][X0] = tokens[i][1];
                X0++;
                break;
            case 1:
                XGroup[1][X1] = tokens[i][1];
                X1++;
                break;
            case 2:
                XGroup[2][X2] = tokens[i][1];
                X2++;
                break;
            default:
                continue;
        }
    }
    if (XGroup[0][2] != -1 || XGroup[1][2] != -1 || XGroup[2][2] != -1) {
        printf(" PLayer %c Wins!!! Column!!!\n", player);
        return 1;
    }

    // test for 3 in a row in a row
    int YGroup[3][3] = {{-1, -1, -1}, {-1, -1, -1}, {-1, -1, -1}};
    int Y0, Y1, Y2;
    Y0 = Y1 = Y2 = 0;
    for (int i = 0; i < 6; i++) {
        // groups all player tokens based on their y coordinates
        switch (tokens[i][0]) {
            case 0:
                YGroup[0][Y0] = tokens[i][0];
                Y0++;
                break;
            case 1:
                YGroup[1][Y1] = tokens[i][0];
                Y1++;
                break;
            case 2:
                YGroup[2][Y2] = tokens[i][0];
                Y2++;
                break;
            default:
                continue;
        }
    }
    if (YGroup[0][2] != -1 || YGroup[1][2] != -1 || YGroup[2][2] != -1) {
        printf("Player %c Wins!!! Row!!!\n", player);
        return 1;
    }

    // test for 3 in a row diagonally in the direction going to the top right
    int InDiagonal[3][2] = {{-1, -1}, {-1, -1}, {-1, -1}};
    int DiagIter = 0;
    for (int i = 0; i < 6; i++) {
        if (tokens[i][1] == tokens[i][0] && tokens[i][1] != -1 && tokens[i][0] != -1) {
            InDiagonal[DiagIter][0] = tokens[i][0];
            InDiagonal[DiagIter][1] = tokens[i][1];
            DiagIter++;
        }
    }
    for (int z = 0; z < 4; z++) {
        printf("\n%i, %i", InDiagonal[z][0], InDiagonal[z][1]);
    }
    if (InDiagonal[2][2] != -1) {
        printf("Player %c Wins!!! Forward Diagonal!!!\n", player);
        return 1;        
    }

    // other diagonal test (hard coded but okay) will be changed

    if (*cells[2][0] == player && *cells[1][1] == player && *cells[0][2] == player) {
        printf("Player %c Wins!!! Backwards Diagonal!!!\n", player);
        return 1;        
    }
    return 0;
}


int main() {
    bool endgame = 0;
    int round = 0;
    char player;
    PrintBoard();
    while (endgame ==  0) {
        if (round % 2 == 0) {
            player = 'X';
        } else {
            player = 'O';
        }
        Input(player);
        PrintBoard();
        endgame = ThreeInARow(player);
        round++;
    }
    return 0;
}