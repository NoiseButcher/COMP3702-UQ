#include "a2_classes.h"

int move_to_edge (signed int a, int aMax) {
    signed int l = a;
    if ( l < 0) { return 0;}
    else {
        while (l >= aMax) {
            l--;
            if (l < aMax) break;
        }
        return l;
    }
}

int is_terminal (int x, int x_max) {

    if (x < (x_max - 1)) {
        return 0;
    } else {
        return 1;
    }
}

void stack_players (string *node, char ID) {
    char tmp;
    tmp = (*node)[0];

    //Is there already a stack? Add to it.
    if ((*node)[0] == '[') {
        (*node)[(*node).size() - 1] = '-';
        (*node).push_back(ID);
        (*node).append("]");

    //Is the node a player/distractor? start a stack.
    } else if ((*node)[0] > 64) {
        (*node) = "[";
        (*node).push_back(tmp);
        (*node).append("-");
        (*node).push_back(ID);
        (*node).append("]");

    //Node is clear, place player on it.
    } else {
        (*node)[0] = ID;
    }
}

int is_Race(string name, string * listoNames, int listLen) {
    int i;
    int x = 0;
    for (i = 0; i < listLen; i++) {
        if (name == listoNames[i]) x++;
    }
    return x;
}

int in_Race(TronCycle * trackCycles, string * thisBike, int inRace, int xs) {
    int i;
    int x;
    int a = 0;
    for (x = 0; x < xs; x++) {
        for (i = 0; i < inRace; i++) {
            if (trackCycles[i].name == thisBike[x]) a++;
        }
    }
    if (a == inRace) {
        return a;
    } else {
        return 0;
    }
}

void sort_Cycles_X (TronCycle * tronIn, TronCycle ** tronOut, int totes, int slots) {
    signed int i, k;
    signed int j = 0;
    k = 0;
    int z = 1;
    while (true) {
        tronOut[k] = new TronCycle[totes];
        for (i = 0; i < totes; i++) {
            if (i + j >= totes) j = -i;
            tronOut[k][i] = tronIn[i+j];
        }
        k++;
        j = k;

        if (k >= totes) break;
    }
    j = -1;
    while (true) {
        if (k >= totes*slots) break;
        tronOut[k] = new TronCycle[totes];
        for (i = 0; i < totes; i++) {
            if (i + j > totes - 1) j = -i;
            if (i+j+1 < totes) {
                tronOut[k][i] = tronIn[i+j+1];
            } else {
                j = -i-1;
                tronOut[k][i] = tronIn[i+j+1];
            }
            j++;
        }
        k++;
        j = k - totes;
    }
}

void move_to_limit (Node *** nodes, int x, int y, int tX, int tY, Adversary * them) {
    if (((*nodes)[tY][tX].obs_true == 1 || (*nodes)[tY][tX].block_true == 1) && tY != y) {
        them->posx = x;
        them->posy = y;
    } else if (((*nodes)[tY][tX].obs_true == 1 || (*nodes)[tY][tX].block_true == 1) && tX != x) {
        while (true) {
            tX--;
            if ((*nodes)[tY][tX].obs_true == 0) break;
            if ((*nodes)[tY][tX].block_true == 0) break;
        }
        them->posx = tX;
        them->posy = tY;
    } else {
        them->posx = tX;
        them->posy = tY;
    }
}

//Use a random number generator to choose the action of an adversary.
void move_adversary(Adversary * them, Node *** allNodes, int length, int depth) {
    int i, j, k;
    k = 0;
    i = 0;
    signed int rowT = them->posy;
    int colT = them->posx;
    char default_actions[] = { '3', '2', '1', 'N', 'S', '0'};
    srand(time(NULL)*(colT + 1)*0.88288);
    j = rand() % 100;

    //Determine which action to take based on the random number.
    while (true) {
      i += (them->adPol[them->posy][them->posx][k])*100;
      if (i >= j) break;
      k++;
      if ( k > 5) k = 0;
    }
    //Take the action and update the position of the adversary's cycle. It gets blocked by
    //obstacles.
    switch(default_actions[k]) {
        case '3':
            move_to_limit(allNodes, them->posx, them->posy, move_to_edge(colT + 3, length), them->posy, them);
            break;
        case '2':
            move_to_limit(allNodes, them->posx, them->posy, move_to_edge(colT + 2, length), them->posy, them);
            break;
        case '1':
            move_to_limit(allNodes, them->posx, them->posy, move_to_edge(colT + 1, length), them->posy, them);
            break;
        case 'N':
            move_to_limit(allNodes, them->posx, them->posy,
                move_to_edge(colT + 1, length), move_to_edge(rowT - 1, depth), them);
            break;
        case 'S':
            move_to_limit(allNodes, them->posx, them->posy,
                move_to_edge(colT + 1, length), move_to_edge(rowT + 1, depth), them);
            break;
    }
}

void update_gametree (GameTree * master, GameTree * simulation) {
    int i;
    for (i = 0; i < 7; i++) {
        master->policy[master->steps][i] += simulation->policy[0][i];
    }
}

void update_gametree_sim (GameTree * master, float result) {
    int i;
    for (i = 0; i < 7; i++) {
        master->policy[0][i] += result;
    }
}



