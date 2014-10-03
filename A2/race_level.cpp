#include "a2_classes.h"

/*This file handles the race level, and should recieve arguments of
a file and a string. The file will contain the track and opponent data,
the string will be the cycle(s) used for this race. The return value on
this program is the expected reward [prizes - damages] from the completion
of the track using MCTS to find the most high yield path.
*/

void traverse_cell(TronCycle * me, Node * thisNode, float p) {

    int speed_multiplier = (thisNode->col) - (me->posx);
    //If there is a distractor in the cell, cause some damage
    //based on E[distractor].
    if (thisNode->tile > 96 && me->reliability == 'N') {
        thisNode->reward -= 75*thisNode->distractor;
        thisNode->visits++;
    } else if (thisNode->tile > 96 && me->reliability == 'R') {
        thisNode->reward -= 10*thisNode->distractor;
        thisNode->visits++;

    //If there is an obstacle in the cell, cause some more damage.
    } else if (thisNode->tile == '1' && me->durability == 'W') {
        thisNode->reward -= 5;
        thisNode ->visits++;

    //Note that this cell will have 0 visits for  domestic cycles,
    //so will almost certainly be removed from the tree during pruning.
    }  else if (thisNode->tile == '1' && me->durability == 'D') {
        thisNode->reward -= 50;

    //Otherwise, assume it is a clearway.
    } else {
        thisNode->reward += 10*p*speed_multiplier;
        thisNode->visits ++;
    }
}

void execute_action(char action, Node * thisNode, TronCycle * me, Node *** allNodes) {

    int x = thisNode->col;
    int y = thisNode->row;

    switch(action) {
        case '3':
            traverse_cell(me, allNodes[x][y+3], 1.0);
            traverse_cell(me, allNodes[x][y+2], 1.0);
            traverse_cell(me, allNodes[x][y+1], 1.0);
            break;
        case '2':
            traverse_cell(me, allNodes[x][y+2], 1.0);
            traverse_cell(me, allNodes[x][y+1], 1.0);
            break;
        case '1':
            traverse_cell(me, allNodes[x][y+1], 1.0);
            break;
        case 'N':
            traverse_cell(me, allNodes[x+1][y+1], 0.7);
            traverse_cell(me, allNodes[x+1][y], 0.1);
            traverse_cell(me, allNodes[x][y+1], 0.1);
            traverse_cell(me, allNodes[x][y], 0.1);
            break;
        case 'S':
            traverse_cell(me, allNodes[x-1][y+1], 0.7);
            traverse_cell(me, allNodes[x-1][y], 0.1);
            traverse_cell(me, allNodes[x][y+1], 0.1);
            traverse_cell(me, allNodes[x][y], 0.1);
            break;
        case '0':
            traverse_cell(me, allNodes[x][y], 1.0);
            break;
    }
}

int is_legal (int x, int y, int x_max, int y_max) {
    int xtmp = 0;
    int ytmp = 0;

    if (x < x_max && x > 0) {
        xtmp = 1;
    }

    if (y < x_max && y > 0) {
        ytmp = 1;
    }

    return (xtmp & ytmp);
}

int is_terminal (int x, int x_max) {
    if (x < x_max) {
        return 0;
    } else {
        return 1;
    }
}

void expand_node(TronCycle * me, Policy * pi, Node *** allNodes) {
    char current_action;
    int action = (pi->actions_taken)-(me->MaxSpeed);
    current_action = pi->p_actions[action];
    while(action < 6) {
        execute_action(current_action, allNodes[me->posy][me->posx], me, allNodes);
        action++;
    }
}

//Chooses a local node to add to the tree based purely on reward.
void select_node(Node * thisNode, Node *** allNodes, TronCycle * me, GameTree * theRace) {
    int i, j, optx, opty, steps;
    steps = theRace->steps;
    optx = me->posx;
    opty = me->posy;
    for (i = me->posy-1; i < me->posy+1; i++) {
        for(j = me->posx; j < me->posx+3; j++) {
            if (allNodes[i][j]->reward > allNodes[optx][opty]->reward) {
                optx = j;
                opty = i;
            }
        }
    }
    me->posx = optx;
    me->posy = opty;

    //Store the address of the best child node into the next element of
    //the game tree.
    theRace->playable[steps + 1] = new Node;
    theRace->playable[steps + 1] = &(*allNodes[opty][optx]);
    theRace->steps++;
}

void simulate_game(GameTree * theRace, Adversary ** them, Node *** allNodes) {
    int i = 0;

    while (i < theRace->steps) {
        //Make adversary move.

        //make my move;

        i++;
    }
}
int mcts(char *tFile, TronCycle **tronPut, int numReg) {

    GameTree *thisRace;
    Policy *policy;
    char buffer[1000];
    char default_actions[] = { '3', '2', '1', 'N', 'S', '0', 'T' };
    char tmp;
    int rows, cols, ops, prize, myTeam, i, j, k, l;

    //This is the value that is returned at completion of
    //execution.
    int expected_reward = 0;

    //open the track file -- first argument.
    fstream infile;
    infile.open(tFile, ios::in);
    infile >> buffer;
    cols = atoi(buffer);
    infile >> buffer;
    rows = atoi(buffer);
    infile >> buffer;
    ops = atoi(buffer);
    infile >> buffer;
    infile >> buffer;
    prize = atoi(buffer);

    //Init the map as a matrix of node structs.
    Node node[rows][cols];
    Adversary adversary[ops];
    myTeam = numReg;

    l = 0;
    k = 0;
    //Populate the nodes.
    for (i = 0; i < rows; i++) {
        infile >> buffer;
        for (j = 0; j < cols; j++) {
            node[i][j].tile = buffer[j];
            node[i][j].row = i;
            node[i][j].col = j;
            node[i][j].visits = 0;
            node[i][j].reward = 0;
            node[i][j].distractor = 0;

            //Register a member of the cycle team at one of the available starting
            //positions if there is a spot available.
            if (buffer[j] < 91 && buffer[j] > 74 && l < myTeam) {
                tronPut[l]->posx = j;
                tronPut[l]->posy = i;
                l++;
            } else if (buffer[j] < 75 && buffer[j] > 64 && k < ops) {
                adversary[k].name = buffer[j];
                adversary[k].posx = j;
                adversary[k].posy = i;
                k++;
            }
        }
    }

    i = 0;
    //Get Adversary deets.
    while (true) {
        infile >> buffer;
        while (i < ops) {
            if (adversary[i].name == buffer[0]) {
                for (j = 0; j < rows ; j++) {
                    for (l = 0; l < cols; l++) {
                        adversary[i].adPol[j][l]= new float[8];
                        for (k = 0; k < 8; k++) {
                            infile >> buffer;
                            adversary[i].adPol[j][l][k] = atof(buffer);
                        }
                    }
                }
                i = ops;
            } else {
                i++;
            }
       }
       i = 0;
       if (buffer[0] > 74) break;
    }

    //Get distractor details.
    while (true) {
        infile >> buffer;
        tmp = buffer[0];
        infile >> buffer;
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                if (node[i][j].tile == tmp) {
                    node[i][j].distractor = atof(buffer);
                }
            }
        }
        if ( infile.eof() ) break;
    }

    //Store address of the starting node.
    thisRace = new GameTree;
    thisRace->steps = 0;
    thisRace->playable[0] = new Node;
    thisRace->playable[0] = &node[tronPut[0]->posy][tronPut[0]->posx];

    while(true) {

        //Start a new policy for my location.
        policy = new Policy;
        policy->p_actions = default_actions;
        policy->actions_taken = 3;
        /*
        select_child();
        simulate();
        expand();
        backpropagate();
        */
        delete policy;
    }

    return expected_reward;
}
