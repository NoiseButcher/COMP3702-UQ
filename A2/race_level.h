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
    if (thisNode->isDist() && me->reliability == 'N') {
        thisNode->reward -= 75*thisNode->distractor;
        thisNode->visits++;
    } else if (thisNode->isDist() && me->reliability == 'R') {
        thisNode->reward -= 10*thisNode->distractor;
        thisNode->visits++;

    //If there is an obstacle in the cell, cause some more damage.
    } else if (thisNode->isObs() && me->durability == 'W') {
        thisNode->reward -= 5;
        thisNode ->visits++;

    //Note that this cell will have 0 visits for  domestic cycles,
    //so will almost certainly be removed from the tree during pruning.
    }  else if (thisNode->isObs() && me->durability == 'D') {
        thisNode->reward -= 50;

    //Otherwise, assume it is a clearway.
    } else {
        thisNode->reward += 10*p*speed_multiplier;
        thisNode->visits ++;
    }
}

void execute_action(char action, TronCycle * me, Node *** allNodes) {

    int x = me->posx;
    int y = me->posy;

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

int move_to_edge (signed int a, int aMax) {
    signed int l = a;
    if ( l < 0) { return 0;}
    else {
        while (l > aMax) {
            l--;
            if (l == aMax) break;
        }
        return l;
    }
}

//Quick function to see if a node is already part of the game tree.
int node_in_Tree(const Node& targetNode, const vector<Node>& searchTree) {
    int i;
    for (i = 0; i < searchTree.size(); i++) {
        if ((searchTree[i].row == targetNode.row) && (searchTree[i].col == targetNode.col)) {
            return 1;
        }
    }
    return 0;
}

void expand_node(TronCycle * me, Policy * pi, Node *** allNodes) {
    char current_action;
    int action = (pi->actions_taken)-(me->MaxSpeed);
    current_action = pi->p_actions[action];
    while(action < 6) {
        execute_action(current_action, me, allNodes);
        action++;
    }
}

//Chooses a local node to add to the tree based purely on value.
void select_node(Node *** allNodes, TronCycle * me, GameTree * theRace) {
    int i, j, optx, opty, steps;
    steps = theRace->steps;
    optx = me->posx;
    opty = me->posy;
    for (i = me->posy-1; i < me->posy+1; i++) {
        for(j = me->posx; j < me->posx+3; j++) {
            //Check if the node has greater value, and if it does not already exist
            //in the game tree.
            if (allNodes[i][j]->value() > allNodes[optx][opty]->value() &&
                !node_in_Tree(*allNodes[i][j], theRace->playable)) {
                optx = j;
                opty = i;
            }
        }
    }
    me->posx = optx;
    me->posy = opty;

    //Store the address of the best child node into the next element of
    //the game tree.
    theRace->playable.push_back(*allNodes[opty][optx]);
    theRace->steps++;
}

//Does the same as the above funtion without adding the node to the game tree.
void select_node_tmp(Node *** allNodes, TronCycle * me) {
    int i, j, optx, opty;
    optx = me->posx;
    opty = me->posy;
    for (i = me->posy-1; i < me->posy+1; i++) {
        for(j = me->posx; j < me->posx+3; j++) {
            //Check if the node has greater value, and if it does not already exist
            //in the game tree.
            if (allNodes[i][j]->value() > allNodes[optx][opty]->value()) {
                optx = j;
                opty = i;
            }
        }
    }
    me->posx = optx;
    me->posy = opty;

}

//Runs the simulation for a game with a temporary TronCycle and Policy until
//a terminal state is reached, then deletes the temporary things.
void simulate_game(Policy * defaultPol, int length, TronCycle * me, Node *** allNodes) {

    int valX, valY;
    valX = me->posx;
    valY = me->posy;

    Policy *thisSim;
    TronCycle *autoPilot;

    autoPilot = new TronCycle;
    autoPilot->durability = me->durability;
    autoPilot->posx = me->posx;
    autoPilot->posy = me->posy;
    autoPilot->MaxSpeed = me->MaxSpeed;
    autoPilot->reliability = me->reliability;

    //Run solo simulations until a terminal state is reached.
    while (!is_terminal(allNodes[valY][valX]->row, length)) {

        thisSim = new Policy;
        thisSim->p_actions = defaultPol->p_actions;
        thisSim->actions_taken = defaultPol->actions_taken;
        expand_node(autoPilot, thisSim, allNodes);
        select_node_tmp(allNodes, autoPilot);
        valX = autoPilot->posx;
        valY = autoPilot->posy;

        delete thisSim;
    }
    delete autoPilot;
}

//Use a random number generator to choose the action of an adversary.
void move_adversary(Adversary * them, Node *** allNodes, int length, int depth) {
    int i, j, k;
    k = 0;
    i = 0;
    char default_actions[] = { '3', '2', '1', 'N', 'S', '0', 'T' };
    j = rand() % 100;

    //Determine which action to take based on the random number.
    while (true) {
      i += (them->adPol[them->posy][them->posx][k])*10;
      k++;
      if (i >= j) break;
      if ( k >= 6) k = 0;
    }

    //Take the action and update the position of the adversary's cycle.
    switch(default_actions[k]) {
        case '3':
            move_to_edge(them->posx += 3, length);
            break;
        case '2':
            move_to_edge(them->posx += 2, length);
            break;
        case '1':
            move_to_edge(them->posx += 1, length);
            break;
        case 'N':
            move_to_edge(them->posx += 1, length);
            move_to_edge(them->posy -= 1, 0);
            break;
        case 'S':
            move_to_edge(them->posx += 1, length);
            move_to_edge(them->posy += 1, depth);
            break;
        default:
            break;
    }
}

void update_map(char ** trackMap, Adversary ** them, TronCycle * me, Distractor ** evil,
                                                    int rows, int cols, int ops, int dist) {
    int i, j, k;

    // See if the distractors are active, otherwise turn them off.
    for (k = 0; k < dist; k++) {
        if(evil[k]->isActive()) {
            trackMap[evil[k]->posy][evil[k]->posx] = evil[k]->ID;
        } else {
            trackMap[evil[k]->posy][evil[k]->posx] = '0';
        }
    }

    for (i = 0; i < rows; i++) {
        for (j = 0; j < cols; j++) {
            //If the old node had a player on it, update poistion.
            if (trackMap[i][j] < 91 && trackMap[i][j] > 74) {
                //Check whether i should be there or not.
                if (me->posx != j || me->posy != i) {
                    trackMap[i][j] = '0';
                    trackMap[me->posy][me->posx] = me->ID;
                }

            //Now do Adversaries.
            } else if (trackMap[i][j] < 75 && trackMap[i][j] > 64) {
                for (k = 0; k < ops; k++) {
                    if (trackMap[i][j] == them[k]->ID) {
                        if (them[k]->posx != j || them[k]->posy != i) {
                            trackMap[i][j] = '0';
                            trackMap[them[k]->posy][them[k]->posx] = them[k]->ID;
                        }
                    }
                }
            }
        }
    }

}

void display_map (string **trackMap, int rows, int cols) {
    int i, j;

    for (i = 0; i < rows; i++) {
        for (j = 0; j < cols; j++) {
            cout << trackMap[i][j];
        }
        cout << endl;
    }
}

int single_race_solver(char *tFile, TronCycle **tronPut, int numReg) {

    GameTree *thisRace;
    Policy dPol;
    char buffer[1000];
    char default_actions[] = { '3', '2', '1', 'N', 'S', '0', 'T' };
    dPol.p_actions = default_actions;
    dPol.actions_taken = 3;
    int rows, cols, ops, prize, myTeam, dist, i, j, k, l;

    //This is the value that is returned at completion of
    //execution.
    int expected_reward = 0;

    //open the track file -- first argument.
    fstream infile;
    infile.open(tFile, ios::in);
    infile >> buffer;
    rows = atoi(buffer);
    infile >> buffer;
    cols = atoi(buffer);
    infile >> buffer;
    ops = atoi(buffer);
    infile >> buffer;
    infile >> buffer;
    prize = atoi(buffer);

    //Init the map as a matrix of node structs.
    Node node[rows][cols];
    Adversary adversary[ops];
    string trackMap[rows][cols];
    myTeam = numReg;

    l = 0;
    k = 0;
    dist = 0;
    //Populate the nodes.
    for (i = 0; i < rows; i++) {
        infile >> buffer;
        for (j = 0; j < cols; j++) {
            node[i][j].row = i;
            node[i][j].col = j;
            node[i][j].visits = 0;
            node[i][j].reward = 0;
            node[i][j].distractor = 0;
            //Draw a pretty fucking map.
            trackMap[i][j] = buffer[j];

            //Register a member of the cycle team at one of the available starting
            //positions if there is a spot available.
            if (buffer[j] < 91 && buffer[j] > 74 && l < myTeam) {
                tronPut[l]->posx = j;
                tronPut[l]->posy = i;
                tronPut[l]->ID = buffer[j];
                l++;
                node[i][j].tile = '0';

            //Register an opponent cycle if they exist, and assume they
            //have mint specs.
            } else if (buffer[j] < 75 && buffer[j] > 64 && k < ops && ops > 0) {
                adversary[k].ID = buffer[j];
                adversary[k].posx = j;
                adversary[k].posy = i;
                k++;
                node[i][j].tile = '0';

            } else if (buffer[j] > 96) {
                dist++;
                node[i][j].tile = buffer[j];

            } else {
                node[i][j].tile = buffer[j];

            }
        }
    }

    i = 0;
    //Get Adversary deets.
    if (ops > 0) {
        while (true) {
            infile >> buffer;
            while (i < ops) {
                if (adversary[i].ID == buffer[0]) {
                    adversary[i].adPol.reserve(rows);
                    for (j = 0; j < rows ; j++) {
                        adversary[i].adPol[j].reserve(cols);
                        for (l = 0; l < cols; l++) {
                            for (k = 0; k < 6; k++) {
                                infile >> buffer;
                                adversary[i].adPol[j][l].push_back(atof(buffer));

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
    }

    //Get distractor details.
    Distractor distractor[dist];
    k = 0;
    while (true) {
        infile >> buffer;
        distractor[k].ID = buffer[0];
        infile >> buffer;
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                if (node[i][j].tile == distractor[k].ID) {
                    node[i][j].distractor = atof(buffer);
                    distractor[k].p = atof(buffer);
                    distractor[k].posy = i;
                    distractor[k].posx = j;
                    k++;
                }
            }
        }
        if ( infile.eof() ) break;
    }

    //Store address of the starting node.
    thisRace = new GameTree;
    thisRace->steps = 0;
    thisRace->raceLen = cols;
    thisRace->playable.push_back(node[tronPut[0]->posy][tronPut[0]->posx]);

    //Print the map.
    for (i = 0; i < rows; i++) {
        for (j = 0; j < cols; j++) {
            cout << trackMap[i][j];
        }
        cout << endl;
    }

    return expected_reward;
}
