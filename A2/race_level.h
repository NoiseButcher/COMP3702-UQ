#include "utility_things.h"

/*This file handles the race level, and should recieve arguments of
a file and a string. The file will contain the track and opponent data,
the string will be the cycle(s) used for this race. The return value on
this program is the expected reward [prizes - damages] from the completion
of the track using MCTS to find the most high yield path.
*/

void traverse_cell(TronCycle * me, Node * thisNode, float p) {

    float speed_multiplier = 1 + (thisNode->col) - (me->posx);
    //If there is a distractor in the cell, cause some damage
    //based on E[distractor].
    if (thisNode->isDist() && me->reliability == 'N') {
        thisNode->reward -= 75;
        thisNode->visits++;

    } else if (thisNode->isDist() && me->reliability == 'R') {
        thisNode->reward -= 10;
        thisNode->visits++;

    //If there is an obstacle in the cell, cause some more damage.
    } else if (thisNode->obs_true && me->durability == 'W') {
        thisNode->reward -= 5;
        thisNode ->visits++;

    //Note that this cell will have 0 visits for  domestic cycles,
    //so will almost certainly be removed from the tree during pruning.
    }  else if (thisNode->obs_true && me->durability == 'D') {
        thisNode->reward -= 50;

    //Otherwise, assume it is a clearway.
    } else {
        thisNode->reward += 10*p*speed_multiplier;
        thisNode->visits ++;
    }
}

void execute_action(char action, TronCycle * me, Node *** allNodes, int depth, int length) {

    int impossibleX = 0;
    signed int impossibleY = 0;
    int x = me->posx;
    int y = me->posy;
    int i;

    //Use this to make sure we don't travel through nodes that we cannot.
    if (me->durability == 'D') {
        if ((*allNodes)[y][move_to_edge(x+1, length)].obs_true) { impossibleX = 1;
        }
        else if ((*allNodes)[y][move_to_edge(x+2, length)].obs_true) { impossibleX = 2;
        }
        else if ((*allNodes)[y][move_to_edge(x+3, length)].obs_true) { impossibleX = 3;
        }

        if ((*allNodes)[move_to_edge(y-1, depth)][move_to_edge(x+1, length)].obs_true) { impossibleY = -1; impossibleX = 1;
        }
        if ((*allNodes)[move_to_edge(y+1, depth)][move_to_edge(x+1, length)].obs_true) { impossibleY = 1; impossibleX = 1;
        }
        if ((*allNodes)[move_to_edge(y-1, depth)][x].obs_true) { impossibleY = -1;
        }
        if ((*allNodes)[move_to_edge(y+1, depth)][x].obs_true) { impossibleY = 1;
        }
    }

    switch(action) {
        case '3':
            for (i = 1; i < 4; i++) {
                if (i == impossibleX) break;
                traverse_cell(me, &(*allNodes)[y][move_to_edge(x+i, length)], 1.0);
            }
            break;
        case '2':
            for (i = 1; i < 3; i++) {
                if (i == impossibleX) break;
                traverse_cell(me, &(*allNodes)[y][move_to_edge(x+i, length)], 1.0);
            }
            break;
        case '1':
            for (i = 1; i < 2; i++) {
                if (i == impossibleX) break;
                traverse_cell(me, &(*allNodes)[y][move_to_edge(x+i, length)], 1.0);
            }
            break;
        case 'N':
            if (impossibleY == -1 && impossibleX == 1) break;
            traverse_cell(me, &(*allNodes)[move_to_edge(y-1, depth)][move_to_edge(x+1, length)], 1.0);
            break;
        case 'S':
            if (impossibleY == 1 && impossibleX == 1) break;
            traverse_cell(me, &(*allNodes)[move_to_edge(y+1, depth)][move_to_edge(x+1, length)], 1.0);
        case '0':
            traverse_cell(me, &(*allNodes)[y][x], 1.0);
            break;
    }
}

void expand_node(TronCycle * me, Policy * pi, Node *** allNodes, int len, int depth) {
    char current_action;
    int action = (pi->actions_taken)-(me->MaxSpeed);
    while(action < 5) {
        current_action = pi->p_actions[action];
        execute_action(current_action, me, allNodes, depth, len);
        action++;
        if (action > 5) break;
    }
}

//Chooses a local node to add to the tree based purely on value.
void select_node(Node *** allNodes, TronCycle * me, GameTree * theRace, int depth) {
    signed int i;
    int j, optx, opty, sMax;
    optx = me->posx;
    opty = me->posy;
    sMax = (me->posx) + (me->MaxSpeed) + 1;
    j = (me->posx)+1;

    for (i = (me->posy)-1; i < (me->posy)+2; i++) {

        if ((i >= 0) && (i < depth)) {
            if ((*allNodes)[i][j].value() > (*allNodes)[opty][optx].value()
                && !node_in_Tree((*allNodes)[i][j], theRace->playable)) {
                optx = j;
                opty = i;
            }
        }
    }
    if (me->MaxSpeed > 1) {
        i = me->posy;
        for(j = (me->posx)+2; j < sMax; j++) {
            //Check if the node has greater value, and if it does not already exist
            //in the game tree.
            if ((*allNodes)[i][j].value() > (*allNodes)[opty][optx].value()
                && !node_in_Tree((*allNodes)[i][j], theRace->playable)) {
                optx = j;
                opty = i;
            }
        }
    }

    //If i am a pissweak bike, stop before the obstacles. Or if i cross
    //several on a mighty one, deduct some dollars.
    for (i = me->posx+1; i < optx; i++) {
        if ((*allNodes)[opty][i].obs_true && me->durability=='D') {
            optx = i-1;
            opty = me->posy;
            theRace->expenses -= 50;
            break;
        } else if ((*allNodes)[opty][i].obs_true && me->durability=='W') {
            theRace->expenses -= 5;
        }
    }

    me->posx = optx;
    me->posy = opty;

    //Store the address of the best child node into the next element of
    //the game tree.
    theRace->playable.push_back((*allNodes)[opty][optx]);
    theRace->steps++;
}

//Does the same as the above funtion without adding the node to the game tree.
void select_node_tmp(Node *** allNodes, TronCycle * me, int rows) {
    signed int i;
    int j, optx, opty, sMax;
    optx = me->posx;
    opty = me->posy;
    sMax = (me->posx) + (me->MaxSpeed) + 1;
    j = (me->posx)+1;

    for (i = (me->posy)-1; i < (me->posy)+2; i++) {

        if ((i >= 0) && (i < rows)) {
            if ((*allNodes)[i][j].value() > (*allNodes)[opty][optx].value()) {
                optx = j;
                opty = i;
            }
        }
    }
    if (me->MaxSpeed > 1) {
        i = me->posy;
        for(j = (me->posx)+2; j < sMax; j++) {
            //Check if the node has greater value, and if it does not already exist
            //in the game tree.
            if ((*allNodes)[i][j].value() > (*allNodes)[opty][optx].value()) {
                optx = j;
                opty = i;
            }
        }
    }
    //If i am a pissweak bike, stop before the obstacles. Or if i cross
    //several on a mighty one, deduct some dollars.
    for (i = me->posx; i < optx; i++) {
        if ((*allNodes)[opty][i].obs_true && me->durability=='D') {
            optx = i-1;
            opty = me->posy;
            break;
        }
    }

    //If I am moving diagonally, maybe don't.
    if (opty != me->posy) {
        srand(time(NULL));
        int a = rand() % 10;
        if (a < 8) { opty = opty;
        } else if (a < 9) {
            opty = opty;
            optx = me->posx;
        } else if (a < 10) {
            opty = me->posy;
            optx = optx;
        } else {
            opty = me->posy;
            optx = me->posx;
        }

    }
    me->posx = optx;
    me->posy = opty;
}

//Runs the simulation for a game with a temporary TronCycle and Policy until
//a terminal state is reached, then deletes the temporary things.
void simulate_game(Policy * defaultPol, int length, int depth, TronCycle * me, Node *** allNodes) {

    int valX, valY;
    valX = me->posx;
    valY = me->posy;

    Policy *thisSim;
    TronCycle *autoPilot;

    autoPilot = new TronCycle;
    thisSim = new Policy;
    autoPilot->durability = me->durability;
    autoPilot->posx = me->posx;
    autoPilot->posy = me->posy;
    autoPilot->MaxSpeed = me->MaxSpeed;
    autoPilot->reliability = me->reliability;


    //Run solo simulations until a terminal state is reached.
    while (is_terminal(valX, length) == 0) {

        thisSim->p_actions = defaultPol->p_actions;
        thisSim->actions_taken = defaultPol->actions_taken;
        expand_node(autoPilot, thisSim, allNodes, length, depth);
        select_node_tmp(allNodes, autoPilot, depth);
        valX = autoPilot->posx;
        valY = autoPilot->posy;
        if (is_terminal(valX, length) == 1) break;

    }

    delete thisSim;
    delete autoPilot;
}

//Use a random number generator to choose the action of an adversary.
void move_adversary(Adversary * them, Node *** allNodes, int length, int depth) {
    int i, j, k;
    k = 0;
    i = 0;
    signed int rowT = them->posy;
    int colT = them->posx;
    char default_actions[] = { '3', '2', '1', 'N', 'S', '0', 'T' };
    j = rand() % 10;

    //Determine which action to take based on the random number.
    while (true) {
      i += (them->adPol[them->posy][them->posx][k])*10;
      if (i >= j) break;
      k++;
      if ( k > 4) k = 0;
    }

    //Take the action and update the position of the adversary's cycle.
    switch(default_actions[k]) {
        case '3':
            them->posx = move_to_edge(colT + 3, length);
            break;
        case '2':
            them->posx = move_to_edge(colT + 2, length);
            break;
        case '1':
            them->posx = move_to_edge(colT + 1, length);
            break;
        case 'N':
            them->posx = move_to_edge(colT + 1, length);
            them->posy = move_to_edge(rowT - 1, 0);
            break;
        case 'S':
            them->posx = move_to_edge(colT + 1, length);
            them->posy = move_to_edge(rowT + 1, depth);
            break;
    }
}

void stack_players (string *node, char ID) {
    char tmp;
    tmp = (*node)[0];

    //Is there already a stack? Add to it.
    if ((*node)[0] == '[') {
        (*node).substr(0, (*node).size() - 1);
        (*node).push_back('-');
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

void update_map(Adversary ** them, TronCycle * me, Node *** allNodes,
                    GameTree * theRace, int rows, int cols, int ops) {
    int i, j, k;

    //Reset the map.
    for (i = 0; i < rows; i++) {
        for (j = 0; j < cols; j++) {
            if ((*allNodes)[i][j].obs_true) {
                (*allNodes)[i][j].tile = '1';
            } else {
                (*allNodes)[i][j].tile = '0';
            }
        }
    }

    for (i = 0; i < rows; i++) {
        for (j = 0; j < cols; j++) {

            //Determine whether the distractors are active, this called the rand()
            //function to see whether or not they are there.
            if ((*allNodes)[i][j].dist_true == 1) {
                if ((*allNodes)[i][j].distractor.isActive()) {
                    stack_players(&(*allNodes)[i][j].tile, (*allNodes)[i][j].distractor.ID);
                } else {
                    (*allNodes)[i][j].tile = '0';
                }
            }

            //Update player locations.
            if (me->posx == j && me->posy == i) {
                stack_players(&(*allNodes)[i][j].tile, me->ID);
                //If there is a distractor there, bill the fucker.
                if ((*allNodes)[i][j].isDist()) {
                    switch(me->reliability) {
                        case 'R':
                            theRace->expenses -= 10;
                            break;
                        case 'N':
                            theRace->expenses -= 75;
                            break;
                    }
                }
            }

            //Update adversary locations.
            for (k = 0; k < ops; k++) {
                if (them[k]->posx == j && them[k]->posy == i) {
                    stack_players(&(*allNodes)[i][j].tile, them[k]->ID);
                }
            }
        }
    }
}

int single_race_solver(char *tFile, TronCycle **tronPut, int numReg) {

    GameTree * thisRace;
    Node ** node;
    Adversary * adversary;
    Policy dPol;
    char buffer[1000];
    char default_actions[] = { '3', '2', '1', 'N', 'S', '0', 'T' };
    dPol.p_actions = default_actions;
    dPol.actions_taken = 3;
    int rows, cols, ops, myTeam, i, j, k, l;

    //This is the value that is returned at completion of
    //execution.
    signed int expected_reward = 0;

    thisRace = new GameTree;
    thisRace->steps = 0;
    thisRace->expenses = 0;
    thisRace->expenses -= tronPut[0]->price;

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
    thisRace->prize = atoi(buffer);

    //Init the map as a matrix of node structs.
    node = new Node*[rows];
    if (ops > 0) {
        adversary = new Adversary[ops];
    }
    myTeam = numReg;

    l = 0;
    k = 0;
    //Populate the nodes.
    for (i = 0; i < rows; i++) {
        infile >> buffer;
        node[i] = new Node[cols];
        for (j = 0; j < cols; j++) {
            node[i][j].row = i;
            node[i][j].col = j;
            node[i][j].visits = 1;
            node[i][j].reward = 1;
            node[i][j].dist_true = 0;
            node[i][j].obs_true = 0;
            node[i][j].tile = buffer[j];
            //Draw a pretty fucking map.

            //Register a member of the cycle team at one of the available starting
            //positions if there is a spot available.
            if (buffer[j] < 91 && buffer[j] > 74 && l < myTeam) {
                tronPut[l]->posx = j;
                tronPut[l]->posy = i;
                tronPut[l]->ID = buffer[j];
                l++;

            //Register an opponent cycle if they exist, and assume they
            //have mint specs.
            } else if (buffer[j] < 75 && buffer[j] > 64 && k < ops && ops > 0) {
                adversary[k].ID = buffer[j];
                adversary[k].posx = j;
                adversary[k].posy = i;
                k++;

            //If its a distractor construct the thing.
            } else if (buffer[j] > 96) {
                node[i][j].dist_true = 1;
                node[i][j].distractor.ID = buffer[j];
                node[i][j].tile = buffer[j];

            } else if (buffer[j] == '1') {
                node[i][j].obs_true = 1;
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
           if ( infile.eof() ) break;
        }
    }

    //Get distractor details.
    while (true) {
        infile >> buffer;
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                if (node[i][j].tile[0] == buffer[0]) {
                    infile >> buffer;
                    node[i][j].distractor.p = atof(buffer);
                    i = rows;
                    j = cols;
                }
            }
        }
        if ( infile.eof() ) break;
    }

    //Store address of the starting node.
    thisRace->raceLen = cols;
    thisRace->playable.push_back(node[tronPut[0]->posy][tronPut[0]->posx]);

    //Print the inital state of the map.
    for (i = 0; i < rows; i++) {
        for (j = 0; j < cols; j++) {
            cout << node[i][j].tile;
        }
        cout << endl;
    }
    cout << endl;

    int tFlag = 0;

    while (true) {

        simulate_game(&dPol, cols, rows, tronPut[0], &node);

        expand_node(tronPut[0], &dPol, &node, cols, rows);

        select_node(&node, tronPut[0], thisRace, rows);

        if (ops > 0) {
            for (i = 0; i < ops; i++) {
                move_adversary(&adversary[i], &node, cols, rows);
            }
        }
        update_map(&adversary, tronPut[0], &node, thisRace,  rows, cols, ops);

        //Print the map.
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                cout << node[i][j].tile;
            }
            cout << endl;
        }
        cout << endl;

        //If a player has won, break.
        if (is_terminal(tronPut[0]->posx, cols)) break;

        //If an adversary has won, set the flag and break.
        if (ops > 0) {
            for (i = 0; i < ops; i++) {
                if (is_terminal(adversary[i].posx, cols)) tFlag++;
            }
        }

        if (tFlag > 0) break;

    }

    if (tFlag > 0) {
        expected_reward = thisRace->expenses;
        cout << "LOSER!!!!" << endl;
    } else {
        expected_reward = thisRace->prize + thisRace->expenses;
        cout << "WINNER!!!!" << endl;
    }
    //Clear the memory I have used.
    for (i = 0; i < rows; i++) {
        delete [] node[i];
    }
    delete [] node;
    delete [] adversary;
    delete thisRace;

    //Return the expected output.
    return expected_reward;
}
