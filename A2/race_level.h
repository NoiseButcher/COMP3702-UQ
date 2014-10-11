#include "utility_things.h"

/*This file handles the race level, and should recieve arguments of
a file and a string. The file will contain the track and opponent data,
the string will be the cycle(s) used for this race. The return value on
this program is the expected reward [prizes - damages] from the completion
of the track using MCTS to find the most high yield path.
*/

float traverse_cell(TronCycle * me, Node * thisNode, float p) {
    //If there is a distractor in the cell, cause some damage
    //based on E[distractor].
    if (thisNode->isDist() && me->reliability == 'N') {
//        thisNode->reward -= 75;
        thisNode->reward += 0.1*p;
        thisNode->visits++;

    } else if (thisNode->isDist() && me->reliability == 'R') {
//        thisNode->reward -= 10;
        thisNode->reward += 0.5*p;
        thisNode->visits++;

    //If there is an obstacle in the cell, cause some more damage.
    } else if (thisNode->obs_true && me->durability == 'W') {
//        thisNode->reward -= 5;
        thisNode->reward += 0.8*p;
        thisNode ->visits++;

    //Note that this cell will have 0 visits for  domestic cycles,
    //so will almost certainly be removed from the tree during pruning.
    }  else if (thisNode->obs_true && me->durability == 'D') {
//        thisNode->reward -= 50;
        thisNode ->visits++;

    //Otherwise, assume it is a clearway.
    } else {
        thisNode->reward += 5*p;
        thisNode->visits ++;
    }

    return thisNode->value();
}

float execute_action(int action, TronCycle * me, Node *** allNodes, int depth, int length) {
    float z = 0;
    int x = me->posx;
    int y = me->posy;

    switch(action) {
        case 0:
            if ((*allNodes)[y][move_to_edge(x+3, length)].obs_true) { break;
            } else { z = traverse_cell(me, &(*allNodes)[y][move_to_edge(x+3, length)], 1.0);
            }
            break;
        case 1:
            if ((*allNodes)[y][move_to_edge(x+2, length)].obs_true) { break;
            } else { z = traverse_cell(me, &(*allNodes)[y][move_to_edge(x+2, length)], 1.0);
            }
            break;
        case 2:
            if ((*allNodes)[y][move_to_edge(x+1, length)].obs_true) { break;
            } else { z = traverse_cell(me, &(*allNodes)[y][move_to_edge(x+1, length)], 1.0);
            }
            break;
        case 3:
            if ((*allNodes)[move_to_edge(y-1, depth)][move_to_edge(x+1, length)].obs_true) { break;
            } else { z = traverse_cell(me, &(*allNodes)[move_to_edge(y-1, depth)][move_to_edge(x+1, length)], 1.0);
            }
            break;
        case 4:
            if ((*allNodes)[move_to_edge(y+1, depth)][move_to_edge(x+1, length)].obs_true) { break;
            } else { z = traverse_cell(me, &(*allNodes)[move_to_edge(y+1, depth)][move_to_edge(x+1, length)], 1.0);
            }
            break;
        case 5:
            z = traverse_cell(me, &(*allNodes)[y][x], 0.5);
            if (me->ImaChurch == 1) {
                (*allNodes)[y][x].block_true = me->ImaChurch;
            }
            break;
        case 6:
            z = traverse_cell(me, &(*allNodes)[y][x], 0.5);
            if ((*allNodes)[y][x].tile[0] != '1') (*allNodes)[y][x].block_true = me->ImaChurch;
            break;
    }
    return z;
}

void expand_node(TronCycle * me, float * policy, Node *** allNodes, int len, int depth) {
    int i, action;
    if (me->ImaChurch == 1) {
        action = 5;
    } else {
        action = 3-(me->MaxSpeed);
    }

    for (i = action; i < 7; i++) {
        policy[i] += execute_action(i, me, allNodes, depth, len);
    }
}

//Chooses a local node to add to the tree based purely on value.
void select_node(Node *** allNodes, TronCycle * me, GameTree * theRace, int depth, int length) {
    signed int i;
    int k;
    int optx, opty;
    optx = me->posx;
    opty = me->posy;

    k = theRace->best_child(theRace->steps);

    if (k < (3 - me->MaxSpeed)) {
        k = (3 - me->MaxSpeed);
    }

    if (me->ImaChurch == 1 && k < 5) {
        k = 5;
    }

    //Determine which action I am taking and execute it.
    switch(k) {
        //FF
        case 0:
            optx = move_to_edge(optx+=3, length);
            break;
        //FM
        case 1:
            optx = move_to_edge(optx+=2, length);
            break;
        //FS
        case 2:
            optx = move_to_edge(optx+=1, length);
            break;
        //NE
        case 3:
            optx = move_to_edge(optx+=1, length);
            opty = move_to_edge(opty-=1, depth);
            break;
        //SE
        case 4:
            optx = move_to_edge(optx+=1, length);
            opty = move_to_edge(opty+=1, depth);
            break;
        //ST
        case 5:
            break;
        //T
        case 6:
            if (me->ImaChurch == 0) {
                srand(time(NULL)*optx*opty);
                int a = rand() % 100;
                if (a < 71) {
                    me->ImaChurch = 1;
                }
                else {
                    me->ImaChurch = 0;
                }
            }
            break;
    }

    //If i am a pissweak bike, stop before the obstacles. Or if i cross
    //several on a mighty one, deduct some dollars. This includes hitting my buddies.
    for (i = me->posx+1; i < optx; i++) {
        if (((*allNodes)[opty][i].obs_true || ((*allNodes)[opty][i].block_true && me->ImaChurch == 0)) && me->durability=='D') {
            optx = i-1;
            opty = me->posy;
            theRace->expenses -= 50;
            break;
        } else if (((*allNodes)[opty][i].obs_true || ((*allNodes)[opty][i].block_true && me->ImaChurch == 0))
                && me->durability=='W') {
            theRace->expenses -= 5;
        }
    }

    if (opty != me->posy) {
        srand(time(NULL)*theRace->steps*opty);
        int a = rand() % 101;
        if (a < 70) { opty = opty;
        } else if (a < 80 && a > 70) {
            opty = opty;
            optx = me->posx;
        } else if (a > 80 && a < 90) {
            opty = me->posy;
            optx = optx;
        } else {
            opty = me->posy;
            optx = me->posx;
        }
    }

    me->posx = optx;
    me->posy = opty;

    if ((*allNodes)[opty][optx].isDist() && me->reliability == 'N') {
        theRace->expenses -= 75;
    } else if ((*allNodes)[opty][optx].isDist() && me->reliability == 'R') {
        theRace->expenses -= 10;
    }

    theRace->steps++;
}

//Chooses an action at random.
void select_node_X(Node *** allNodes, TronCycle * me, GameTree * theRace, int depth, int length) {
    int i;
    int optx, opty, a, k;
    optx = me->posx;
    opty = me->posy;

    srand(time(NULL)*theRace->steps*7.2);
    a = rand() % 7*(theRace->steps+1)*1.5;
    k = 0;

    i = 0;

    while(true) {
        i += theRace->policy[theRace->steps][k];
        if (i > a) break;
        k++;
        if (k > 6) k = 0;
    }

    if (k < (3 - me->MaxSpeed)) {
        k = (3 - me->MaxSpeed);
    }


    //Determine which action I am taking and execute it.
    switch(k) {
        //FF
        case 0:
            optx = move_to_edge(optx+=3, length);
            break;
        //FM
        case 1:
            optx = move_to_edge(optx+=2, length);
            break;
        //FS
        case 2:
            optx = move_to_edge(optx+=1, length);
            break;
        //NE
        case 3:
            optx = move_to_edge(optx+=1, length);
            opty = move_to_edge(opty-=1, depth);
            break;
        //SE
        case 4:
            optx = move_to_edge(optx+=1, length);
            opty = move_to_edge(opty+=1, depth);
            break;
        //ST
        case 5:
            break;
        //T
        case 6:
            if (me->ImaChurch == 0) {
                srand(time(NULL)*optx*opty);
                int a = rand() % 100;
                if (a < 71) {
                    me->ImaChurch = 1;
                    }
            } else {
                me->ImaChurch = 0;
            }
            break;
    }

    //If i am a pissweak bike, stop before the obstacles. Or if i cross
    //several on a mighty one, deduct some dollars. This includes hitting my buddies.
    for (i = me->posx+1; i < optx; i++) {
        if ((((*allNodes)[opty][i].obs_true || ((*allNodes)[opty][i].block_true) && me->ImaChurch == 0)) && me->durability=='D') {
            optx = i-1;
            opty = me->posy;
            theRace->expenses -= 50;
            break;
        } else if (((*allNodes)[opty][i].obs_true || ((*allNodes)[opty][i].block_true) && me->ImaChurch == 0)
                && me->durability=='W') {
            theRace->expenses -= 5;
        }
    }

    if (opty != me->posy) {
        srand(time(NULL)*theRace->steps*optx);
        a = rand() % 101;
        if (a < 70) { opty = opty;
        } else if (a < 80 && a > 70) {
            opty = opty;
            optx = me->posx;
        } else if (a > 80 && a < 90) {
            opty = me->posy;
            optx = optx;
        } else {
            opty = me->posy;
            optx = me->posx;
        }
    }

    me->posx = optx;
    me->posy = opty;

    if ((*allNodes)[opty][optx].isDist() && me->reliability == 'N') {
        theRace->expenses -= 75;
    } else if ((*allNodes)[opty][optx].isDist() && me->reliability == 'R') {
        theRace->expenses -= 10;
    }

    theRace->steps++;
}

//Runs the simulation for a game with a temporary TronCycle and Policy until
//a terminal state is reached, then deletes the temporary things.
void simulate_game(GameTree ** theTruth, int length, int depth, TronCycle ** me, Node *** allNodes, int ops,
    Adversary ** opponents, int num) {

    int simSteps, i, j, maxSteps;
    int valX[num];
    float default_weights[] = {1, 1, 1, 1, 1, 1, 1};
    if (ops == 0) {
        default_weights[5] = 0;
        default_weights[6] = 0;
    }

    int DoomFlag = 0;
    int YayFlag = 0;

    GameTree ** simTree;
    TronCycle ** autoPilot;
    Adversary ** chariots;

    simTree = new GameTree*[num];
    for (i = 0; i < num; i++) {
        simTree[i] = new GameTree;
        simTree[i]->policy.push_back(default_weights);
        simTree[i]->steps = 0;
        simTree[i]->prize = theTruth[i]->prize;
    }

    chariots = new Adversary*[ops];
    for (i = 0; i < ops; i++) {
        chariots[i] = new Adversary;
        chariots[i]->adPol = opponents[i]->adPol;
        chariots[i]->posx = opponents[i]->posx;
        chariots[i]->posy = opponents[i]->posy;
    }

    autoPilot = new TronCycle*[num];
    for (i = 0; i < num; i++) {
        autoPilot[i] = new TronCycle;
        autoPilot[i]->durability = me[i]->durability;
        autoPilot[i]->posx = me[i]->posx;
        autoPilot[i]->posy = me[i]->posy;
        autoPilot[i]->MaxSpeed = me[i]->MaxSpeed;
        autoPilot[i]->reliability = me[i]->reliability;
        autoPilot[i]->ImaChurch = me[i]->ImaChurch;
        valX[i] = me[i]->posx;
    }
    //Run solo simulations until a hard limit is reached OR
    //enough wins have been achieved.
    while (true) {
        for (i = 0; i < num; i++) {
            simTree[i]->policy.push_back(default_weights);
            expand_node(autoPilot[i], simTree[i]->policy[simTree[i]->steps], allNodes, length, depth);
            select_node_X(allNodes, autoPilot[i], simTree[i], depth, length);
            valX[i] = autoPilot[i]->posx;
            if (simTree[i]->steps > length*2) DoomFlag++;
            if (is_terminal(valX[i], length)) {
                YayFlag++;
                maxSteps = simTree[i]->steps;
            }
            if (DoomFlag + YayFlag > 0) break;
        }

        //If I win this simulation, backtrack, and boost reward for actions.
        if (YayFlag > 0) {
            for (i = 0; i < num; i++) {
                update_gametree_sim(simTree[i], 8);
                autoPilot[i]->posx = me[i]->posx;
                autoPilot[i]->posy = me[i]->posy;
                autoPilot[i]->ImaChurch = me[i]->ImaChurch;
                valX[i] = autoPilot[i]->posx;
                maxSteps = simTree[i]->steps;
                simTree[i]->steps = 0;
            }
            for (i = 0; i < ops; i++) {
                chariots[i]->posx = opponents[i]->posx;
                chariots[i]->posy = opponents[i]->posy;
            }
            for (i = 0; i < depth; i++) {
                for (j = 0; j < length; j++) {
                    (*allNodes)[i][j].block_true = 0;
                    (*allNodes)[i][j].reward = 0;
                    (*allNodes)[i][j].visits = 1;
                }
            }
            break;
        }

        for (i = 0; i < ops; i++) {
            move_adversary(chariots[i], allNodes, length, depth);
            if (is_terminal(chariots[i]->posx, length)) DoomFlag = 1;
        }

        //If the opponents win, backtrack and reset the map.
        if (DoomFlag > 0) {
            for (i = 0; i < num; i++) {
                update_gametree_sim(simTree[i], 0);
                autoPilot[i]->posx = me[i]->posx;
                autoPilot[i]->posy = me[i]->posy;
                autoPilot[i]->ImaChurch = me[i]->ImaChurch;
                valX[i] = autoPilot[i]->posx;
                simTree[i]->steps = 0;
            }
            for (i = 0; i < ops; i++) {
                chariots[i]->posx = opponents[i]->posx;
                chariots[i]->posy = opponents[i]->posy;
            }
            for (i = 0; i < depth; i++) {
                for (j = 0; j < length; j++) {
                    (*allNodes)[i][j].block_true = 0;
                    (*allNodes)[i][j].reward = 0;
                    (*allNodes)[i][j].visits = 1;
                }
            }
            break;
        }
    }


    YayFlag = 0;
    DoomFlag = 0;
    simSteps = 0;

    while (simSteps < 20*length) {

        for (i = 0; i < num; i++) {
            if (simTree[i]->steps >= maxSteps) {
                simTree[i]->policy.push_back(default_weights);
                maxSteps++;
            } else if (simTree[i]->steps < maxSteps) {
                simTree[i]->policy.push_back(default_weights);
            }
        }

        for (i = 0; i < num; i++) {
            expand_node(autoPilot[i], simTree[i]->policy[simTree[i]->steps], allNodes, length, depth);
            select_node_X(allNodes, autoPilot[i], simTree[i], depth, length);
            valX[i] = autoPilot[i]->posx;
            if (simTree[i]->steps > length*2) DoomFlag++;
            if (is_terminal(valX[i], length)) YayFlag++;
            if (DoomFlag + YayFlag > 0) break;
        }
        //If I win this simulation, backtrack, and boost reward for actions.
        if (YayFlag > 0) {
            DoomFlag = 0;
            YayFlag = 0;
            for ( i = 0; i < num; i++) {
                update_gametree_sim(simTree[i], 8);
                simTree[i]->steps = 0;
                autoPilot[i]->posx = me[i]->posx;
                autoPilot[i]->posy = me[i]->posy;
                autoPilot[i]->ImaChurch = me[i]->ImaChurch;
                valX[i] = autoPilot[i]->posx;
                autoPilot[i]->ImaChurch = me[i]->ImaChurch;
            }
            simSteps++;
            for (i = 0; i < ops; i++) {
                chariots[i]->posx = opponents[i]->posx;
                chariots[i]->posy = opponents[i]->posy;
            }
            for (i = 0; i < depth; i++) {
                for (j = 0; j < length; j++) {
                    (*allNodes)[i][j].block_true = 0;
                    (*allNodes)[i][j].reward = 0;
                    (*allNodes)[i][j].visits = 1;
                }
            }
        }
        for (i = 0; i < ops; i++) {
            move_adversary(chariots[i], allNodes, length, depth);
            if (is_terminal(chariots[i]->posx, length)) DoomFlag = 1;
        }
        //If the opponents win, backtrack and reset the map.
        if (DoomFlag > 0) {
            DoomFlag = 0;
            YayFlag = 0;
            for ( i = 0; i < num; i++) {
                update_gametree_sim(simTree[i], 0);
                simTree[i]->steps = 0;
                autoPilot[i]->posx = me[i]->posx;
                autoPilot[i]->posy = me[i]->posy;
                autoPilot[i]->ImaChurch = me[i]->ImaChurch;
                valX[i] = autoPilot[i]->posx;
                autoPilot[i]->ImaChurch = me[i]->ImaChurch;
            }
            simSteps++;
            for (i = 0; i < ops; i++) {
                chariots[i]->posx = opponents[i]->posx;
                chariots[i]->posy = opponents[i]->posy;
            }
            for (i = 0; i < depth; i++) {
                for (j = 0; j < length; j++) {
                    (*allNodes)[i][j].block_true = 0;
                    (*allNodes)[i][j].reward = 0;
                    (*allNodes)[i][j].visits = 1;
                }
            }
        }
    }

    for (i = 0; i < num; i++) {
        update_gametree(theTruth[i], simTree[i]);
    }

    for (i = 0; i < ops; i++) {
        delete chariots[i];
    }
    delete [] chariots;
    for (i = 0; i < num; i++) {
        delete simTree[i];
    }
    delete [] simTree;
    for (i = 0; i < num; i++) {
        delete autoPilot[i];
    }
    delete [] autoPilot;
}

void update_map(Adversary ** them, TronCycle ** me, Node *** allNodes,
                    GameTree ** theRace, int rows, int cols, int ops, int us) {
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

            for (k = 0; k < us; k++) {
                //Update player locations.
                if (me[k]->posx == j && me[k]->posy == i) {
                    stack_players(&(*allNodes)[i][j].tile, me[k]->ID);
                    //If there is a distractor there, bill the fucker.
                    if ((*allNodes)[i][j].isDist()) {
                        switch(me[k]->reliability) {
                            case 'R':
                                theRace[k]->expenses -= 10;
                                break;
                            case 'N':
                                theRace[k]->expenses -= 75;
                                break;
                        }
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

int single_race_solver(char * tFile, TronCycle * tronIn, int total) {

    GameTree ** thisRace;
    Node ** node;
    TronCycle ** tronPut;
    Adversary ** adversary;
    char buffer[1000];
    int rows, cols, ops, i, j, k, l;
    float default_weights[] = {1, 1, 1, 1, 1, 1, 1};

    //This is the value that is returned at completion of
    //execution.
    signed int expected_reward = 0;

    //Construct a new game tree for each human player, start with no
    //expenses, steps at zero and a balanced policy for the first move.
    thisRace = new GameTree*[total];
    for (l = 0; l < total; l++) {
        thisRace[l] = new GameTree;
        thisRace[l]->steps = 0;
        thisRace[l]->expenses = 0;
        thisRace[l]->policy.push_back(default_weights);
    }

    tronPut = new TronCycle*[total];
    for (l = 0; l < total; l++) {
        tronPut[l] = new TronCycle;
        tronPut[l]->durability = tronIn[l].durability;
        tronPut[l]->reliability = tronIn[l].reliability;
        tronPut[l]->MaxSpeed = tronIn[l].MaxSpeed;
        tronPut[l]->price = tronIn[l].price;
    }

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
    //Register my dudes.
    for (l = 0; l < total; l++) {
        thisRace[l]->expenses -= atoi(buffer);
    }
    infile >> buffer;
    for (l = 0; l < total; l++) {
        thisRace[l]->prize = atoi(buffer);
        thisRace[l]->raceLen = cols;
    }

    if (ops == 0) {
        default_weights[5] = 0;
        default_weights[6] = 0;
    }


    //Init the map as a matrix of node structs.
    node = new Node*[rows];
    if (ops > 0) {
        adversary = new Adversary*[ops];
    }

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
            node[i][j].reward = 0;
            node[i][j].dist_true = 0;
            node[i][j].obs_true = 0;
            node[i][j].block_true = 0;
            node[i][j].distractor.p = 1;
            node[i][j].distractor.ID = '!';
            node[i][j].tile = buffer[j];

            //Register a member of the cycle team at one of the available starting
            //positions if there is a spot available.
            if (buffer[j] < 91 && buffer[j] > 74 && l < total) {
                tronPut[l]->posx = j;
                tronPut[l]->posy = i;
                tronPut[l]->ID = buffer[j];
                tronPut[l]->ImaChurch = 0;
//                thisRace[l]->expenses -= tronPut[l]->price;
                l++;

            //Register an opponent cycle if they exist, and assume they
            //have mint specs.
            } else if (buffer[j] < 75 && buffer[j] > 64 && k < ops && ops > 0) {
                adversary[k] = new Adversary;
                adversary[k]->ID = buffer[j];
                adversary[k]->posx = j;
                adversary[k]->posy = i;
                k++;
            //If its a distractor construct the thing.
            } else if (buffer[j] > 96) {
                node[i][j].dist_true = 1;
                node[i][j].distractor.ID = buffer[j];

            } else if (buffer[j] == '1') {
                node[i][j].obs_true = 1;
            } else {
                node[i][j].tile = '0';
            }
        }
    }

    i = 0;

    //Get Adversary deets.
    if (ops > 0) {
        while (true) {
            infile >> buffer;
            while (i < ops) {
                if (adversary[i]->ID == buffer[0]) {
                    adversary[i]->adPol = new float**[rows];
                    for (j = 0; j < rows ; j++) {
                        adversary[i]->adPol[j] = new float*[cols];
                        for (l = 0; l < cols; l++) {
                            adversary[i]->adPol[j][l] = new float[6];
                            for (k = 0; k < 6; k++) {
                                infile >> buffer;
                                adversary[i]->adPol[j][l][k] = (atof(buffer));
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
        if (buffer[0] < 97) {
            infile >> buffer;
        }
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

    infile.close();

//    Print the inital state of the map.
//    for (i = 0; i < rows; i++) {
//        for (j = 0; j < cols; j++) {
//            cout << node[i][j].tile;
//        }
//        cout << endl;
//    }
//    cout << endl;

    int Oflag = 0;
    int Pflag = 0;

    //Game loop.
    while (true) {

        //Run simulations, choose expansion point and add a new layer of policy.
        simulate_game(thisRace, cols, rows, tronPut, &node, ops, adversary, total);

        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                node[i][j].block_true = 0;
            }
        }

        for (l = 0; l < total; l++) {
            thisRace[l]->policy.push_back(default_weights);
            select_node(&node, tronPut[l], thisRace[l], rows, cols);
        }

        if (ops > 0) {
            for (i = 0; i < ops; i++) {
                move_adversary(adversary[i], &node, cols, rows);
            }
        }

        update_map(adversary, tronPut, &node, thisRace,  rows, cols, ops, total);

        //Print the map.
//        for (i = 0; i < rows; i++) {
//            for (j = 0; j < cols; j++) {
//                cout << node[i][j].tile;
//                node[i][j].reward = 0;
//                node[i][j].visits = 1;
//            }
//            cout << endl;
//        }
//        cout << endl;

        //If a player has won, break.

        for (i = 0; i < total; i++) {
            if (is_terminal(tronPut[i]->posx, cols)) Pflag++;
            if (thisRace[i]->steps > cols*2) Oflag++;
        }

        if (Pflag > 0) break;

        //If an adversary has won, set the flag and break.
        if (ops > 0) {
            for (i = 0; i < ops; i++) {
                if (is_terminal(adversary[i]->posx, cols)) Oflag++;
            }
        }

        if (Oflag > 0) break;

    }

    //Determine winner and return the reward.
    if (Oflag > 0) {
        for (l = 0; l < total; l++) {
            expected_reward += thisRace[l]->expenses;
//            cout << thisRace[l]->expenses << endl;
        }
//        cout << tFile << " LOSER!!!!" << endl;
    } else {
        expected_reward += thisRace[0]->prize;
        for (l = 0; l < total; l++) {
            expected_reward += thisRace[l]->expenses;
//            cout << thisRace[l]->expenses << endl;
        }
//        cout << tFile << " WINNER!!!!" << endl;
    }
    //Clear the memory I have used.
    for (i = 0; i < rows; i++) {
        delete [] node[i];
    }
    if (ops > 0) {
        for (k = 0; k < ops; k++) {
            for (i = 0; i < rows; i++) {
                for (j = 0; j < cols; j++) {
                    delete [] adversary[k]->adPol[i][j];
                }
                delete [] adversary[k]->adPol[i];
            }
        }
    }
    for (i = 0; i < total; i++) {
        delete tronPut[i];
    }
    delete [] tronPut;
    delete [] adversary;
    delete [] node;
    delete [] thisRace;

    //Return the expected output.
    return expected_reward;
}
