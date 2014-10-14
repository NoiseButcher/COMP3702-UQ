#include "utility_things.h"

/*This file handles the races, and the various MCTS functionality.
HERE IS WHERE THE WEIGHTS FOR EXPLORE VS EXPLOIT NEED TO BE TINKERED WITH.
*/

float traverse_cell(TronCycle * me, Node * thisNode, float p) {
    //If there is a distractor in the cell, cause some damage
    //based on E[distractor].
    if (thisNode->isDist() && me->reliability == 'N') {
        thisNode->reward -= 1.5*p;
        thisNode->visits++;

    } else if (thisNode->isDist() && me->reliability == 'R') {
        thisNode->reward += 0.4*p;
        thisNode->visits++;

    //If there is an obstacle in the cell, cause some more damage.
    } else if (thisNode->obs_true && me->durability == 'W') {
        thisNode->reward += 0.8*p;
        thisNode->visits++;

    //Note that this cell will have 0 visits for  domestic cycles,
    //so will almost certainly be removed from the tree during pruning.
    }  else if (thisNode->obs_true && me->durability == 'D') {
        thisNode->reward -= 2.0*p;
        thisNode->visits++;

    //Otherwise, assume it is a clearway.
    } else {
        thisNode->reward += 1.0*p;
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
            z = traverse_cell(me, &(*allNodes)[y][move_to_edge(x+3, length)], 0.5);
            z += traverse_cell(me, &(*allNodes)[y][move_to_edge(x+2, length)], 0.6);
            z += traverse_cell(me, &(*allNodes)[y][move_to_edge(x+1, length)], 0.6);
            break;
        case 1:
            z = traverse_cell(me, &(*allNodes)[y][move_to_edge(x+2, length)], 0.6);
            z += traverse_cell(me, &(*allNodes)[y][move_to_edge(x+1, length)], 0.8);
            break;
        case 2:
            z = traverse_cell(me, &(*allNodes)[y][move_to_edge(x+1, length)], 1.1);
            break;
        case 3:
            z = traverse_cell(me, &(*allNodes)[move_to_edge(y-1, depth)][move_to_edge(x+1, length)], 1.0);
            break;
        case 4:
            z = traverse_cell(me, &(*allNodes)[move_to_edge(y+1, depth)][move_to_edge(x+1, length)], 1.0);
            break;
        case 5:
            z = traverse_cell(me, &(*allNodes)[y][x], 0.4);
            if (me->ImaChurch == 1) {
                (*allNodes)[y][x].block_true = me->ImaChurch;
            }
            break;
        case 6:
            z = traverse_cell(me, &(*allNodes)[y][x], 0.40);
            if (me->ImaChurch == 1) {
                me->ImaChurch = 0;
                (*allNodes)[y][x].block_true = me->ImaChurch;
            } else {
                me->ImaChurch = 1;
                (*allNodes)[y][x].block_true = me->ImaChurch;
            }
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
        if (i == 3) {
            if (me->posy > 0) {
                policy[i] += execute_action(i, me, allNodes, depth, len);
            }
        } else if (i == 4) {
            if (me->posy < depth - 1) {
                policy[i] += execute_action(i, me, allNodes, depth, len);
            }
        } else {
            policy[i] += execute_action(i, me, allNodes, depth, len);
        }
    }
}

//Chooses a local node to add to the tree based purely on value.
void select_node(Node *** allNodes, TronCycle * me, GameTree * theRace, int depth, int length, int ops) {
    signed int i;
    int k;
    int optx, opty;
    string actions[8] = {"FF", "FM", "FS", "NE", "SE", "ST", "TO", "TC"};
    optx = me->posx;
    opty = me->posy;
    me->damages.push_back(0.0);
    int maxDex = 7;
    if ( maxDex > 4 && ops == 0) maxDex = 5;

    k = theRace->best_child(theRace->steps, maxDex);

    if (k < (3 - me->MaxSpeed)) {
        k = (3 - me->MaxSpeed);
    }

    if (me->ImaChurch == 1 && k < 5) {
        k = 5;
    }

    if (k == 3 && me->posy == 0) k = 2;
    if (k == 4 && me->posy == depth-1) k = 2;
    //Determine which action I am taking and execute it.
    switch(k) {
        //FF
        case 0:
            optx = move_to_edge(optx+=3, length);
            me->action.push_back(actions[0]);
            break;
        //FM
        case 1:
            optx = move_to_edge(optx+=2, length);
            me->action.push_back(actions[1]);
            break;
        //FS
        case 2:
            optx = move_to_edge(optx+=1, length);
            me->action.push_back(actions[2]);
            break;
        //NE
        case 3:
            optx = move_to_edge(optx+=1, length);
            opty = move_to_edge(opty-=1, depth);
            me->action.push_back(actions[3]);
            break;
        //SE
        case 4:
            optx = move_to_edge(optx+=1, length);
            opty = move_to_edge(opty+=1, depth);
            me->action.push_back(actions[4]);
            break;
        //ST
        case 5:
            me->action.push_back(actions[5]);
            break;
        //T
        case 6:
            if (me->ImaChurch == 0) {
                srand(time(NULL)*optx*opty);
                int a = rand() % 100;
                if (a < 71) {
                    me->ImaChurch = 1;
                    me->action.push_back(actions[6]);
                }
                else {
                    me->action.push_back(actions[5]);
                }
            } else {
                me->ImaChurch = 0;
                me->action.push_back(actions[7]);
            }
            break;
    }

    //If i am a pissweak bike, stop before the obstacles. Or if i cross
    //several on a mighty one, deduct some dollars. This includes hitting my buddies.
    for (i = me->posx + 1; i < optx + 1; i++) {
        if (((*allNodes)[opty][i].obs_true || ((*allNodes)[opty][i].block_true && me->ImaChurch == 0)) && me->durability=='D') {
            optx = i-1;
            opty = me->posy;
            theRace->expenses -= 50;
            me->damages[me->damages.size() - 1] += 50.0;
            break;
        } else if (((*allNodes)[opty][i].obs_true || ((*allNodes)[opty][i].block_true && me->ImaChurch == 0))
                && me->durability=='W') {
            theRace->expenses -= 5;
            me->damages[me->damages.size() - 1] += 5.0;
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
        me->damages[me->damages.size() - 1] += 75.0;
    } else if ((*allNodes)[opty][optx].isDist() && me->reliability == 'R') {
        theRace->expenses -= 10;
        me->damages[me->damages.size() - 1] += 10.0;
    }

    theRace->steps++;
}

//Chooses an action at random.
void select_node_X(Node *** allNodes, TronCycle * me, GameTree * theRace, int depth, int length, int ops) {
    int i, maxSlider;
    int optx, opty, a, k;
    optx = me->posx;
    opty = me->posy;

    if (ops < 1) {
        maxSlider = 5;
    } else {
        maxSlider = 7;
    }
    srand(time(NULL)*(theRace->steps + 6)*7.2*(me->posx + 1));
    k = rand() % maxSlider;

    if (k < (3 - me->MaxSpeed)) {
        k = (3 - me->MaxSpeed);
    }


//    cout << k << "select_X issue." << endl;

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
    for (i = me->posx + 1; i < optx + 1; i++) {
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
    maxSteps = 0;

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

    if (ops > 0) {
        chariots = new Adversary*[ops];
        for (i = 0; i < ops; i++) {
            chariots[i] = new Adversary;
            chariots[i]->adPol = opponents[i]->adPol;
            chariots[i]->posx = opponents[i]->posx;
            chariots[i]->posy = opponents[i]->posy;
        }
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
//        cout << "sim 1 loop" << endl;
        for (i = 0; i < num; i++) {
            simTree[i]->policy.push_back(default_weights);
            expand_node(autoPilot[i], simTree[i]->policy[simTree[i]->steps], allNodes, length, depth);
            select_node_X(allNodes, autoPilot[i], simTree[i], depth, length, ops);
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
                update_gametree_sim(simTree[i], 2.5);
                update_gametree(theTruth[i], simTree[i]);
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
        if (ops > 0) {
            for (i = 0; i < ops; i++) {
                move_adversary(chariots[i], allNodes, length, depth);
                if (is_terminal(chariots[i]->posx, length)) DoomFlag++;
            }
        }
        //If the opponents win, backtrack and reset the map.
        if (DoomFlag > 0) {
            for (i = 0; i < num; i++) {
                update_gametree_sim(simTree[i], -0.09);
                update_gametree(theTruth[i], simTree[i]);
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

    while (simSteps < 35/num) {
//        cout << "sim 2 loop" << endl;
        for (i = 0; i < num; i++) {
            if (simTree[i]->steps >= maxSteps) {
                maxSteps++;
                for (j = 0; j < num; j++) {
                    if (simTree[j]->steps < maxSteps) simTree[j]->policy.push_back(default_weights);
                }
            }
        }

        for (i = 0; i < num; i++) {
            simTree[i]->policy[simTree[i]->steps] = default_weights;
            expand_node(autoPilot[i], simTree[i]->policy[simTree[i]->steps], allNodes, length, depth);
            select_node_X(allNodes, autoPilot[i], simTree[i], depth, length, ops);
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
                update_gametree_sim(simTree[i], 2.5);
                update_gametree(theTruth[i], simTree[i]);
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
        if (ops > 0) {
            for (i = 0; i < ops; i++) {
                move_adversary(chariots[i], allNodes, length, depth);
                if (is_terminal(chariots[i]->posx, length)) DoomFlag++;
            }
        }
        //If the opponents win, backtrack and reset the map.
        if (DoomFlag > 0) {
            DoomFlag = 0;
            YayFlag = 0;
            for ( i = 0; i < num; i++) {
                update_gametree_sim(simTree[i], -0.09);
                update_gametree(theTruth[i], simTree[i]);
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

    if (ops > 0) {
        for (i = 0; i < ops; i++) {
            delete chariots[i];
        }
        delete [] chariots;
    }
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
