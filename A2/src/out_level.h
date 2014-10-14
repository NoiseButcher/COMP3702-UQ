#include "race_level.h"

void single_race_output (char * tFile, TronCycle * tronIn, int total, char * oFile) {

    GameTree ** thisRace;
    Node ** node;
    TronCycle ** tronPut;
    Adversary ** adversary;
    char buffer[1000];
    char buffer2[1000];
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
        tronPut[l]->name = tronIn[l].name;
        tronPut[l]->durability = tronIn[l].durability;
        tronPut[l]->reliability = tronIn[l].reliability;
        tronPut[l]->MaxSpeed = tronIn[l].MaxSpeed;
        tronPut[l]->price = tronIn[l].price;
        tronPut[l]->damages.push_back(0.0);
    }

    //open the track file -- first argument.
    fstream infile;
    fstream outfile;
    fstream tmpfileXX;
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
        infile >> buffer2;
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                if (node[i][j].distractor.ID == buffer[0]) {
                    node[i][j].distractor.p = atof(buffer2);
                }
            }
        }
        if ( infile.eof() ) break;
    }
    infile.close();

    tmpfileXX.open("TEMP.txt", ios::out | ios::trunc);
    outfile.open(oFile, ios::out | ios::app);
    outfile << tFile << " ";
    int Oflag = 0;
    int Pflag = 0;
    //Game loop.
    while (true) {
        //Print the map to file.
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                tmpfileXX << node[i][j].tile;
                node[i][j].reward = 0;
                node[i][j].visits = 1;
            }
            tmpfileXX << endl;
        }
        //Run simulations, choose expansion point and add a new layer of policy.
        simulate_game(thisRace, cols, rows, tronPut, &node, ops, adversary, total);
        for (i = 0; i < rows; i++) {
            for (j = 0; j < cols; j++) {
                node[i][j].block_true = 0;
            }
        }
        for (l = 0; l < total; l++) {
            thisRace[l]->policy.push_back(default_weights);
            select_node(&node, tronPut[l], thisRace[l], rows, cols, ops);
        }
        if (ops > 0) {
            for (i = 0; i < ops; i++) {
                move_adversary(adversary[i], &node, cols, rows);
            }
        }
        update_map(adversary, tronPut, &node, thisRace,  rows, cols, ops, total);
        for (k = 0; k < total; k++) {
            tmpfileXX << tronPut[k]->ID << "-";
            tmpfileXX << tronPut[k]->damages[tronPut[k]->damages.size()-2] << "-";
            if (tronPut[k]->ImaChurch == 1) {
                tmpfileXX << "true" << "-";
            } else {
                tmpfileXX << "false" << "-";
            }
            tmpfileXX << tronPut[k]->action[tronPut[k]->action.size()-1] << endl;
        }
        for (i = 0; i < total; i++) {
            if (is_terminal(tronPut[i]->posx, cols)) {
                outfile << thisRace[i]->steps + 1;
                Pflag++;
            }
            if (thisRace[i]->steps > cols*2) {
                outfile << thisRace[i]->steps + 1;
                Oflag++;
            }
        }
        if (Pflag > 0) break;
        //If an adversary has won, set the flag and break.
        if (ops > 0) {
            for (i = 0; i < ops; i++) {
                if (is_terminal(adversary[i]->posx, cols)) {
                    outfile << thisRace[0]->steps + 1;
                    Oflag++;
                }
            }
        }
        if (Oflag > 0) break;
    }
    for (i = 0; i < rows; i++) {
        for (j = 0; j < cols; j++) {
            tmpfileXX << node[i][j].tile;
            node[i][j].reward = 0;
            node[i][j].visits = 1;
        }
        tmpfileXX << endl;
    }
    for (k = 0; k < total; k++) {
        tmpfileXX << tronPut[k]->ID << "-";
        tmpfileXX << tronPut[k]->damages[tronPut[k]->damages.size()-1] << "-";
        if (tronPut[k]->ImaChurch == 1) {
            tmpfileXX << "true";
        } else {
            tmpfileXX << "false";
        }
    }
    tmpfileXX << endl;
    tmpfileXX.close();
    //Determine winner and return the reward.
    if (Oflag > 0) {
        for (l = 0; l < total; l++) {
            expected_reward += thisRace[l]->expenses;
            outfile << " " << tronPut[l]->ID << " " << tronPut[l]->name;
        }
    } else {
        expected_reward += thisRace[0]->prize;
        for (l = 0; l < total; l++) {
            expected_reward += thisRace[l]->expenses;
            outfile << " " << tronPut[l]->ID << " " << tronPut[l]->name;
        }
    }
    outfile << endl;
    tmpfileXX.open("TEMP.txt", ios::in);
    while(true) {
        tmpfileXX >> buffer;
        if ( tmpfileXX.eof() ) break;
        outfile << buffer << endl;
    }
    tmpfileXX.close();
    outfile << expected_reward << endl;
    outfile.close();

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
            delete [] adversary[k]->adPol;
        }
//        delete adversary[k];
    }
    for (i = 0; i < total; i++) {
        delete tronPut[i];
    }
    delete [] tronPut;
    delete [] adversary;
    delete [] node;
    delete [] thisRace;

}
