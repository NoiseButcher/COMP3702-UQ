#include "out_level.h"

//Function to run races within the threads. Most of the functionality here is handled in
//race_level.cpp.
void *runRace (void *arg) {

    clock_t t1;
    double duration;
    Track * thisTrack;
    thisTrack = (Track *)arg;
    t1 = clock();
    thisTrack->result = single_race_solver(&thisTrack->name[0], thisTrack->cycles, thisTrack->numCycles);
    duration = (clock() - t1 )/ CLOCKS_PER_SEC;
    cout << thisTrack->name << " : " << duration << " seconds" << endl;
    pthread_exit(NULL);
 }

int main(int argc, char* argv[]) {

    char buffer[1000];
    char subBuffer[1000];
    int capital, i, j, k, tc, tCount, cCount, z, maxCombo, trackNum;
    signed int tRows, tCols;
    Track solveMe;
    clock_t t1;
    double duration;

    //Error check cli here.
    if (argc != 3) {
        cout << "Wrong input moron." << endl;
    }

    //Get cycle data.
    fstream cycFile;
    cycFile.open(argv[1], ios::in);

    //Determine how many cycles are available.
    cycFile >> buffer;
    cCount = atoi(buffer);
    TronCycle trons[cCount];
    TronCycle ** allCombos;

    j = 0;
    //Populate the array of TronCycles.
    while (j < cCount) {
        cycFile >> trons[j].name;
        cycFile >> buffer;
        switch(buffer[0]) {
            case 'F':
                trons[j].MaxSpeed = 3;
                break;
            case 'M':
                trons[j].MaxSpeed = 2;
                break;
            case'S':
                trons[j].MaxSpeed = 1;
                break;
        };
        cycFile >> buffer;
        trons[j].reliability = buffer[0];
        cycFile >> buffer;
        trons[j].durability = buffer[0];
        cycFile >> buffer;
        trons[j].price = atoi(buffer);
        trons[j].ImaChurch = 0;
        if ( cycFile.eof() ) break;
        j++;
    }

    cycFile.close();

    //Get track data.
    fstream metaTrkFile;
    fstream trkFile;
    metaTrkFile.open(argv[2], ios::in);
    metaTrkFile >> buffer;
    trackNum = atoi(buffer);
    //Initialise the threads for race processing.

    string names[trackNum];
    //Initialise the track data structures.
    vector <Track> tracks;

    //Get initial capital.
    metaTrkFile >> buffer;
    capital = atoi(buffer);

    tc = 0;
    j = 0;
    maxCombo = 1;
    //Populate the track list.
    while (true) {
        metaTrkFile >> buffer;
        if (metaTrkFile.eof() ) break;
        names[tc] = buffer;
        while (true) {
            solveMe.name = buffer;
            solveMe.numCycles = 0;
            solveMe.cycleCost = 0;
            solveMe.totes = cCount;
            //Process a track file in a seperate buffer.
            trkFile.open(buffer, ios::in);
            //Run this to get to the costs.
            trkFile >> subBuffer;
            tRows = atoi(subBuffer);
            trkFile >> subBuffer;
            tCols = atoi(subBuffer);
            trkFile >> subBuffer;
            trkFile >> subBuffer;
            solveMe.costIn = atoi(subBuffer);
            trkFile >> subBuffer;
            solveMe.costOut = atoi(subBuffer);

            //Use this loop to figure out how many slots are available
            //for player use, how many obstacles exist and how many
            //distractors exist.
            for (i = 0; i < tRows; i++) {
                trkFile >> subBuffer;
                for (k = 0; k < tCols; k++) {
                    if (subBuffer[k] < 91 && subBuffer[k] > 74) {
                        solveMe.numCycles++;
                        if (solveMe.numCycles > maxCombo) maxCombo = solveMe.numCycles;
                    }
                }
            }
            z++;
            j++;
            trkFile.close();
            tracks.push_back(solveMe);
            if (z >= tracks[j-1].numCycles * cCount) break;
        }
        z = 0;
        tc++;
//        if (metaTrkFile.eof() ) break;
    }

    tc = 0;
    metaTrkFile.close();
    tCount = j;
    int beta = maxCombo*cCount;
    allCombos = new TronCycle*[beta];
    Result results[beta];
    sort_Cycles_X(trons, allCombos, cCount, maxCombo);
    for (i = 0; i < beta; i++) {
        results[i].totalLoss = 0;
        results[i].profitThree = 0;
        results[i].profitTwo = 0;
        results[i].profitOne = 0;
        results[i].Tnames = new string[trackNum];
        results[i].bestResults = new int[trackNum];
        results[i].Raceindex = new int[trackNum];
        for (k = 0; k < trackNum; k++) {
            results[i].Tnames[k] = names[k];
            results[i].bestResults[k] = 0;
            results[i].Raceindex[k] = 0;
        }
    }

    pthread_t raceThreads[tCount];

    //Create threads to run MCTS for the tracks.
    z = 0;
    i = 0;
    for (j = 0; j < tCount; j++) {
        if (i < (cCount*(tracks[j].numCycles - 1))) tracks[j].numCycles--;
        tracks[j].cycles = new TronCycle[tracks[j].numCycles];
        for (k = 0; k < tracks[j].numCycles; k++) {
            tracks[j].cycles[k] = allCombos[i][k];
            tracks[j].cycleCost += allCombos[i][k].price;
//            cout << tracks[j].cycles[k].name << " ";
        }
        /*ENABLE THIS FOR MULTI-THREADED VERSION*/
        tc = pthread_create(&raceThreads[j], NULL, runRace, (void *)&tracks[j]);

        /*ENABLE THIS FOR FOR LOOP VERSION.*/
//        t1 = clock();
//        tracks[j].result = single_race_solver(&tracks[j].name[0], tracks[j].cycles, tracks[j].numCycles);
//        duration = (clock() - t1 )/ CLOCKS_PER_SEC;
//        cout << tracks[j].name << " : " << duration << " seconds" << endl;

//        cout << j << endl;
        if (j < tCount - 1) {
            if ((tracks[j].name != tracks[j+1].name)) {
                i = 0;
            } else {
                i++;
                if (z < i) {
                    results[z].CycleName = new string[tracks[j].numCycles];
                    for (k = 0; k < tracks[j].numCycles; k++) {
                        results[z].CycleName[k] = tracks[j].cycles[k].name;
                        results[z].totalLoss += tracks[j].cycles[k].price;
                    }
                    results[z].sizeOf = tracks[j].numCycles;
//                    cout << results[z].sizeOf << endl;
                    z++;
                }
            }
        } else {
            i++;
            if (z < i) {
                results[z].CycleName = new string[tracks[j].numCycles];
                for (k = 0; k < tracks[j].numCycles; k++) {
                    results[z].CycleName[k] = tracks[j].cycles[k].name;
                    results[z].totalLoss += tracks[j].cycles[k].price;
                }
                results[z].sizeOf = tracks[j].numCycles;
                z++;
            }
        }
        if (i >= beta) i = 0;
    }

    i = 0;
    //Wait for the race threads to terminate before continuing.
    for (j = 0; j < tCount; j++) {
        tc = pthread_join(raceThreads[j], NULL);
//        cout << tracks[j].name << " : " << tracks[j].result << endl;
        if (tracks[j].result > 0) {
            for (k = 0; k < beta; k++) {
                for (i = 0; i < trackNum; i++) {
                    if (in_Race(tracks[j].cycles, results[k].CycleName, tracks[j].numCycles, results[k].sizeOf)
                        && (results[k].Tnames[i] == tracks[j].name)
                        && (results[k].sizeOf == tracks[j].numCycles)) {
                        results[k].bestResults[i] = tracks[j].result;
                        results[k].Raceindex[i] = j;
                    }
                }
            }
        }
    }

    //Sort the results, from best to worst.
    string heapsBeef;
    int hurdyGurdy;
    int bvvt;
    for (j = 0; j < beta; j++) {
        for (k = 0; k < beta; k++) {
            for (i = 1; i < trackNum; i++) {
                if (results[k].bestResults[i] > results[k].bestResults[i - 1]) {
                    hurdyGurdy = results[k].bestResults[i];
                    heapsBeef = results[k].Tnames[i];
                    bvvt = results[k].Raceindex[i];
                    results[k].bestResults[i] = results[k].bestResults[i - 1];
                    results[k].Tnames[i] = results[k].Tnames[i - 1];
                    results[k].Raceindex[i] = results[k].Raceindex[i - 1];
                    results[k].bestResults[i - 1] = hurdyGurdy;
                    results[k].Tnames[i - 1] = heapsBeef;
                    results[k].Raceindex[i - 1] = bvvt;
                }
            }
        }
    }

    for (k = 0; k < beta; k++) {
        for (i = 0; i < 3; i++) {
            results[k].profitThree += results[k].bestResults[i];
        }
        results[k].profitThree -= results[k].totalLoss;
        for (i = 0; i < 2; i++) {
            results[k].profitTwo += results[k].bestResults[i];
        }
        results[k].profitTwo -= results[k].totalLoss;
        for (i = 0; i < 1; i++) {
            results[k].profitOne += results[k].bestResults[i];
        }
        results[k].profitOne -= results[k].totalLoss;
    }

    OutPut single;
    single.profit = 0;
    single.track = new string[1];
    single.indexes = new int[1];
    for (i = 0; i < maxCombo; i++) {
        single.display.push_back("");
    }

    OutPut d_ouble;
    d_ouble.profit = 0;
    d_ouble.track = new string[2];
    d_ouble.indexes = new int[2];

    OutPut triple;
    triple.profit = 0;
    triple.track = new string[3];
    triple.indexes = new int[3];

    int useFlag = 0;

   //Best single race.
    for (k = 0; k < beta; k++) {
        if (results[k].profitOne > single.profit
            && capital > (tracks[results[k].Raceindex[0]].costIn +
               results[k].totalLoss)) {
            single.profit = results[k].profitOne;
            single.indexes[0] = results[k].Raceindex[0];
            single.track[0] = results[k].Tnames[0];
            single.numCycles = results[k].sizeOf;
            for (z = 0; z < results[k].sizeOf; z++) {
                if (z >= single.display.size()) single.display.push_back("");
                single.display[z] = (results[k].CycleName[z]);
            }
        }
    }

    //Two track solver.
    for (k = 0; k < beta; k++) {
        if (results[k].profitTwo > d_ouble.profit
            && capital > (tracks[results[k].Raceindex[0]].costIn +
               tracks[results[k].Raceindex[1]].costIn +
               results[k].totalLoss)) {
            d_ouble.profit = results[k].profitTwo;
            d_ouble.indexes[0] = results[k].Raceindex[0];
            d_ouble.indexes[1] = results[k].Raceindex[1];
            d_ouble.track[0] = results[k].Tnames[0];
            d_ouble.track[1] = results[k].Tnames[1];
            d_ouble.numCycles = results[k].sizeOf;
            for (z = 0; z < results[k].sizeOf; z++) {
                if (z >= d_ouble.display.size()) d_ouble.display.push_back("");
                d_ouble.display[z] = (results[k].CycleName[z]);
            }
        }
        for (j = 0; j < beta; j++) {
            if (results[k].profitOne + results[j].profitOne > d_ouble.profit
                && (k!=j)
                && (results[k].Tnames[0] != results[j].Tnames[0])
                && capital < (tracks[results[k].Raceindex[0]].costIn +
               tracks[results[j].Raceindex[0]].costIn +
               results[k].totalLoss +
               results[j].totalLoss)) {
                d_ouble.profit = results[k].profitOne + results[j].profitOne;
                d_ouble.indexes[0] = results[k].Raceindex[0];
                d_ouble.indexes[1] = results[j].Raceindex[0];
                d_ouble.track[0] = results[k].Tnames[0];
                d_ouble.track[1] = results[j].Tnames[0];
                d_ouble.numCycles = results[k].sizeOf + results[j].sizeOf;
                for (z = 0; z < results[k].sizeOf; z++) {
                    if (z >= d_ouble.display.size()) d_ouble.display.push_back("");
                    d_ouble.display[z] = (results[k].CycleName[z]);
                }
                useFlag = 0;
                for (z = results[k].sizeOf; z < results[k].sizeOf + results[j].sizeOf; z++) {
                    for (tc = 0; tc < d_ouble.display.size(); tc++) {
                        if (d_ouble.display[tc] == results[j].CycleName[z - results[k].sizeOf]) {
                            useFlag++;
                            d_ouble.numCycles--;
                        }
                    }
                    if (useFlag == 0) {
                        if (z >= d_ouble.display.size()) d_ouble.display.push_back("");
                        d_ouble.display[z] = (results[j].CycleName[z - results[k].sizeOf]);
                    }
                }
                useFlag = 0;
            }
        }
    }

    //Three track solver.
    for (k = 0; k < beta; k++) {
        if (results[k].profitThree > triple.profit
         && capital > (tracks[results[k].Raceindex[0]].costIn +
               tracks[results[k].Raceindex[1]].costIn +
               tracks[results[k].Raceindex[2]].costIn +
               results[k].totalLoss)) {
            triple.profit = results[k].profitThree;
            triple.indexes[0] = results[k].Raceindex[0];
            triple.indexes[1] = results[k].Raceindex[1];
            triple.indexes[2] = results[k].Raceindex[2];
            triple.track[0] = results[k].Tnames[0];
            triple.track[1] = results[k].Tnames[1];
            triple.track[2] = results[k].Tnames[2];
            triple.numCycles = results[k].sizeOf;
            for (z = 0; z < results[k].sizeOf; z++) {
                if (z >= triple.display.size()) triple.display.push_back("");
                triple.display[z] = results[k].CycleName[z];
            }
        }
        for (j = 0; j < beta; j++) {
            if (results[k].profitOne + results[j].profitTwo > triple.profit
                && (k!=j)
                && (results[k].Tnames[0] != results[j].Tnames[0])
                && (results[k].Tnames[0] != results[j].Tnames[1])
                && capital > (tracks[results[k].Raceindex[0]].costIn +
               tracks[results[j].Raceindex[0]].costIn +
               tracks[results[j].Raceindex[1]].costIn +
               results[k].totalLoss  +
               results[j].totalLoss)) {
                triple.profit = results[k].profitOne + results[j].profitTwo;
                triple.indexes[0] = results[k].Raceindex[0];
                triple.indexes[1] = results[j].Raceindex[0];
                triple.indexes[2] = results[j].Raceindex[1];
                triple.track[0] = results[k].Tnames[0];
                triple.track[1] = results[j].Tnames[0];
                triple.track[2] = results[j].Tnames[1];
                triple.numCycles = results[k].sizeOf + results[j].sizeOf;
                for (z = 0; z < results[k].sizeOf; z++) {
                    if (z >= triple.display.size()) triple.display.push_back("");
                    triple.display[z] = results[k].CycleName[z];
                }

                useFlag = 0;
                for (z = results[k].sizeOf; z < results[k].sizeOf + results[j].sizeOf; z++) {
                    for (tc = 0; tc < triple.display.size(); tc++) {
                        if (triple.display[tc] == results[j].CycleName[z - results[k].sizeOf]) {
                            useFlag++;
                            triple.numCycles--;
                        }
                    }
                    if (useFlag == 0) {
                        if (z >= triple.display.size()) triple.display.push_back("");
                        triple.display[z] = results[j].CycleName[z - results[k].sizeOf];
                    }
                }
                useFlag = 0;
            }
            for (i = 0; i < beta; i++) {
                if (results[k].profitOne + results[j].profitOne + results[i].profitOne > triple.profit
                    && (k!=j)
                    && (i!=j)
                    && (k!=i)
                    && (results[k].Tnames[0] != results[j].Tnames[0])
                    && (results[k].Tnames[0] != results[i].Tnames[0])
                    && (results[j].Tnames[0] != results[i].Tnames[0])
                    && capital > (tracks[results[k].Raceindex[0]].costIn +
                    tracks[results[j].Raceindex[0]].costIn +
                    tracks[results[i].Raceindex[0]].costIn +
                    results[k].totalLoss  +
                    results[j].totalLoss  +
                    results[i].totalLoss)) {
                            triple.profit = results[k].profitOne + results[j].profitOne + results[i].profitOne;
                            triple.indexes[0] = results[k].Raceindex[0];
                            triple.indexes[1] = results[j].Raceindex[0];
                            triple.indexes[2] = results[i].Raceindex[0];;
                            triple.track[0] = results[k].Tnames[0];
                            triple.track[1] = results[j].Tnames[0];
                            triple.track[2] = results[i].Tnames[0];
                            triple.numCycles = results[k].sizeOf + results[j].sizeOf + results[i].sizeOf;
                            for (z = 0; z < results[k].sizeOf; z++) {
                                if (z >= triple.display.size()) triple.display.push_back("");
                                triple.display[z] = results[k].CycleName[z];
                            }
                            useFlag = 0;
                            for (z = results[k].sizeOf; z < results[k].sizeOf + results[j].sizeOf; z++) {
                                for (tc = 0; tc < triple.display.size(); tc++) {
                                    if (triple.display[tc] == results[j].CycleName[z - results[k].sizeOf]) {
                                        useFlag++;
                                        triple.numCycles--;
                                    }
                                }
                                if (useFlag == 0) {
                                    if (z >= triple.display.size()) triple.display.push_back("");
                                    triple.display[z] = results[j].CycleName[z - results[k].sizeOf];
                                }
                            }
                            useFlag = 0;
                            for (z = results[k].sizeOf + results[j].sizeOf;
                                z < results[k].sizeOf + results[j].sizeOf + results[i].sizeOf;
                                z++) {
                                for (tc = 0; tc < triple.display.size(); tc++) {
                                    if (triple.display[tc] == results[i].CycleName[z - (results[k].sizeOf + results[j].sizeOf)]) {
                                        useFlag++;
                                        triple.numCycles--;
                                    }
                                }
                                if (useFlag == 0) {
                                    if (z >= triple.display.size()) triple.display.push_back("");
                                    triple.display[z] = results[i].CycleName[z - (results[k].sizeOf + results[j].sizeOf)];
                                }
                            }
                            useFlag = 0;
                }
            }
        }
    }

    int bestBet = 0;
//    cout << "I get up to file writing" << endl;
    string outName = "myresult.txt";
    ofstream outFile;
    outFile.open(&outName[0], ios::out | ios::trunc);
    outFile << argv[1] << " " << argv[2] << endl;

    if ((single.profit >= d_ouble.profit) && (single.profit >= triple.profit)) {
        cout << "single race " << single.profit << endl;
        for (k = 0; k < single.numCycles; k++) {
            outFile << single.display[k] << endl;
        }
        outFile << tracks[single.indexes[0]].name << " " << tracks[single.indexes[0]].numCycles;
        outFile << endl;
        outFile.close();
        single_race_output(&tracks[single.indexes[0]].name[0], tracks[single.indexes[0]].cycles,
                            tracks[single.indexes[0]].numCycles, &outName[0]);
        outFile.open(&outName[0], ios::out | ios::app);
        outFile << single.profit << endl;
        outFile.close();
    } else if ((triple.profit <= d_ouble.profit) && (single.profit <= d_ouble.profit)) {
        cout << "two races " << d_ouble.profit << endl;
        for (k = 0; k < d_ouble.numCycles; k++) {
            outFile << d_ouble.display[k] << endl;
        }
        outFile << tracks[d_ouble.indexes[0]].name << " " << tracks[d_ouble.indexes[0]].numCycles << " ";
        outFile << tracks[d_ouble.indexes[1]].name << " " << tracks[d_ouble.indexes[1]].numCycles;
        outFile << endl;
        outFile.close();
        single_race_output(&tracks[d_ouble.indexes[0]].name[0], tracks[d_ouble.indexes[0]].cycles,
                                    tracks[d_ouble.indexes[0]].numCycles, &outName[0]);
        single_race_output(&tracks[d_ouble.indexes[1]].name[0], tracks[d_ouble.indexes[1]].cycles,
                                tracks[d_ouble.indexes[1]].numCycles, &outName[0]);
        outFile.open(&outName[0], ios::out | ios::app);
        outFile << d_ouble.profit << endl;
        outFile.close();
    } else if ((triple.profit >= d_ouble.profit) && (single.profit <= triple.profit)) {
        cout << "three races " << triple.profit << endl;
        for (k = 0; k < triple.numCycles; k++) {
            outFile << triple.display[k] << endl;
        }
        outFile << tracks[triple.indexes[0]].name << " " << tracks[triple.indexes[0]].numCycles << " ";
        outFile << tracks[triple.indexes[1]].name << " " << tracks[triple.indexes[1]].numCycles << " ";
        outFile << tracks[triple.indexes[2]].name << " " << tracks[triple.indexes[2]].numCycles;
        outFile << endl;
        outFile.close();
        single_race_output(&tracks[triple.indexes[0]].name[0], tracks[triple.indexes[0]].cycles,
                            tracks[triple.indexes[0]].numCycles, &outName[0]);
        single_race_output(&tracks[triple.indexes[1]].name[0], tracks[triple.indexes[1]].cycles,
                            tracks[triple.indexes[1]].numCycles, &outName[0]);
        single_race_output(&tracks[triple.indexes[2]].name[0], tracks[triple.indexes[2]].cycles,
                            tracks[triple.indexes[2]].numCycles, &outName[0]);
        outFile.open(&outName[0], ios::out | ios::app);
        outFile << triple.profit << endl;
        outFile.close();
    }

//    Memory cleanup.
    delete [] single.track;
    delete [] single.indexes;
    delete [] d_ouble.track;
    delete [] d_ouble.indexes;
    delete [] triple.track;
    delete [] triple.indexes;
//
//
    for (i=0; i < beta; i++) {
//        delete [] results[i].bestResults;
//        delete [] results[i].CycleName;
//        delete [] results[i].Tnames;
//        delete [] results[i].Raceindex;
        delete [] allCombos[i];
    }
    delete [] allCombos;

    for (j = 0; j < tCount; j++) {
        delete [] tracks[j].cycles;
    }

    return 0;
}
