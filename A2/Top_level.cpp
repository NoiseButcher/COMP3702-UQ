#include "race_level.h"

//Function to run races within the threads. Most of the functionality here is handled in
//race_level.cpp.
void *runRace (void *arg) {
    Track * thisTrack;
    thisTrack = (Track *)arg;
    thisTrack->result = single_race_solver(&thisTrack->name[0], &thisTrack->cycles, thisTrack->numCycles);
    pthread_exit(NULL);
 }

int main(int argc, char* argv[]) {

    char buffer[1000];
    char subBuffer[1000];
    int capital, i, j, k, tc, tCount, cCount, z, maxCombo;
    signed int tRows, tCols;
    Track solveMe;

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
    tCount = atoi(buffer);

    //Initialise the threads for race processing.

    //Initialise the track data structures.
    vector <Track> tracks;

    //Get initial capital.
    metaTrkFile >> buffer;
    capital = atoi(buffer);

    j = 0;
    maxCombo = 1;
    //Populate the track list.
    while (true) {
        metaTrkFile >> buffer;
        if (metaTrkFile.eof() ) break;
        while (true) {
            solveMe.name = buffer;
            solveMe.numCycles = 0;
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

    }
    metaTrkFile.close();
    tCount = j;
    allCombos = new TronCycle*[maxCombo*cCount];
    sort_Cycles_X(trons, allCombos, cCount, maxCombo);


    pthread_t raceThreads[tCount];

    //Create threads to run MCTS for the tracks.
    i = 0;
    for (j = 0; j < tCount; j++) {
        if (i < (cCount*(tracks[j].numCycles - 1))) tracks[j].numCycles--;
        tracks[j].cycles = new TronCycle[tracks[j].numCycles];
        for (k = 0; k < tracks[j].numCycles; k++) {
            tracks[j].cycles[k] = allCombos[i][k];
        }
        tc = pthread_create(&raceThreads[j], NULL, runRace, (void *)&tracks[j]);


        if (j < tCount - 1) {
            if ((tracks[j].name != tracks[j+1].name)) {
                i = 0;
            } else {
                i++;
            }
        } else {
            i++;
        }
        if (i >= 2*maxCombo) i = 0;
    }
    //Wait for the race threads to terminate before continuing.
    for (j = 0; j < tCount; j++) {
        tc = pthread_join(raceThreads[j], NULL);
        cout << tracks[j].result << endl;
    }



    //Memory cleanup.
    for (j = 0; j < tCount; j++) {
        delete [] tracks[j].cycles;
    }

    delete [] allCombos;
    return 0;
}
