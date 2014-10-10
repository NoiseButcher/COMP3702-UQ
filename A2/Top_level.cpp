#include "race_level.h"

//Function to run races within the threads. Most of the functionality here is handled in
//race_level.cpp.
void *runRace (void *arg) {

    int i;
    Track *thisTrack;
    int bestBet = 0;
    thisTrack = (Track *)arg;
    signed int trackVal[thisTrack->numCombo];

    for (i = 0; i < thisTrack->numCombo; i++) {
        trackVal[i] = single_race_solver(&thisTrack->name[0], &thisTrack->cycles[i], thisTrack->numCycles);
    }
    for (i = 0; i < thisTrack->numCombo; i++) {
        if (trackVal[i] > trackVal[bestBet]) { bestBet = i;
        } else if (trackVal[i] == trackVal[bestBet]) {
            if (thisTrack->cycles[i]->price < thisTrack->cycles[bestBet]->price) bestBet = i;
        }
    }
    cout << "For " << thisTrack->name << " the bests odds are with "
        << thisTrack->cycles[bestBet][0].name << " for " << trackVal[bestBet] << endl;

    pthread_exit(NULL);
 }

int main(int argc, char* argv[]) {

    char buffer[1000];
    char subBuffer[1000];
    int capital, i, j, k, tc, tCount, cCount;
    signed int tRows, tCols;

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
    pthread_t raceThreads[tCount];

    //Initialise the track data structures.
    Track tracks[tCount];

    //Get initial capital.
    metaTrkFile >> buffer;
    capital = atoi(buffer);

    j = 0;
    //Populate the track list.
    while (true) {

        metaTrkFile >> buffer;
        tracks[j].name = buffer;
        tracks[j].oCount = 0;
        tracks[j].dCount = 0;
        tracks[j].numCycles = 0;
        tracks[j].numCombo = cCount;
        //Process a track file in a seperate buffer.
        trkFile.open(buffer, ios::in);
        //Run this to get to the costs.
        trkFile >> subBuffer;
        tRows = atoi(subBuffer);
        trkFile >> subBuffer;
        tCols = atoi(subBuffer);
        trkFile >> subBuffer;
        trkFile >> subBuffer;
        tracks[j].costIn = atoi(subBuffer);
        trkFile >> subBuffer;
        tracks[j].costOut = atoi(subBuffer);

        //Use this loop to figure out how many slots are available
        //for player use, how many obstacles exist and how many
        //distractors exist.
        for (i = 0; i < tRows; i++) {
            trkFile >> subBuffer;
            for (k = 0; k < tCols; k++) {
                if (subBuffer[k] == '1') { tracks[j].oCount++;
                break;
                }
            }
            for (k = 0; k < tCols; k++) {
                if (subBuffer[k] > 96) { tracks[j].dCount++;
                break;
                }
            }
            for (k = 0; k < tCols; k++) {
                if (subBuffer[k] < 91 && subBuffer[k] > 74) { tracks[j].numCycles++;
                }
            }
        }


        if ((tRows - tracks[j].oCount < 2) && (tRows - tracks[j].dCount < 2)) {
            tracks[j].pQueue = "WR3";
        } else if ((tRows - tracks[j].dCount < 2) && (tracks[j].oCount > 0)) {
            tracks[j].pQueue = "RW3";
        } else if ((tracks[j].oCount > tracks[j].dCount) && (tracks[j].oCount > 0))  {
            tracks[j].pQueue = "3WR";
        } else {
            tracks[j].pQueue = "3RW";
        }

        tracks[j].cycles = new TronCycle*[cCount];
        sort_Cycles_X(trons, tracks[j].cycles, cCount, tracks[j].numCycles);
//        tracks[j].cycles = trons;
        trkFile.close();
        if (metaTrkFile.eof() ) break;
        j++;
    }

    metaTrkFile.close();

    //Create threads to run MCTS for the tracks.
    for (j = 0; j < tCount; j++) {
        tc = pthread_create(&raceThreads[j], NULL, runRace, (void *)&tracks[j]);
    }
    //Wait for the race threads to terminate before continuing.
    for (j = 0; j < tCount; j++) {
        tc = pthread_join(raceThreads[j], NULL);
    }

    return 0;
}
