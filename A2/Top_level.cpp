#include "race_level.h"

//Function to run races within the threads. Most of the functionality here is handled in
//race_level.cpp.
void *runRace (void *arg) {

    int trackVal;
    Track *thisTrack;
    thisTrack = (Track *)arg;
    cout << "I am a thread, my target file is " << thisTrack->name << endl;
    trackVal = single_race_solver(&thisTrack->name[0], &thisTrack->cycles, thisTrack->numCycles);
    cout << "Solved: Expected value " << trackVal << endl;
    pthread_exit(NULL);
 }

int main(int argc, char* argv[]) {

    char buffer[1000];
    char subBuffer[1000];
    int j = 0;
    int capital, k, tc, tCount, cCount;

    //Error check cli here.
    if (argc != 3) {
        //Throw error.
    }

    //Get cycle data.
    fstream cycFile;
    cycFile.open(argv[1], ios::in);

    //Determine how many cycles are available.
    cycFile >> buffer;
    cCount = atoi(buffer);
    TronCycle trons[cCount];

    //Populate the array of TronCycles.
    while (j < cCount) {
        cycFile >> trons[j].name;
        cycFile >> buffer;
        trons[j].MaxSpeed = buffer[0];
        cycFile >> buffer;
        trons[j].reliability = buffer[0];
        cycFile >> buffer;
        trons[j].durability = buffer[0];
        cycFile >> buffer;
        trons[j].price = atoi(buffer);
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
        //Process a track file in a seperate buffer.
        trkFile.open(buffer, ios::in);
        //Run this to get to the costs.
        for (k = 0; k < 4; k ++) { trkFile >> subBuffer;}
        tracks[j].costIn = atoi(subBuffer);
        trkFile >> subBuffer;
        tracks[j].costOut = atoi(subBuffer);
        tracks[j].cycles = trons;
        tracks[j].numCycles = cCount;
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
