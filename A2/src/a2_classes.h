#include <iostream>
#include <string>
#include <cstdlib>
#include <pthread.h>
#include <fstream>
#include <unistd.h>
#include <vector>
#include <ctime>

using namespace std;

class TronCycle {
    public:
        string name;
        int price;
        char ID;
        char durability;
        char reliability;
        signed int posx;
        signed int posy;
        int ImaChurch;
        int MaxSpeed;
        vector <float> damages;
        vector <string> action;

};

class OutPut {
    public:
        string * track;
        int * indexes;
        signed int profit;
        int numCycles;
        vector <string> display;
};

class Result {
    public:
        string * CycleName;
        string * Tnames;
        signed int * bestResults;
        int sizeOf;
        signed int totalLoss;
        signed int profitThree;
        signed int profitTwo;
        signed int profitOne;
        int * Raceindex;
};

class Track {
    public:
        string name;
        int costIn;
        int costOut;
        int cycleCost;
        TronCycle * cycles;
        int numCycles;
        int totes;
        signed int result;
};

class Distractor {
    public:
        float p;
        char ID;
        int isActive (void) {
            srand(time(NULL)*p*0.928);
            int x = rand() % 100;
            if (x < (p*100)) {return 1; }
            else {return 0; }
        }
};

class Node {
    public:
        signed int row;
        signed int col;
        int visits;
        float reward;
        int dist_true;
        int obs_true;
        int block_true;
        Distractor distractor;
        string tile;
        int isDist (void) {
            if (dist_true == 1 && (tile.find(distractor.ID) != string::npos)) { return 1; }
            else { return 0; }
        }
        int isOpp (void) {
            if (tile[0] > 64 && tile[0] < 75) { return 1;}
            else { return 0;}
        }
        float value (void) { return (reward/visits)*distractor.p; }
};

class Adversary {
    public:
        char ID;
        signed int posx;
        signed int posy;
        float *** adPol;
};

class GameTree {
    public:
        vector <float*> policy;
        int steps;
        int raceLen;
        int prize;
        signed int expenses;
        int best_child (int a, int maxDex) {
            int y;
            int x = 0;
            for (y = 1; y < maxDex; y++) {
                if (policy[a][y] > policy[a][x]) x = y;
            }
            return x;
        }
};
