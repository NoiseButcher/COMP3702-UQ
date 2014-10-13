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
        int has_attr (char a) {
            if (a == durability) { return 1;
            } else if (a == reliability) { return 1;
            } else if (a - 48 <=  MaxSpeed) { return 1;
            } else return 0;
        }

};

class OutPut {
    public:
        string * track;
        int * indexes;
        int profit;
        int numCycles;
};

class Result {
    public:
        string * CycleName;
        string * Tnames;
        int * bestResults;
        int sizeOf;
        int totalLoss;
        int profitThree;
        int profitTwo;
        int profitOne;
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
        int result;
};

class Distractor {
    public:
        float p;
        char ID;
        int isActive (void) {
            srand(time(NULL));
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
        float value (void) { return (reward/visits); }
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
        int best_child (int a) {
            int y;
            int x = 0;
            for (y = 1; y < 7; y++) {
                if (policy[a][y] > policy[a][x]) x = y;
            }
            return x;
        }
};
