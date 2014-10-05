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
        int posx;
        int posy;
        int MaxSpeed;
};

class Track {
    public:
        string name;
        int costIn;
        int costOut;
        int numCycles;
        TronCycle * cycles;
};

class Distractor {
    public:
        float p;
        char ID;
        int isActive (void) {
            srand(time(NULL));
            int x = rand() % 10;
            if (x <= (p*10)) {return 1; }
            else {return 0; }
        }
};

class Node {
    public:
        int row;
        int col;
        int visits;
        float reward;
        int dist_true;
        int obs_true;
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
        int posx;
        int posy;
        vector < vector < vector <float> > > adPol;
};

class Policy {
    public:
        char *p_actions;
        int actions_taken;
};

class GameTree {
    public:
        vector <Node> playable;
        int steps;
        int raceLen;
        int prize;
        signed int expenses;
};
