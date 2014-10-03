#include <iostream>
#include <string>
#include <cstdlib>
#include <pthread.h>
#include <fstream>
#include <unistd.h>
#include <vector>

using namespace std;

class TronCycle {
    public:
        string name;
        char ID;
        char MaxSpeed;
        char durability;
        char reliability;
        string price;
        int posx;
        int posy;
};

class Track {
    public:
        string name;
        int costIn;
        int costOut;
        int numCycles;
        TronCycle * cycles;
};

class Node {
    public:
        int row;
        int col;
        float visits;
        float reward;
        float distractor;
        char tile;
        int isDist (void) {
            if (tile > 96) { return 1; }
            else { return 0; }
        }
        int isOpp (void) {
            if (tile > 64 && tile < 75) { return 1;}
            else { return 0;}
        }
        int isObs (void) {
            if (tile == '1') { return 1;}
            else { return 0;}
        }
        float value (void) { return reward/visits; }
};

class Distractor {
    public:
        float p;
        int posx;
        int posy;
        char ID;
        int isActive (void) {
            int x = rand() % 10;
            if (x <= p) {return 1; }
            else {return 0; }
        }
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
};
