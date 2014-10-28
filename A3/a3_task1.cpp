#include <iostream>
#include <string>
#include <sstream>
#include <cstdlib>
#include <pthread.h>
#include <fstream>
#include <math.h>
#include <unistd.h>
#include <vector>
#include <ctime>

using namespace std;

class Node {
    public:
        string child;
        int cAddr;
        int numParents;
        int tableSize;
        vector <string> parents;
        double * cpd;
        double * cpn;
        double * cpv;
        int * mask;
        int * addr;
        string * cpt;
};

void processTD (string buffer, string * bib, int len, Node * nodes) {
    int i, j, nodeVal;
    string submarine;

    nodeVal = 0;
    istringstream iss(buffer);
    for (i = 0; i < len; i++) {
        iss >> submarine;
        if (submarine == "0") {
            cout << bib[i] << " : F";
            nodeVal |= 0x0 << len - i - 1;
        } else if (submarine == "1") {
            cout << bib[i] << " : T";
            nodeVal |= 0x1 << len - i - 1;
        } else {
            cout << bib[i] << " : ?";
        }
        if (i < len - 1) cout << ", ";
        submarine.clear();
    }
    cout << " " << nodeVal << endl;

    for (i = 0; i < len; i++) {
        if (nodes[i].cAddr & nodeVal) {
            for (j = 0; j < nodes[i].tableSize; j++) {
                nodes[i].cpd[j]++;
                if ((nodeVal&nodes[i].mask[j]) == nodes[i].addr[j]) {
                    nodes[i].cpn[j]++;
                }
                nodes[i].cpv[j] = nodes[i].cpn[j]/nodes[i].cpd[j];
            }

        }
    }

}

int main (int argc, char * argv[]) {

    int i, j, k, l, nc, data, cc;
    char buffer[1000];
    string buffer2;
    string subby;
    fstream infile, outfile;
    float z;

    infile.open(argv[1], ios::in);
    infile >> buffer;
    nc = atoi(buffer);
    infile >> buffer;
    data = atoi(buffer);

    Node node[nc];
    string indexes[nc];

    //Redundant read because getline and >> do read into seperate buffers.
    getline(infile, buffer2);

    //Process parent/child combinations as data structures.
    for (i = 0; i < nc; i++) {
        getline(infile, buffer2);
        istringstream iss(buffer2);
        iss >> node[i].child;
        indexes[i] = node[i].child;
        k = 0;
        iss >> subby;

        while (iss != NULL) {
            node[i].parents.push_back(subby);
            k++;
            iss >> subby;
            if (iss == NULL) {
                node[i].numParents = k;
                break;
            }
        }
        z = pow(2.00, (double)k);
        cc = (int)z;
        node[i].cpv = new double[cc];
        node[i].cpd = new double[cc];
        node[i].cpn = new double[cc];
        node[i].cpt = new string[cc];
        node[i].mask = new int[cc];
        node[i].addr = new int[cc];
        node[i].tableSize = cc;
    }

    for (i = 0; i < nc; i++) {
        //Format the probability table for display.
        for (j = 0; j < node[i].tableSize; j++) {

            for (k = 0; k < nc; k++) {
                if (indexes[k] == node[i].child) {
                    node[i].mask[j] = 0x1 << (nc - (k + 1));
                    node[i].addr[j] = 0x1 << (nc - (k + 1));
                    node[i].cAddr = 0x1 << (nc - (k + 1));
                }
            }

            if (node[i].tableSize > 1) {
                node[i].cpt[j].append("P(");
                node[i].cpt[j].append(node[i].child);
                node[i].cpt[j].append("|");
                for (k = 0; k < node[i].numParents; k++) {
                    if (j & (node[i].numParents - k)) {
                        node[i].cpt[j].append("~");
                        for (l = 0; l < nc; l++) {
                            if (indexes[l] == node[i].parents[k]){
                                node[i].mask[j] |= (0x1 << (nc - (l + 1)));
                                node[i].addr[j] |= (0x0 << (nc - (l + 1)));
                            }
                        }
                    } else {
                         for (l = 0; l < nc; l++) {
                            if (indexes[l] == node[i].parents[k]){
                                node[i].mask[j] |= (0x1 << (nc - (l + 1)));
                                node[i].addr[j] |= (0x1 << (nc - (l + 1)));
                            }
                        }
                    }
                    node[i].cpt[j].append(node[i].parents[k]);
                    if (k < node[i].numParents - 1) node[i].cpt[j].append(",");
                }
                node[i].cpt[j].append(")");
                node[i].cpn[j] = 1.00;
                node[i].cpd[j] = 1.00;
                node[i].cpv[j] = node[i].cpn[j]/node[i].cpd[j];
                cout << node[i].cpt[j] << " = " << node[i].addr[j] << " " << node[i].mask[j] << " ";
            } else {
                node[i].cpt[j].append("P(");
                node[i].cpt[j].append(node[i].child);
                node[i].cpt[j].append(")");
                node[i].cpn[j] = 1.00;
                node[i].cpd[j] = 1.00;
                node[i].cpv[j] = node[i].cpn[j]/node[i].cpd[j];
                cout << node[i].cpt[j] << " = " << node[i].addr[j] << " " << node[i].mask[j] << " ";
            }
        }
        cout << endl;
    }

    for (i = 0; i < data; i++) {
        getline(infile, buffer2);
        processTD(buffer2, indexes, nc, node);
    }

    return 0;
}
