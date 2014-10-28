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
        int numParents;
        vector <string> parents;
        float * cpv;
        string * cpt;
};

int main (int argc, char * argv[]) {

    int i, j, k, nc, data, cc;
    char buffer[1000];
    string buffer2;
    string subby;
    fstream infile, outfile;
    float z;

    infile.open(argv[1], ios::in);
    infile >> buffer;
    nc = atoi(buffer);
    cout << nc << endl;
    infile >> buffer;
    data = atoi(buffer);
    cout << data << endl;

    Node node[nc];

    //Redundant read because getline and >> do read into seperate buffers.
    getline(infile, buffer2);

    //Process parent/child combinations as data structures.
    for (i = 0; i < nc; i++) {
        getline(infile, buffer2);
        istringstream iss(buffer2);
        iss >> node[i].child;
        k = 0;
        iss >> subby;
        cout << node[i].child << " ";
        while (iss != NULL) {
            iss >> subby;
            node[i].parents.push_back(subby);
            cout << node[i].parents[k] << " ";
            k++;
            if (iss == NULL) {
                node[i].numParents = k;
                break;
            }
        }
        z = pow(2.00, (double)k);
        cc = (int)z;
        node[i].cpv = new float[cc];
        node[i].cpt = new string[cc];

        cout << cc << " ";


        for (j = 0; j < cc; j++) {
            if (cc > 1) {
                node[i].cpt[j].append("P(");
                node[i].cpt[j].append(node[i].child);
                node[i].cpt[j].append("|");
                for (k = 0; k < node[i].numParents; k++) {
                    if (j & (node[i].numParents - k)) node[i].cpt[j].append("~");
                    node[i].cpt[j].append(node[i].parents[k]);
                    if (k < node[i].numParents - 1) node[i].cpt[j].append(",");
                }
                node[i].cpt[j].append(")");
                node[i].cpv[j] = 0.00;
                cout << node[i].cpt[j] << " ";
            } else {
                node[i].cpt[j].append("P(");
                node[i].cpt[j].append(node[i].child);
                node[i].cpt[j].append(")");
                node[i].cpv[j] = 0.00;
                cout << node[i].cpt[j] << " ";
            }
        }
        cout << endl;
    }



    return 0;
}
