#include <iostream>
#include <fstream>
#include <vector>
#include <string.h>
using namespace std;

#ifndef CSV_READER_HPP
#define CSV_READER_HPP

const vector<string> explode(const string& s, const char& c) {
	string buff{""};
	vector<string> v;
	for(auto n:s) {
		if(n != c) buff+=n; else
		if(n == c && buff != "") { v.push_back(buff); buff = ""; }
	}
	if(buff != "") v.push_back(buff);
	return v;
}

vector<vector<string>> read(string file_path) {
    ifstream fin;
    string line;

    fin.open(file_path);
	vector<vector<string>> out;

    while(!fin.eof()){
		vector<string> aux;
		fin>>line;
		vector<string> splitedLine = explode(line, ',');
		for(string elem : splitedLine){
			aux.push_back(elem);
		}
		out.push_back(aux);
	}
	return out;
}

void print_csv(vector<vector<string>> csv){
	for(vector<string> line: csv){
		for (int i = 0; i < line.size(); i++){
			if(i == line.size()-1){
				cout << line[i] << endl;
			} else {
				cout << line[i] << ", ";
			}
		}
	}
}

#endif