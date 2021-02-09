
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <iostream>
#include <unistd.h>
#include <vector>
#include <iomanip>
#include <fstream>
#include <string.h>
#include <string>
#include <tuple>
#include <math.h>
#include <locale.h>
#include <algorithm>
#include <sstream>

#include "./csv_reader.hpp"

using namespace std;

// Aux
string float_to_string(float n){
    ostringstream ss;
    ss << n;
    return (ss.str());
}

void printEspacos(int qtd){
  for(int i = 0; i < qtd; i++){
    cout << ' ';
  }
}

int getBiggestSizeM(vector<vector<float>> matriz){
    int size = -1;
    for(int i = 0; i < matriz.size(); i++){
        for (int j = 0; j < matriz[i].size(); j++){
            int aux = float_to_string(matriz[i][j]).length(); 
            if(aux > size){
                size = aux;
            }
        }
    } return size;
}

void printMatriz(vector<vector<float>> matriz){
    int bigSize = getBiggestSizeM(matriz);
    cout << "[";
    for (int i = 0; i < matriz.size(); i++){
        if(i == 0){
            cout << " [";
        } else {
            cout << "  [";
        }
        for (int j = 0; j < matriz[i].size(); j++){
            if(j == matriz[i].size()-1){
                printEspacos(bigSize - float_to_string(matriz[i][j]).length());
                cout << matriz[i][j];
            } else {
                printEspacos(bigSize - float_to_string(matriz[i][j]).length());
                cout << matriz[i][j] << " ";
            }
        }
        if(i == matriz.size()-1){
            cout << "]";
        } else {
            cout << "]" << endl;
        }
    } cout << " ]" << endl;
}

void printVector(vector<float> v, bool end_l){
    cout << "[";
    for (int i = 0; i < v.size(); i++){
        if(i == v.size()-1){
            cout << v[i];
        } else {
            cout << v[i] << " ";
        }
    } 
    if(end_l){
        cout << "]" << endl;
    } else {
        cout << "] ";
    }    
}

vector<float> matrixToVector(vector<vector<float>> m) {
    vector<float> out;
    for (int i = 0; i < m.size(); i++){
        for (int j = 0; j < m[i].size(); j++){
            out.push_back(m[i][j]);
        }
    } return out;
}

vector<float> vectorZeros(int qtd){
    vector<float> out;
    for (int i = 0; i < qtd; i++){
        out.push_back(0);
    } return out;
}

float sumVector(vector<float> vect) {
    float out = 0;
    for (int i = 0; i < vect.size(); i++){
        out += vect[i];
    } return out;
}

float dot(vector<float> v1, vector<float> v2){
    float out = 0;
    for (int i = 0; i < v1.size(); i++){
        out += v1[i] * v2[i];
    } return out;
}

vector<float> subVectors(vector<float> v1, vector<float> v2) {
    vector<float> out;
    for (int i = 0; i < v1.size(); i++) {
        out.push_back(v1[i] - v2[i]);
    } return out;
}

vector<float> sumVectors(vector<float> v1, vector<float> v2) {
    vector<float> out;
    for (int i = 0; i < v1.size(); i++) {
        out.push_back(v1[i] + v2[i]);
    } return out;
}

vector<float> multVectors(vector<float> v1, vector<float> v2) {
    vector<float> out;
    for (int i = 0; i < v1.size(); i++) {
        out.push_back(v1[i] * v2[i]);
    } return out;
}

vector<float> multVectorByConstant(vector<float> v1, float c) {
    vector<float> out;
    for (int i = 0; i < v1.size(); i++) {
        out.push_back(v1[i] * c);
    } return out;
}

// Perceptron
vector<float> stepActivationFunction(float sum) {
    vector<float> out;
    if(sum > 0) {
        out.push_back(1);
    } else {
        out.push_back(0);
    }
    return out;
}

vector<float> predict(vector<float> inputs, vector<float> weights, float bias){
    float summation = dot(inputs, weights) + bias;
    return stepActivationFunction(summation);
}

tuple<vector<float>, float> train(vector<vector<float>> training_inputs, vector<vector<float>> labels, vector<float> weights, float bias, float learning_rate, int epochs) {
    for (int i = 0; i < epochs; i++){
        for (int j = 0; j < training_inputs.size(); j++){
            vector<float> inputs = training_inputs[j];
            vector<float> output = labels[j];

            vector<float> prediction = predict(inputs, weights, bias);
            vector<float> error = subVectors(output, prediction);

            weights = sumVectors(weights, multVectorByConstant(multVectorByConstant(inputs, error[0]), learning_rate)); 
            bias +=  sumVector(multVectorByConstant(error, learning_rate));
        }
    }
    tuple<vector<float>, float> tuple = make_tuple(weights, bias);
    return tuple;
}

// Main - gcc perceptron.cpp -lstdc++ -o perceptron.o -w
int main(){

    int epochs = 50;
    float learning_rate = 0.01;
    int number_of_inputs = 2;
    float bias = 0; 
    vector<float> weights = vectorZeros(number_of_inputs);

    // Inputs
    vector<float> i0{0,0};
    vector<float> i1{0,1};
    vector<float> i2{1,0};
    vector<float> i3{1,1};
    vector<vector<float>> inputs{i0, i1, i2, i3};
    // Outputs
    vector<float> o0{1};
    vector<float> o1{1};
    vector<float> o2{1};
    vector<float> o3{0};
    vector<vector<float>> outputs{o0, o1, o2, o3};

    tuple<vector<float>, float> t = train(inputs, outputs, weights, bias, learning_rate, epochs);

    // NAND Gate
    cout << "NAND Gate" << endl;
    cout << "X train:" << endl;
    printMatriz(inputs);
    cout << "y train:" << endl;
    printMatriz(outputs);
    cout << endl;

    cout << "weights: ";
    printVector(get<0>(t), true);
    cout << "bias: " << get<1>(t) << endl << endl;

    cout << "X[0, 0], Y[1] Predict => ";
    printVector( predict(inputs[0], get<0>(t), get<1>(t)) , true);
    cout << "X[0, 1], Y[1] Predict => ";
    printVector( predict(inputs[1], get<0>(t), get<1>(t)) , true);
    cout << "X[1, 0], Y[1] Predict => ";
    printVector( predict(inputs[2], get<0>(t), get<1>(t)) , true);
    cout << "X[1, 1], Y[0] Predict => ";
    printVector( predict(inputs[3], get<0>(t), get<1>(t)) , true);
    cout << endl << endl;

    // Iris dataset
    vector<vector<string>> csv = read("../iris.csv");
	//print_csv(csv);

    // Inputs
    vector<vector<float>> X1;
    for(int i = 1; i < 100; i++) {
        vector<float> aux;
        for (int j = 0; j < csv[i].size()-1; j++) {
            aux.push_back(stof(csv[i][j]));
        }
        X1.push_back(aux);
    }
    // Outputs
    vector<vector<float>> y1;
    for(int i = 1; i < 100; i++) {
        if(csv[i][csv[i].size()-1] == "Iris-setosa"){
            y1.push_back(vector<float> {1});
        } else {
            y1.push_back(vector<float> {0});
        }
    }

    int epochs1 = 100;
    float learning_rate1 = 0.01;
    int number_of_inputs1 = 4;
    float bias1 = 0; 
    vector<float> weights1 = vectorZeros(number_of_inputs1);

    cout << "Iris dataset" << endl;
    cout << "X: " << endl;
    printMatriz(X1);
    cout << "y: ";
    printVector(matrixToVector(y1), true);

    tuple<vector<float>, float> t1 = train(X1, y1, weights1, bias1, learning_rate1, epochs1);
    
    cout << endl << "weights: ";
    printVector(get<0>(t1), true);
    cout << "bias: " << get<1>(t1) << endl << endl;

    cout << "predicts" << endl;

    cout << "X"; printVector(X1[0], false); cout << "y"; printVector(y1[0], false); cout << "Predict => ";
    printVector( predict(X1[0], get<0>(t1), get<1>(t1)) , true);
    
    cout << "X"; printVector(X1[1], false); cout << "y"; printVector(y1[1], false); cout << "Predict => ";
    printVector( predict(X1[1], get<0>(t1), get<1>(t1)) , true);
    
    cout << "X"; printVector(X1[50], false); cout << "y"; printVector(y1[50], false); cout << "Predict => ";
    printVector( predict(X1[50], get<0>(t1), get<1>(t1)) , true);
    
    cout << "X"; printVector(X1[51], false); cout << "y"; printVector(y1[51], false); cout << "Predict => ";
    printVector( predict(X1[51], get<0>(t1), get<1>(t1)) , true);
    cout << endl;

    return 0;
}