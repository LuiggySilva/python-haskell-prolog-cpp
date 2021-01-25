
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
#include "json_reader/jsoncpp.cpp"

using namespace std;

typedef struct {
  string nome;
  float valor; 
  float espaco; 
} Produto;

typedef struct {
  vector <float> espacos;
  vector <float> valores;
  vector <int> cromossomo;
  float espaco_usado;
  float limite_espacos;
  int geracao;
  float nota_avaliacao;
} Individuo;

// Le um arquivo
Json::Value readJSONFile(string file_path) {
  vector<string> result;
  string line;
  ifstream inFile;

  inFile.open(file_path);
  if(!inFile) {
    cout << "Unable to open file";
    exit(1);
  }
  for (string line; getline(inFile, line);){
    result.push_back(line);
  }
  inFile.close();

  string rawJson = "";
  for (int i = 0; i < result.size(); i++){
    rawJson += result.at(i);
  }

  const auto rawJsonLength = static_cast<int>(rawJson.length());
  constexpr bool shouldUseOldWay = false;
  JSONCPP_STRING err;
  Json::Value root;

  if (shouldUseOldWay) {
    Json::Reader reader;
    reader.parse(rawJson, root);
  } else {
    Json::CharReaderBuilder builder;
    const std::unique_ptr<Json::CharReader> reader(builder.newCharReader());
    if (!reader->parse(rawJson.c_str(), rawJson.c_str() + rawJsonLength, &root,
                       &err)) {
      std::cout << "error" << std::endl;
      return EXIT_FAILURE;
    }
  }
  return root;
}

// Imprime um vetor de inteiros
void printArray(vector<int> arr){
  for (int cromo : arr){
      cout << to_string(cromo) << ' ';
  }
  cout << endl;
}

// Divide uma string por um separador
const vector<string> splitString(const string& s, const char& c) {
	string buff{""};
	vector<string> v;
	
	for(auto n:s) {
		if(n != c) buff+=n; else
		if(n == c && buff != "") { v.push_back(buff); buff = ""; }
	}
	if(buff != "") v.push_back(buff);
	
	return v;
}

// Retorna um intervalo do array
vector<int> sliceArray(vector<int>& arr, int x, int y) {
  vector<int> result;
  for (int i = 0; i < y; i++){
    if(i >= x){
      result.push_back(arr[i]);
    }
  }
  return result;
}

// Concatena dois vetores
vector<int> concatArray(vector<int> v1, vector<int> v2) {
  vector<int> v(v1);
  for(int n : v1){ v.push_back(n);}
  for(int n : v2){ v.push_back(n);}
  return v;
}

// Cria os produtos
vector<Produto> getProdutos() {
  Json::Value produtos_raw = readJSONFile("../produtos.json");
  vector<Produto> produtos;

  for (int i = 0; i < produtos_raw.size(); i++){
    Produto p;
    p.nome = produtos_raw[i]["nome"].asString();
    p.espaco = produtos_raw[i]["espaco"].asFloat();
    p.valor = produtos_raw[i]["valor"].asFloat();
    produtos.push_back(p);
  }
  return produtos;
}

// Imprime espacos em branco
void printEspacos(int qtd){
  for(int i = 0; i < qtd; i++){
    cout << ' ';
  }
}

// Imprime os produtos
void imprimeProdutos(vector<Produto> produtos){
  int maior_qtd_char_nome = produtos[0].nome.size();
  int maior_qtd_char_espaco = to_string(produtos[0].espaco).size();
  int maior_qtd_char_valor = to_string(produtos[0].valor).size();;

  for (Produto p : produtos) {
    if(p.nome.size() > maior_qtd_char_nome){
      maior_qtd_char_nome = p.nome.size();
    }
    if(to_string(p.espaco).size() > maior_qtd_char_espaco){
      maior_qtd_char_espaco = to_string(p.espaco).size();
    }
    if(to_string(p.valor).size() > maior_qtd_char_valor){
      maior_qtd_char_valor = to_string(p.valor).size();
    }
  }

  maior_qtd_char_nome += 1;
  maior_qtd_char_espaco += 1;
  maior_qtd_char_valor += 1;

  cout << "Produto";
  printEspacos(maior_qtd_char_nome - 4);
  cout << "Valor";
  printEspacos(maior_qtd_char_valor - 2);
  cout << "Espaco";
  cout << endl;

  for(Produto p : produtos){
    cout << p.nome;
    printEspacos(maior_qtd_char_nome - p.nome.size());
    cout << " - ";

    cout << to_string(p.valor);
    printEspacos(maior_qtd_char_valor - to_string(p.valor).size());
    cout << " - ";

    cout << to_string(p.espaco);
    printEspacos(maior_qtd_char_espaco - to_string(p.espaco).size());
    cout << endl;
  }
}

/* INICIO - Algoritmos dos Individuos */
Individuo initIndividuo( vector<float> espacos, vector<float> valores, int limite_espacos) {
  Individuo individuo;
  individuo.espacos = espacos;
  individuo.valores = valores;
  individuo.espaco_usado = 0;
  individuo.nota_avaliacao = 0;
  individuo.geracao = 0;
  individuo.limite_espacos = limite_espacos;

  vector<int> cromossomo;
  for(int i = 0; i < espacos.size(); i++){
    if((float) rand()/RAND_MAX < 0.5){
      cromossomo.push_back(0);
    }
    else{
      cromossomo.push_back(1);
    }
  }

  individuo.cromossomo = cromossomo;
  return individuo;
}

Individuo avaliacaoIndividuo(Individuo individuo) {
  int nota = 0;
  float soma_espacos = 0;

  for (int i = 0; i < individuo.cromossomo.size(); i++){
    if(individuo.cromossomo[i] == 1){
      nota += individuo.valores[i];
      soma_espacos += individuo.espacos[i];
    }
  }
  if(soma_espacos > individuo.limite_espacos){
    nota = 1;
  }

  individuo.nota_avaliacao = nota;
  individuo.espaco_usado = soma_espacos;
  return individuo;
}

tuple<Individuo, Individuo> crossoverIndividuo(Individuo a, Individuo b) {
  int corte = 0;
  do {
    corte = round(rand() % a.cromossomo.size());
  } while(corte != 0);

  vector<int> filho1_pt1 = sliceArray(b.cromossomo, 0, corte);
  vector<int> filho1_pt2 = sliceArray(a.cromossomo, corte, a.cromossomo.size());
  vector<int> f1 = concatArray(filho1_pt1, filho1_pt2);

  vector<int> filho2_pt1 = sliceArray(a.cromossomo, 0, corte);
  vector<int> filho2_pt2 = sliceArray(b.cromossomo, corte, b.cromossomo.size());
  vector<int> f2 = concatArray(filho2_pt1, filho2_pt2);

  Individuo filho1 = initIndividuo(a.espacos, a.valores, a.limite_espacos);
  filho1.cromossomo = f1;
  filho1.geracao = a.geracao + 1;

  Individuo filho2 = initIndividuo(a.espacos, a.valores, a.limite_espacos);
  filho2.cromossomo = f2;
  filho2.geracao = a.geracao + 1;

  tuple<Individuo, Individuo> filhos = make_tuple(filho1, filho2);
  return filhos;
}

Individuo mutacaoIndividuo(Individuo individuo, float taxa_mutacao) {
  for (int i = 0; i < individuo.cromossomo.size(); i++){
    if((float) rand()/RAND_MAX < taxa_mutacao){
      if(individuo.cromossomo[i] == 1){
        individuo.cromossomo[i] = 0;
      }
      else {
        individuo.cromossomo[i] = 1;
      }
    }
  }
  return individuo;
}
/* FIM - Algoritmos dos Individuos */

/* INICIO - Algoritmo Genetico*/
vector<Individuo> initPopulacao(int tamanho_populacao, vector<float> espacos, vector<float> valores, int limite_espacos) {
  vector<Individuo> populacao;
  for (int i = 0; i < tamanho_populacao; i++) {
    Individuo individuo = initIndividuo(espacos, valores, limite_espacos);
    populacao.push_back(individuo);
  }
  return populacao;
}

Individuo melhorIndividuo(Individuo a, Individuo b) {
  if(a.nota_avaliacao >= b.nota_avaliacao){
    return a;
  }
  else {
    return b;
  }
}

vector<Individuo> ordenaPopulacao(vector<Individuo> populacao) {
  for (int i = 0; i < populacao.size(); i++) {
    for (int j = 0; j < populacao.size(); j++) {
      if(populacao[i].nota_avaliacao < populacao[j].nota_avaliacao){
        Individuo aux = populacao[i];
        populacao[i] = populacao[j];
        populacao[j] = aux;
      }
    } 
  }
  return populacao;
}

vector<Individuo> avaliaPopulacao(vector<Individuo> populacao) {
  for (int i = 0; i < populacao.size(); i++){
    populacao[i] = avaliacaoIndividuo(populacao[i]);
  }
  return populacao;
}

float somaAvaliacoes(vector<Individuo> populacao) {
  float soma = 0;
  for (Individuo individuo : populacao) {
    soma += individuo.nota_avaliacao;
  }
  return soma;
}

int selecionaPai(vector<Individuo> populacao, int soma_avaliacao) {
  int pai_indice = -1;
  float valor_sorteado = ((float) rand()/RAND_MAX) * soma_avaliacao;
  int soma = 0;
  int i = 0;

  while(i < populacao.size() && soma < valor_sorteado){
    soma += populacao[i].nota_avaliacao;
    pai_indice += 1;
    i += 1;
  }
  return pai_indice;
}

void visualizaIndividuo(Individuo invididuo) {
  cout << "Geracao: " << to_string(invididuo.geracao) << endl;
  cout << "Nota avaliacao: R$" << to_string(invididuo.nota_avaliacao) << endl;
  cout << "Espaco usado: " << to_string(invididuo.espaco_usado) << endl;
  cout << "Cromossomo: ";
  printArray(invididuo.cromossomo);
  cout << endl;
}

void visualizaGeracao(vector<Individuo> populacao) {
  Individuo melhor = populacao[0];
  visualizaIndividuo(melhor);
}

vector<int> resolver(int tamanho_populacao, float taxa_mutacao, int numero_geracoes, vector<Produto> produtos, int limite_espacos) {
  vector<float> espacos;
  vector<float> valores;
  vector<string> nomes;

  imprimeProdutos(produtos);
  for (Produto produto : produtos){
    espacos.push_back(produto.espaco);
    valores.push_back(produto.valor);
    nomes.push_back(produto.nome);
  }
  cout << endl;

  vector<Individuo> populacao = initPopulacao(tamanho_populacao, espacos, valores, limite_espacos);
  vector<int> solucoes;
  Individuo melhor_individuo;

  populacao = avaliaPopulacao(populacao);
  populacao = ordenaPopulacao(populacao);
  melhor_individuo = melhorIndividuo(populacao[0], populacao[0]);
  visualizaGeracao(populacao);
  solucoes.push_back(melhor_individuo.nota_avaliacao);

  for (int i = 1; i <= numero_geracoes; i++){
    cout << endl;
    int soma_avaliacoes = somaAvaliacoes(populacao);
    vector<Individuo> nova_populacao;
    for (int j = 0; j < (int)(tamanho_populacao/2); j++){
      int pai1 = selecionaPai(populacao, soma_avaliacoes);
      int pai2 = selecionaPai(populacao, soma_avaliacoes);

      tuple<Individuo, Individuo> filhos = crossoverIndividuo(populacao[pai1], populacao[pai2]);
      nova_populacao.push_back(mutacaoIndividuo(get<0>(filhos), taxa_mutacao));
      nova_populacao.push_back(mutacaoIndividuo(get<1>(filhos), taxa_mutacao));
    }
    populacao = nova_populacao;

    populacao = avaliaPopulacao(populacao);
    populacao = ordenaPopulacao(populacao);
    melhor_individuo = melhorIndividuo(populacao[0], melhor_individuo);
    visualizaGeracao(populacao);
    solucoes.push_back(melhor_individuo.nota_avaliacao);    
  }

  cout << endl << "Melhor individuo" << endl;
  visualizaIndividuo(melhor_individuo);
  vector<Produto> produtos_selecionados;
  for(int i = 0; i < melhor_individuo.cromossomo.size(); i++){
    if(melhor_individuo.cromossomo[i] == 1){
      produtos_selecionados.push_back(produtos[i]);
    }
  }
  imprimeProdutos(produtos_selecionados);
  return solucoes;
}
/* FIM - Algoritmo Genetico */

// Main - gcc genetic_algorithm.cpp -lstdc++ -o genetic_algorithm.o -w
int main() {
  setlocale(LC_ALL, "");
  srand((unsigned) time(NULL));

  int tamanho_populacao = 20;
  int limite = 3;
  float taxa_mutacao = 0.01;
  int geracoes = 100;

  cout << "Tamanho da populacao: " << to_string(tamanho_populacao) << endl;
  cout << "Limite: " << to_string(limite) << endl;
  cout << "Taxa mutacao: " << to_string(taxa_mutacao) << endl;
  cout << "Geracoes: " << to_string(geracoes) << endl;
  cout << endl;
  vector<Produto> produtos = getProdutos();
  vector<int> solucoes = resolver(tamanho_populacao, taxa_mutacao, geracoes, produtos, limite);

  return 0;
}