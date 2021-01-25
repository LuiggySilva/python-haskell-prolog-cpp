from random import random
import matplotlib.pyplot as plt
import json

class Produto():
  def __init__(self, nome, espaco, valor):
    self.nome = nome
    self.espaco = espaco
    self.valor = valor

class Individuo():
  def __init__(self, espacos, valores, limite_espacos, geracao=0):
    self.espacos = espacos
    self.espaco_usado = 0
    self.valores = valores
    self.limite_espacos = limite_espacos

    self.geracao = geracao
    self.nota_avaliacao = 0
    self.cromossomo = []

    for _ in range(len(espacos)):
      if(random() < 0.5):
        self.cromossomo.append(0)
      else:
        self.cromossomo.append(1)
  
  def avaliacao(self):
    nota = 0
    soma_espacos = 0
    for i in range(len(self.cromossomo)):
      if(self.cromossomo[i] == 1):
        nota += self.valores[i]
        soma_espacos += self.espacos[i]
      
    if(soma_espacos > self.limite_espacos):
      nota = 1
    self.nota_avaliacao = nota
    self.espaco_usado = soma_espacos
  
  def crossover(self, outro_individuo):
    corte = round(random() + len(self.cromossomo))
    
    filho1 = outro_individuo.cromossomo[0:corte] + self.cromossomo[corte::]
    filho2 = self.cromossomo[0:corte] + outro_individuo.cromossomo[corte::]
    
    filhos = (Individuo(self.espacos, self.valores, self.limite_espacos, self.geracao + 1),
              Individuo(self.espacos, self.valores, self.limite_espacos, self.geracao + 1))

    filhos[0].cromossomo = filho1
    filhos[1].cromossomo = filho2
    return filhos
  
  def mutacao(self, taxa_mutacao):
    for i in range(len(self.cromossomo)):
      if(random() < taxa_mutacao):
        if(self.cromossomo[i] == 1):
          self.cromossomo[i] = 0
        else:
          self.cromossomo[i] = 1
    return self

class AlgoritmoGenetico():
  def __init__(self, tamanho_populacao):
    self.tamanho_populacao = tamanho_populacao
    self.populacao = []
    self.geracao = 0
    self.melhor_solucao = None
    self.solucoes = []
  
  def inicializa_populacao(self, espacos, valores, limite_espacos):
    for _ in range(self.tamanho_populacao):
      self.populacao.append(Individuo(espacos, valores, limite))
    self.melhor_solucao = self.populacao[0]

  def melhor_individuo(self, individuo):
    if(individuo.nota_avaliacao > self.melhor_solucao.nota_avaliacao):
      self.melhor_solucao = individuo

  def ordena_populacao(self):
    self.populacao = sorted(self.populacao, key= lambda populacao: populacao.nota_avaliacao, reverse= True)
  
  def avaliacao_populacao(self):
    for individuo in self.populacao:
      individuo.avaliacao()
  
  def soma_avaliacoes(self):
    soma = 0
    for individuo in self.populacao:
      soma += individuo.nota_avaliacao
    return soma
  
  # selRoulette
  def seleciona_pai(self, soma_avaliacao):
    pai = -1
    valor_sorteado = random() * soma_avaliacao
    soma = 0
    i = 0
    while(i < len(self.populacao) and soma < valor_sorteado):
      soma += self.populacao[i].nota_avaliacao
      pai += 1
      i += 1
    return pai
  
  def visualiza_geracao(self):
    melhor = self.populacao[0]
    print( f'Geração: {melhor.geracao}\nNota avaliacao: R$: {melhor.nota_avaliacao}\nEspaço usado: {melhor.espaco_usado}\nCromossomo: {melhor.cromossomo}\n' )
  
  def resolver(self, taxa_mutacao, numero_geracoes, espacos, valores, limite_espacos):
    self.inicializa_populacao(espacos, valores, limite_espacos)
    self.avaliacao_populacao()
    self.ordena_populacao()
    self.melhor_individuo(self.populacao[0])  
    self.visualiza_geracao()
    self.solucoes.append(self.melhor_solucao.nota_avaliacao)

    for _ in range(numero_geracoes):
      soma_avaliacao = self.soma_avaliacoes()
      nova_populacao = []
      for _ in range(0, self.tamanho_populacao, 2):
        pai1 = self.seleciona_pai(soma_avaliacao)
        pai2 = self.seleciona_pai(soma_avaliacao)

        filhos = self.populacao[pai1].crossover(self.populacao[pai2])
        nova_populacao.append(filhos[0].mutacao(taxa_mutacao))
        nova_populacao.append(filhos[1].mutacao(taxa_mutacao))
      
      self.populacao = list(nova_populacao)
      
      self.avaliacao_populacao()
      self.ordena_populacao()
      self.melhor_individuo(self.populacao[0])  
      self.visualiza_geracao()
      self.solucoes.append(self.melhor_solucao.nota_avaliacao)
    
    return self.melhor_solucao

def get_produtos(path):
  try:
    _file = open(path, "r")
    produtos = json.load(_file)
    lista_produtos = []
    for produto in produtos:
      lista_produtos.append(Produto(produto["nome"], float(produto["espaco"]), float(produto["valor"])))
    return lista_produtos
  except Exception as e:
    print(e)

def imprime_produtos(produtos):
  maior_qtd_char_nome = len(produtos[0].nome)
  maior_qtd_char_espaco = len(str(produtos[0].espaco))
  maior_qtd_char_valor = len(str(produtos[0].valor))

  for p in produtos:
    if (len(p.nome) > maior_qtd_char_nome):
      maior_qtd_char_nome = len(p.nome)
    if(len(str(p.espaco)) > maior_qtd_char_espaco):
      maior_qtd_char_espaco = len(str(p.espaco))
    if(len(str(p.valor)) > maior_qtd_char_valor):
      maior_qtd_char_valor = len(str(p.valor)) 

  maior_qtd_char_nome += 1
  maior_qtd_char_valor += 1
  maior_qtd_char_espaco += 1

  print('Produto', end='')
  print(' ' * (maior_qtd_char_nome - 4), end='')
  print('Valor', end='')
  print(' ' * (maior_qtd_char_valor - 2), end='')
  print('Espaco')

  for p in produtos:
    print(p.nome, end='')
    print(' ' * (maior_qtd_char_nome - len(p.nome)), end=' - ')
    print(p.valor, end='')
    print(' ' * (maior_qtd_char_valor - len(str(p.valor))), end=' - ')
    print(p.espaco, end='')
    print(' ' * (maior_qtd_char_espaco - len(str(p.espaco))))
  print()

def algoritmo_genetico(produtos=None, tamanho_populacao=None, limite_espacos=None, taxa_mutacao=None, numero_geracoes=None):
  espacos = [p.espaco for p in produtos]
  valores = [p.valor for p in produtos]
  nomes = [p.nome for p in produtos]

  ag = AlgoritmoGenetico(tamanho_populacao)
  resultado = ag.resolver(taxa_mutacao, numero_geracoes, espacos, valores, limite_espacos)
  print(f'Melhor solução\nGeração: {resultado.geracao}\nNota avaliacao: R${resultado.nota_avaliacao}\nEspaco usado: {resultado.espaco_usado}\nCromossomo: {resultado.cromossomo}\n')

  lista_produtos_selecionados = []
  for i in range(len(resultado.cromossomo)):
    if(resultado.cromossomo[i] == 1):
      lista_produtos_selecionados.append(produtos[i])

  imprime_produtos(lista_produtos_selecionados)

  plt.plot(ag.solucoes)
  plt.title('Acompanhamento dos valores', fontdict={'fontsize':20})
  plt.show()


populacao = 20
limite = 3
mutacao = 0.1
geracoes = 100

produtos_path = "../produtos.json"
produtos = get_produtos(produtos_path)

print(f"Tamanho da populacao: {populacao}")
print(f"Limite: {limite}")
print(f"Taxa mutacao: {mutacao}")
print(f"Geracoes: {geracoes}", end='\n'*2)
imprime_produtos(produtos)

algoritmo_genetico(produtos= produtos, tamanho_populacao= populacao, limite_espacos= limite, taxa_mutacao= mutacao, numero_geracoes= geracoes)