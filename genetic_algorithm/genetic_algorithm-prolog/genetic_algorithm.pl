:- use_module(library(http/json)).
:- initialization(main).

:- dynamic individuo/7. % espacos, valores, cromossomo, espaco_usado, limite_espacos, geracao, nota_avaliacao

% Le um arquivo JSON
lerJson(File) :-
 open("../produtos.json", read, Teste),
 json_read_dict(Teste, File).

% Retorna um intervalo de uma lista
sliceArray([], _, _, [], _).
sliceArray([H|T], Start, End, [H|Tail], Cont) :- 
 Cont >= Start, Cont < End, NewCont is Cont+1, sliceArray(T, Start, End, Tail, NewCont).
sliceArray([_|T], Start, End, Elem, Cont) :- NewCont is Cont+1, sliceArray(T, Start, End, Elem, NewCont).

% Concatena duas Listas
concatArray(L1, L2, L3) :- append(L1, L2, L3).

% Retorna elemento pelo indice na lista
getByIndex([], _, _) :- fail.
getByIndex([H|_], 0, H).
getByIndex([_|T], Index, E) :- Index > 0, NewIndex is Index-1, getByIndex(T, NewIndex, E).

% Float (Scientific Notation) number to String
float_to_string(Number, String, Size) :- format(atom(A), Size, [Number]), atom_string(A, String).

% Retorna os produtos
getProdutos(Produtos) :- lerJson(Produtos).

getEspacos([], []).
getEspacos([P|T], [H|Tail]) :- H = P.espaco, getEspacos(T, Tail).

getValores([], []).
getValores([P|T], [H|Tail]) :- H = P.valor, getValores(T, Tail).

getNomes([], []).
getNomes([P|T], [H|Tail]) :- H = P.nome, getNomes(T, Tail).

% Retorna o tamanho da maior string de nome
getTamanhoStringProdutoComMaiorNome([], Maior, Maior).
getTamanhoStringProdutoComMaiorNome([H|T], Aux, Maior) :- 
 string_length(H.nome, StrSize), StrSize > Aux, getTamanhoStringProdutoComMaiorNome(T, StrSize, Maior).
getTamanhoStringProdutoComMaiorNome([_|T], Aux, Maior) :- getTamanhoStringProdutoComMaiorNome(T, Aux, Maior).

% Retorna o tamanho da maior string de espaco
getTamanhoStringProdutoComMaiorEspaco([], Maior, Maior).
getTamanhoStringProdutoComMaiorEspaco([H|T], Aux, Maior) :- float_to_string(H.espaco, Str, "~8f"),
 string_length(Str, StrSize), StrSize > Aux, getTamanhoStringProdutoComMaiorEspaco(T, StrSize, Maior).
getTamanhoStringProdutoComMaiorEspaco([_|T], Aux, Maior) :- getTamanhoStringProdutoComMaiorEspaco(T, Aux, Maior).

% Retorna o tamanho da maior string de valor
getTamanhoStringProdutoComMaiorValor([], Maior, Maior).
getTamanhoStringProdutoComMaiorValor([H|T], Aux, Maior) :- float_to_string(H.valor, Str, "~8f"),
 string_length(Str, StrSize), StrSize > Aux, getTamanhoStringProdutoComMaiorValor(T, StrSize, Maior).
getTamanhoStringProdutoComMaiorValor([_|T], Aux, Maior) :- getTamanhoStringProdutoComMaiorValor(T, Aux, Maior).

% Rretorna as infos necessarias para imprimir os produtos
getMaioresTamanhosStringProdutosInfo(Produtos, [MN, ME, MV]) :- 
 getTamanhoStringProdutoComMaiorValor(Produtos, 0, MV), getTamanhoStringProdutoComMaiorEspaco(Produtos, 0, ME), getTamanhoStringProdutoComMaiorNome(Produtos, 0, MN).

% Imprime x espacos na mesma linha
imprimeEspacos(0) :- write('').
imprimeEspacos(X) :- write(' '), NewX is X-1, imprimeEspacos(NewX).

% Imprime os produtos
imprimeProdutosAux([], _).
imprimeProdutosAux([H|T], [MN,ME,MV]) :-
 write(H.nome), string_length(H.nome, NL), NewMN is MN - NL, imprimeEspacos(NewMN), write(" - "),
 float_to_string(H.valor, Str1, "~5f"), write(Str1),  string_length(Str1, VL), NewMV is MV - VL, imprimeEspacos(NewMV), write(" - "),
 float_to_string(H.espaco, Str2, "~8f"), write(Str2), write_ln(''), imprimeProdutosAux(T, [MN,ME,MV]).

imprimeProdutos(Produtos) :- getMaioresTamanhosStringProdutosInfo(Produtos, [MN,ME,MV]),
 write("Produto"), NewMN is MN-6, imprimeEspacos(NewMN), 
 write("Valor"), NewMV is MV-2, imprimeEspacos(NewMV), 
 write_ln("Espaco"), imprimeProdutosAux(Produtos, [MN,ME,MV]).

% Cria um individuo
cromossomo([], 0).
cromossomo([H|T], S) :- NS is S-1, random(0,2,H), cromossomo(T, NS).

initIndividuo(Espacos, Valores, Limite, Individuo) :- length(Espacos, Size), cromossomo(C, Size),
 Individuo = individuo(Espacos, Valores, C, 0, Limite, 0, 0).

% Visualiza um individuo
visualizaIndividuo(Individuo) :-
 getIndividuoGeracao(Individuo, G), write("Geracao: "), write_ln(G),
 getIndividuoNotaAvaliacao(Individuo, N), write("Nota avaliacao R$: "), write_ln(N),
 getIndividuoEspacoUsado(Individuo, E), write("Espaco usado: "), write_ln(E),
 getIndividuoCromossomo(Individuo, C), write("Cromossomo: "), write_ln(C), write_ln("").

% Gets para o individuo
getIndividuoEspacos(individuo(Espacos, _, _, _, _, _, _), Espacos).
getIndividuoValores(individuo(_, Valores, _, _, _, _, _), Valores).
getIndividuoCromossomo(individuo(_, _, Cromossomo, _, _, _, _), Cromossomo).
getIndividuoEspacoUsado(individuo(_, _, _, EspacoUsado, _, _, _), EspacoUsado).
getIndividuoLimiteEspaco(individuo(_, _, _, _, LimiteEspaco, _, _), LimiteEspaco).
getIndividuoGeracao(individuo(_, _, _, _, _, Geracao, _), Geracao).
getIndividuoNotaAvaliacao(individuo(_, _, _, _, _, _, NotaAvaliacao), NotaAvaliacao).

% Avalia o cromossomo de um individuo
avaliacaoCromossomoIndividuo(Individuo, _, _, SomaEspaco, _, (1, SomaEspaco)) :- getIndividuoLimiteEspaco(Individuo, LE), SomaEspaco >= LE.
avaliacaoCromossomoIndividuo(_, [], Nota, SomaEspaco, _, (Nota, SomaEspaco)).
avaliacaoCromossomoIndividuo(Individuo, [H|T], Nota, SomaEspaco, Index, Return) :- H =:= 1, 
 getIndividuoValores(Individuo, Valores), getByIndex(Valores, Index, V), NewNota is Nota + V,
 getIndividuoEspacos(Individuo, Espacos), getByIndex(Espacos, Index, E), NewSomaEspaco is SomaEspaco + E,
 NewIndex is Index+1, avaliacaoCromossomoIndividuo(Individuo, T, NewNota, NewSomaEspaco, NewIndex, Return).
avaliacaoCromossomoIndividuo(Individuo, [_|T], Nota, SomaEspaco, Index, Return) :- NewIndex is Index+1,
 avaliacaoCromossomoIndividuo(Individuo, T, Nota, SomaEspaco, NewIndex, Return).

% Avalia um individuo
avaliacaoIndividuo(Individuo, Return) :-
 getIndividuoEspacos(Individuo, E), getIndividuoValores(Individuo, V), getIndividuoCromossomo(Individuo, C),
 getIndividuoLimiteEspaco(Individuo, LE), getIndividuoGeracao(Individuo, G), 
 avaliacaoCromossomoIndividuo(Individuo, C, 0, 0, 0, (NewNotaAvaliacao, NewEspacoUsado)),
 Return = individuo(E, V, C, NewEspacoUsado, LE, G, NewNotaAvaliacao).  

% Faz o crossover de individuos
corte(Size, Return) :- random(1, Size, Return).

crossoverIndividuo(A, B, (Filho1, Filho2)) :-
 getIndividuoCromossomo(A, C1), getIndividuoCromossomo(B, C2), length(C1, Size1), length(C2, Size2),
 getIndividuoGeracao(A, G), NewG is G+1, getIndividuoValores(A, V), getIndividuoEspacos(A, E),
 getIndividuoLimiteEspaco(A, LE),

 corte(Size1, Corte),

 sliceArray(C2, 0, Corte, Filho1_pt1, 0),
 sliceArray(C1, Corte, Size1, Filho1_pt2, 0),
 concatArray(Filho1_pt1, Filho1_pt2, F1),

 sliceArray(C1, 0, Corte, Filho2_pt1, 0),
 sliceArray(C2, Corte, Size2, Filho2_pt2, 0),
 concatArray(Filho2_pt1, Filho2_pt2, F2),

 avaliacaoCromossomoIndividuo(A, F1, 0, 0, 0, (NewNotaAvaliacao1, NewEspacoUsado1)),
 avaliacaoCromossomoIndividuo(A, F2, 0, 0, 0, (NewNotaAvaliacao2, NewEspacoUsado2)),

 Filho1 = individuo(E, V, F1, NewEspacoUsado1, LE, NewG, NewNotaAvaliacao1),
 Filho2 = individuo(E, V, F2, NewEspacoUsado2, LE, NewG, NewNotaAvaliacao2).

% Faz mutacao no cromossomo de um individuo
getCromossomoMutacaoAux(N, Taxa, Random, 0) :- Random < Taxa, N =:= 1.
getCromossomoMutacaoAux(N, Taxa, Random, 1) :- Random < Taxa, N =:= 0.
getCromossomoMutacaoAux(N, _, _, N).

getCromossomoMutacao([], _, []).
getCromossomoMutacao([H|T], Taxa, [N|Tail]) :- random(Rand), getCromossomoMutacaoAux(H, Taxa, Rand, N),
 getCromossomoMutacao(T, Taxa, Tail).

mutacaoIndividuo(Individuo, Taxa, NewIndividuo) :-
 getIndividuoCromossomo(Individuo, C), getIndividuoGeracao(Individuo, G),
 getIndividuoValores(Individuo, V), getIndividuoEspacos(Individuo, E),
 getIndividuoLimiteEspaco(Individuo, LE), getIndividuoEspacoUsado(Individuo, EU),
 getIndividuoNotaAvaliacao(Individuo, NA),

 getCromossomoMutacao(C, Taxa, NewCromossomo),
 NewIndividuo = individuo(E, V, NewCromossomo, EU, LE, G, NA).

% Cria a populacao
initPopulacao(0, _, _, _, []).
initPopulacao(Tamanho, Espacos, Valores, LimiteEspaco, [H|T]) :-
 initIndividuo(Espacos, Valores, LimiteEspaco, H),
 NewTamanho is Tamanho-1,
 initPopulacao(NewTamanho, Espacos, Valores, LimiteEspaco, T).

% Retorna o melhor individuo
melhorIndividuo(I1, I2, I1) :- getIndividuoNotaAvaliacao(I1, N1), getIndividuoNotaAvaliacao(I2, N2), N1 >= N2.
melhorIndividuo(_, I2, I2).

% Ordena a populacao
getPairs([], []).
getPairs([H|T], [NA-H|Tail]) :- getIndividuoNotaAvaliacao(H, NA), getPairs(T, Tail).

ordenaPopulacao(Populacao, PopulacaoOrdenada):-
 getPairs(Populacao, Pairs),
 sort(1, @>=, Pairs, ParesOrdenados),
 pairs_values(ParesOrdenados, PopulacaoOrdenada).

% Avalia a populacao
avaliaPopulacao([], []).
avaliaPopulacao([H|T], [I|Tail]) :- avaliacaoIndividuo(H, I), avaliaPopulacao(T, Tail).

% Soma as avaliacoes dos individuos
somaAvaliacoes([], Soma, Soma).
somaAvaliacoes([H|T], Acc, Soma) :- getIndividuoNotaAvaliacao(H, NA), NewAcc is Acc + NA, somaAvaliacoes(T, NewAcc, Soma). 

% Seleciona pai para o crossover
selecionaPaiAux(Populacao, PaiIndice, ValorSorteado, Soma, I, Out) :-
 length(Populacao, TamanhoPopulacao), getByIndex(Populacao, I, Individuo), getIndividuoNotaAvaliacao(Individuo, NA),
 I < TamanhoPopulacao, Soma < ValorSorteado,
 NewPaiIndice is PaiIndice+1, NewSoma is Soma + NA, NewI is I+1,
 selecionaPaiAux(Populacao, NewPaiIndice, ValorSorteado, NewSoma, NewI, Out).
selecionaPaiAux(_, PaiIndice, _, _, _, PaiIndice).

selecionaPai(Populacao, SomaAvaliacao, PaiIndice) :- random(Rand), ValorSorteado is Rand * SomaAvaliacao,
 selecionaPaiAux(Populacao, -1, ValorSorteado, 0, 0, PaiIndice).

% Visualiza melhor individuo de uma geracao
visualizaGeracao(Populacao) :- ordenaPopulacao(Populacao, PO), getByIndex(PO, 0, I), visualizaIndividuo(I).

% Cria uma nova populacao
criaNovaPopulacao(_, 0, _, _, []).
criaNovaPopulacao(Populacao, Tamanho, SomaAvaliacao, TaxaMutacao, [NewF1, NewF2|Tail]) :-
 selecionaPai(Populacao, SomaAvaliacao, Pai1Indice),
 selecionaPai(Populacao, SomaAvaliacao, Pai2Indice),
 getByIndex(Populacao, Pai1Indice, P1), getByIndex(Populacao, Pai2Indice, P2),
 crossoverIndividuo(P1, P2, (F1, F2)),
 mutacaoIndividuo(F1, TaxaMutacao, NewF1), mutacaoIndividuo(F2, TaxaMutacao, NewF2),
 NewTamanho is Tamanho-1,
 criaNovaPopulacao(Populacao, NewTamanho, SomaAvaliacao, TaxaMutacao, Tail).

% Loop
loopAux(Populacao, SomaAvaliacao, TaxaMutacao, TamanhoPopulacao, (NPA, Melhor)) :-
 NewTamanho is TamanhoPopulacao / 2,
 criaNovaPopulacao(Populacao, NewTamanho, SomaAvaliacao, TaxaMutacao, NovaPopulacao),
 avaliaPopulacao(NovaPopulacao, NPA), ordenaPopulacao(NPA, PO), getByIndex(PO, 0, Melhor).

looping(_, _, _, 0, _, Melhor, _) :- write_ln("Melhor solucao: "), visualizaIndividuo(Melhor), 
 getIndividuoCromossomo(Melhor, C), getProdutos(P), getProdutosIndividuo(P, C, 0, PI), imprimeProdutos(PI).
looping(SomaAvaliacao, LimiteEspaco, TamanhoPopulacao, NumGeracoes, TaxaMutacao, MelhorIndividuo, Populacao) :-
 loopAux(Populacao, SomaAvaliacao, TaxaMutacao, TamanhoPopulacao, (NovaPopulacao, Melhor)),
 visualizaGeracao(NovaPopulacao), melhorIndividuo(MelhorIndividuo, Melhor, M),
 NewNumGeracoes is NumGeracoes-1, somaAvaliacoes(NovaPopulacao, 0, NewSomaAvaliacao),
 looping(NewSomaAvaliacao, LimiteEspaco, TamanhoPopulacao, NewNumGeracoes, TaxaMutacao, M, NovaPopulacao).

% Retorna os produtos escolhidos apartir do cromossomo do individuo
getProdutosIndividuo(_, [], _, []).
getProdutosIndividuo(Produtos, [1|T], Index, [P|Tail]) :-
 getByIndex(Produtos, Index, P), NewIndex is Index+1,
 getProdutosIndividuo(Produtos, T, NewIndex, Tail).
getProdutosIndividuo(Produtos, [_|T], Index, Out) :-
 NewIndex is Index+1, getProdutosIndividuo(Produtos, T, NewIndex, Out).

main :-
 write("Tamanho da populacao: "), write_ln(20),
 write("Limite: "), write_ln(3.0000),
 write("Taxa mutacao: "), write_ln(0.1),
 write("Geracao: "), write_ln(100),
 write_ln(""),

 getProdutos(Produtos),
 imprimeProdutos(Produtos),
 write_ln(""),
 getEspacos(Produtos, E),
 getValores(Produtos, V),
 
 initPopulacao(20, E, V, 3.0000, Populacao),
 ordenaPopulacao(Populacao, PO),
 avaliaPopulacao(Populacao, PA),
 somaAvaliacoes(PA, 0, SomaAvaliacao),
 visualizaGeracao(PA),
 getByIndex(PO, 0, Melhor),
 looping(SomaAvaliacao, 3.0000, 20, 100, 0.1, Melhor, PA).