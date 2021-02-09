:- initialization(main).
:- use_module(library(csv)).
:- use_module(library(http/json)).

% Le um arquivo CSV
read_csv(Path, CSV) :- csv_read_file(Path, CSV, []).

row_dict(Names, Row, Dict) :-
 Row =.. [row|Fields],
 pairs_keys_values(Data, Names, Fields),
 dict_create(Dict, _, Data).

% Converte um CSV para JSON
csv_to_json(CSV, JSON) :-
 CSV = [Colnames|Rows],
 Colnames =.. [row|Names],
 maplist(row_dict(Names), Rows, JSON).

% Retorna os inputs do dataset Iris
getX([], []).
getX([H1|T1], [[H1.sepal_length, H1.sepal_width, H1.petal_length, H1.petal_width]|T2]) :- getX(T1, T2).

% Retorna os outputs do dataset Iris
getY([], []).
getY([H1|T1], [[1]|T2]) :- atom_chars(H1.label, Chars), string_chars(Str, Chars), Str == "Iris-setosa", getY(T1, T2).
getY([_|T1], [[0]|T2]) :- getY(T1, T2).

% Retorna um intervalo de uma lista
sliceArray([], _, _, [], _).
sliceArray([H|T], Start, End, [H|Tail], Cont) :- 
 Cont >= Start, Cont < End, NewCont is Cont+1, sliceArray(T, Start, End, Tail, NewCont).
sliceArray([_|T], Start, End, Elem, Cont) :- NewCont is Cont+1, sliceArray(T, Start, End, Elem, NewCont).

% Retorna elemento pelo indice na lista
getByIndex([], _, _) :- fail.
getByIndex([H|_], 0, H).
getByIndex([_|T], Index, E) :- Index > 0, NewIndex is Index-1, getByIndex(T, NewIndex, E).

% Imprime x espacos na mesma linha
imprimeEspacos(0) :- write('').
imprimeEspacos(X) :- write(' '), NewX is X-1, imprimeEspacos(NewX).

% Retorna o tamanho do numero com mais digitos
getBiggestSizeM_Aux([], Maior, Maior).
getBiggestSizeM_Aux([H|T], Aux, Maior) :- number_string(H, Str),
 string_length(Str, StrSize), StrSize > Aux, getBiggestSizeM_Aux(T, StrSize, Maior).
getBiggestSizeM_Aux([_|T], Aux, Maior) :- getBiggestSizeM_Aux(T, Aux, Maior).

getBiggestSizeM([], Maior, Maior).
getBiggestSizeM([H|T], Aux, Maior) :- getBiggestSizeM_Aux(H, -1, M), M > Aux, getBiggestSizeM(T, M, Maior).
getBiggestSizeM([_|T], Aux, Maior) :- getBiggestSizeM(T, Aux, Maior).

% Imprime uma matriz
printMatrix(M) :- 
 getBiggestSizeM(M, -1, BigSize), length(M, MSize),
 write("["), 
 printMatrixAux1(M, MSize, BigSize, 0),
 write_ln(" ]").
 
printMatrixAux1([], _, _, _) :- write("").
printMatrixAux1([H|T], MSize, BigSize, 0) :-
 write(" ["),
 printMatrixAux2(H, BigSize, 0),
 printMatrixAux3(MSize, 0),
 printMatrixAux1(T, MSize, BigSize, 1).
printMatrixAux1([H|T], MSize, BigSize, Index) :-
 write("  ["), NewIndex is Index + 1,
 printMatrixAux2(H, BigSize, 0),
 printMatrixAux3(MSize, Index),
 printMatrixAux1(T, MSize, BigSize, NewIndex).

printMatrixAux2([], _ ,_ ) :- write("").
printMatrixAux2([H], BigSize, _) :- number_string(H, Str), string_length(Str, StrSize), QtdE is BigSize - StrSize,
 imprimeEspacos(QtdE), write(Str).
printMatrixAux2([H|T], BigSize, Index) :- 
 number_string(H, Str), string_length(Str, StrSize), QtdE is BigSize - StrSize, NewIndex is Index + 1,
 imprimeEspacos(QtdE), write(Str), write(" "), printMatrixAux2(T, BigSize, NewIndex).

printMatrixAux3(MSize, NewMSize) :- NewMSize is MSize - 1, write("]").
printMatrixAux3(_, _) :- write_ln("]").

% Imprime uma vetor
printVector(Vetor, Endl) :-
 length(Vetor, Size),
 write("["),
 printVectorAux1(Vetor, Size, 0),
 printVectorAux2(Endl).

printVectorAux1([], _, _) :- write("").
printVectorAux1([H], Size, Index) :- Index is Size - 1, number_string(H, Str), write(Str).
printVectorAux1([H|T], Size, Index) :- NewIndex is Index + 1, 
 number_string(H, Str), write(Str), write(" "), printVectorAux1(T, Size, NewIndex).

printVectorAux2(true) :- write_ln("]").
printVectorAux2(false) :- write("] ").

% Matriz para Vetor
matrixToVector([], Out, Out).
matrixToVector([H|T], Aux, Vetor) :- append(Aux, H, Out), matrixToVector(T, Out, Vetor).

% Retorna um vetor com n zeros
vectorZeros(0, []).
vectorZeros(N, [0|T]) :- NewN is N - 1, vectorZeros(NewN, T).

% Soma todos valores de um vetor -> sum_list(Vetor, Soma)

% Multiplica dois vetores
dot([], [], 0).
dot([H1|T1], [H2|T2], Dot) :- dot(T1, T2, Z), Dot is H1 * H2 + Z.

% Soma dois vetores
sumVectors([], [], []).
sumVectors([H1|T1], [H2|T2], [H|T]) :- H is H1 + H2, sumVectors(T1, T2, T).

% Subtrai dois vetores
subVectors([], [], []).
subVectors([H1|T1], [H2|T2], [H|T]) :- H is H1 - H2, subVectors(T1, T2, T).

% Multiplica dois vetores (elemento por elemento)
multVectors([], [], []).
multVectors([H1|T1], [H2|T2], [H|T]) :- H is H1 * H2, multVectors(T1, T2, T).

% Soma dois vetores
multVectorByConstant([], _, []).
multVectorByConstant([H1|T1], C, [H|T]) :- H is H1 * C, multVectorByConstant(T1, C, T).

% Funcao de ativacao
stepActivationFunction(Sum, [1]) :- Sum > 0.
stepActivationFunction(_, [0]).

% Funcao de predicao
predict(Inputs, Weights, Bias, Prediction) :- 
 dot(Inputs, Weights, Dot), Sum is Dot + Bias, stepActivationFunction(Sum, Prediction).

% Funcao de treinamento
trainAux([], [], Weights, Bias, _, (Weights, Bias)).
trainAux([X|XT], [Y|YT], Weights, Bias, LearningRate, Out) :-
 predict(X, Weights, Bias, Prediction),
 subVectors(Y, Prediction, Error),
 % Atualizando os pesos
 getByIndex(Error, 0, E),
 multVectorByConstant(X, E, InputsXError),
 multVectorByConstant(InputsXError, LearningRate, InputsXErrorXLearningRate),
 sumVectors(Weights, InputsXErrorXLearningRate, NewWeights),
 % Atualizando o bias
 multVectorByConstant(Error, LearningRate, ErrorXLearningRate),
 sum_list(ErrorXLearningRate, SumErrors),
 NewBias is Bias + SumErrors,

 trainAux(XT, YT, NewWeights, NewBias, LearningRate, Out).

train(_, _, Weights, Bias, _, 0, (Weights, Bias)).
train(X, Y, Weights, Bias, LearningRate, Epochs, Out) :-
 NewEpochs is Epochs - 1,
 trainAux(X, Y, Weights, Bias, LearningRate, (W, B)),
 train(X, Y, W, B, LearningRate, NewEpochs, Out).

main :-
 % NAND Gate
 write_ln("NAND Gate"),
 write_ln("X: "),
 printMatrix([[0,0], [0,1], [1,0], [1,1]]),
 write_ln("y: "),
 printMatrix([[1], [1], [1], [0]]),

 vectorZeros(2, Weights),
 train([[0,0], [0,1], [1,0], [1,1]], [[1], [1], [1], [0]], Weights, 0, 0.01, 25, (W, B)),
 
 write_ln(""),
 write("weights: "), printVector(W, true),
 write("bias: "), write_ln(B),
 
 write_ln(""),
 write_ln("predicts:"),
 predict([0, 0], W, B, P1),
 write("X[0, 0], Y[1] Predict => "), printVector(P1, true),
 predict([0, 1], W, B, P2),
 write("X[0, 1], Y[1] Predict => "), printVector(P2, true),
 predict([1, 0], W, B, P3),
 write("X[1, 0], Y[1] Predict => "), printVector(P3, true),
 predict([1, 1], W, B, P4),
 write("X[1, 1], Y[0] Predict => "), printVector(P4, true),
 write_ln(""),

 write_ln("Iris dataset"),
 read_csv("../Iris.csv", CSV),
 csv_to_json(CSV, JSON),
 sliceArray(JSON, 0, 100, X1_raw, 0),
 sliceArray(JSON, 0, 100, Y1_raw, 0), 
 getX(X1_raw, X1),
 getY(Y1_raw, Y1), matrixToVector(Y1, [], Y1v),
 
 write_ln("X: "),
 printMatrix(X1),
 write("y: "),
 printVector(Y1v, true),
 
 vectorZeros(4, Weights1),
 train(X1, Y1, Weights1, 0, 0.01, 100, (W1, B1)),
 
 write_ln(""),
 write("weights: "), printVector(W1, true),
 write("bias: "), write_ln(B1),
 
 write_ln(""),
 write_ln("predicts:"),
 getByIndex(X1, 0, InP5),
 getByIndex(Y1, 0, OutP5),
 predict(InP5, W1, B1, P5),
 write("X"), printVector(InP5, false), write("Y"), printVector(OutP5, false), write("Predict => "), printVector(P5, true),

 getByIndex(X1, 1, InP6),
 getByIndex(Y1, 1, OutP6),
 predict(InP6, W1, B1, P6),
 write("X"), printVector(InP6, false), write("Y"), printVector(OutP6, false), write("Predict => "), printVector(P6, true),

 getByIndex(X1, 50, InP7),
 getByIndex(Y1, 50, OutP7),
 predict(InP7, W1, B1, P7),
 write("X"), printVector(InP7, false), write("Y"), printVector(OutP7, false), write("Predict => "), printVector(P7, true),

 getByIndex(X1, 51, InP8),
 getByIndex(Y1, 51, OutP8),
 predict(InP8, W1, B1, P8),
 write("X"), printVector(InP8, false), write("Y"), printVector(OutP8, false), write("Predict => "), printVector(P8, true).