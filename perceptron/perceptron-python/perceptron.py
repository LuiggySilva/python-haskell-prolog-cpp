import pandas as pd
import numpy as np

class Perceptron:

    def __init__(self, number_of_inputs, epochs=100, learning_rate=0.01):
        self.epochs = epochs
        self.learning_rate = learning_rate
        self.weights = np.zeros(number_of_inputs)
        self.bias = np.zeros(1)
           
    def predict(self, inputs):
        summation = np.dot(inputs, self.weights) + self.bias
        return np.array([self.stepActivationFunction(summation)])
    
    def stepActivationFunction(self, _sum):    
        return 1 if sum(_sum) > 0 else 0

    def train(self, training_inputs, labels):
        for _ in range(self.epochs):
            for inputs, label in zip(training_inputs, labels):
                prediction = self.predict(inputs)
                error = label - prediction
                self.weights += self.learning_rate * error * inputs
                self.bias += self.learning_rate * error

# NAND Gate
X = np.array([[0, 0],[0, 1],[1, 0],[1, 1]])
y = np.array([[1],[1],[1],[0]])

print("NAND Gate")
print('X train:')
print(X)
print('y train:')
print(y)

p = Perceptron(2)
p.train(X, y)

print('\nweights:', p.weights)
print('bias:', p.bias)
print('\npredicts:')
print(f'X{X[0]}, y[{y[0]}] Predict => ',p.predict(X[0]))
print(f'X{X[1]}, y[{y[1]}] Predict => ',p.predict(X[1]))
print(f'X{X[2]}, y[{y[2]}] Predict => ',p.predict(X[2]))
print(f'X{X[3]}, y[{y[3]}] Predict => ',p.predict(X[3]))

# Iris dataset
def update_column(val):
    return 1 if val == "Iris-setosa" else 0

df = pd.read_csv("../iris.csv")
df['label'] = df['label'].apply(update_column)
df = df.values

X1 = df[0:100, 0:4]
y1 = df[0:100, 4]

print("\nIris dataset")
print('X train:')
print(X1)
print('y train:')
print(y1)

p1 = Perceptron(4)
p1.train(X1, y1)

print('\nweights:', p1.weights)
print('bias:', p1.bias)
print('\npredicts:')
print(f'X{X1[0]}, y[{y1[0]}] Predict => ',p1.predict(X1[0]))
print(f'X{X1[1]}, y[{y1[1]}] Predict => ',p1.predict(X1[1]))
print(f'X{X1[50]}, y[{y1[50]}] Predict => ',p1.predict(X1[50]))
print(f'X{X1[51]}, y[{y1[51]}] Predict => ',p1.predict(X1[51]))
