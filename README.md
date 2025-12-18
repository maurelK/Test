# MY_TORCH — Neural Network Library & Chessboard Analyzer  
**EPITECH – G-FUN-500 – 2025**

---

##  Introduction

MY_TORCH is a full machine-learning project implementing:
    1. A custom neural network module (no PyTorch / TensorFlow allowed)
    2. A chessboard analyzer executable capable of:
        Training a neural network using supervised learning
        Predicting the state of a chessboard (from a FEN string)
The goal is to design, implement, train, and benchmark an artificial neural network from scratch, then evaluate how well it recognizes chess game states.

MY_TORCH is a project composed of **two main parts**:

1. **A neural network module (NN module)**

The NN module is a standalone library responsible for:

✔ Creating a new neural network
✔ Training a neural network
✔ Saving/loading neural networks
✔ Generating random networks from configuration files
✔ Providing all tools needed by the analyzer
No external ML libraries are allowed. chessboard analysis executable (`my_torch_analyzer`)**

- In **train** mode, it trains a neural network on a dataset of annotated chess positions.
- In **predict** mode, it predicts the state of a given chessboard in FEN:
- `Checkmate`
- `Check`
- `Nothing`
    
### 1.1 Neural Network Features

The neural network must support:
Fully-connected layers
Activation functions (ReLU, Sigmoid, Tanh…)
Forward propagation
Backpropagation
Cost function (MSE or Cross-Entropy)
Weight initialization (Xavier / He-Init recommended)
Gradient descent or mini-batch SGD


### 1.2 Configuration Files

```
./my_torch_generator config_file.conf 3
```

```
basic_network_1.nn
basic_network_2.nn
basic_network_3.nn
```

### 1.3 Saving / Loading Networks

You choose the file format (.nn): JSON, binary, or structured txt.

The file must contain everything needed:
number of layers
layer sizes
activations
weights
biases
learning parameters

---

## 2. Analyzer Executable

The program must handle two modes:
    
## 2.1 Prediction Mode

```
./my_torch_analyzer --predict my_torch_network.nn chessboards.txt
```

```
FEN_STRING
```

### Output must be one of the following:

* `Checkmate`
* `Check`
* `Nothing`
* `Bonus: Check White, Check Black, Checkmate White, Checkmate Black`

```
Checkmate Black
Nothing
Nothing
Check
Nothing
```
## 2.2 Training Mode

```
./my_torch_analyzer --train --save new_network.nn my_network.nn training_data.txt
```

Each line of the training file contains:

```
FEN_STRING EXPECTED_LABEL
```
Labels can be encoded as:

```
Nothing = 0
Check = 1
Checkmate = 2
```
or using one-hot encoding.

After training:
* `If --save is present → save to the specified file`
* `Else overwrite the original file`
* `Training must avoid overfitting (regularization, dropout, shuffling, mini-batches).`

## 3. Chessboard Input (FEN)

All chessboards use Forsyth–Edwards Notation.
We must implement a parser that converts a FEN position into a numeric input vector for the neural network.

```
rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 1 3
```

## 3.1 Input Encoding Examples

Recommended encodings:

Option A — One-hot piece encoding (best quality)
12 channels (6 white, 6 black), 64 squares ⇒ 768 inputs.
Option B — Integer encoding (simpler)
Each square = integer representing piece type.

---

## 4. Training Strategy

Training must be supervised.

✔ Feed FEN examples
✔ Provide expected labels
✔ Update weights via backpropagation
✔ Optimize with SGD / mini-batch / momentum

### 4.1 Avoid Overfitting

Project explicitly requires this.

Recommended techniques:
Shuffle training data
Mini-batch training
Weight decay (L2 regularization)
Dropout (MC Dropout accepted)
Early stopping