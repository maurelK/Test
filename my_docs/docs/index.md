#  Welcome to My_Torch Documentation 

**From-Scratch Neural Network for Chess Position Evaluation**

> **Version**: 2.5.0  
> **Binary name**: `my_torch_analyzer`  
> **Language**: C++20, C, Python (or any language that works with "the dump" – compiled via Makefile)  
> **Pre-trained model**: `my_torch_network_full.nn` (included – mandatory naming convention)  
> **Delivery notes**: All source files included (no binaries, temps, objs). Bonus in `./bonus/`. Error messages to stderr, exit 84 on error.  

---

### Project Overview

This project builds a **machine learning framework from scratch** (no PyTorch/TensorFlow) for supervised learning, applied to chess position evaluation. It detects states like "Nothing", "Check", "Checkmate", and (bonus) the winning side or "Stalemate" from FEN notation.

- **Mandatory**: Supervised learning NN trained on labeled chess data.  
- **NN module**: Generic library for creating, training, saving/loading NNs.  
- **Analyzer**: Executable for training or predicting chess states.  
- **Benchmarking**: Professional documentation with visual proofs (see `docs/BENCHMARKS.md`).  
- **Datasets**: All training scripts/datasets kept (available on demand – not pushed due to size).  

**No external NN libraries allowed – everything custom-built.**

---

### Setup & Compilation
