##
## EPITECH PROJECT, 2025
## Makefile
## File description:
## Makefile for MY_TORCH
##

NAME = my_torch_analyzer

SRC = my_torch_analyzer.py my_torch_generator.py generate_networks.py test_neural_network.py my_torch/*.py

OBJ = $(SRC:.py=.pyc)

.PHONY: all clean fclean re test run generator analyzer networks binaries

all: binaries

binaries: my_torch_generator my_torch_analyzer

my_torch_generator: my_torch_generator.py
	@chmod +x my_torch_generator.py
	@ln -sf my_torch_generator.py my_torch_generator
	@echo "Created executable: my_torch_generator"

my_torch_analyzer: my_torch_analyzer.py
	@chmod +x my_torch_analyzer.py
	@ln -sf my_torch_analyzer.py my_torch_analyzer
	@echo "Created executable: my_torch_analyzer"

generator: my_torch_generator.py
	@echo "Generator ready"

analyzer: my_torch_analyzer.py
	@echo "Analyzer ready"

networks: generate_networks.py
	@python generate_networks.py

test: test_neural_network.py
	@echo "Running neural network test..."
	@python test_neural_network.py

clean:
	@rm -f $(OBJ)
	@find . -name "*.pyc" -delete
	@find . -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
	@rm -f my_torch_generator my_torch_analyzer

fclean: clean
	@rm -f $(NAME)
	@rm -f my_torch_network_*.nn

re: fclean all

run: analyzer
	@echo "Running analyzer..."
	@python my_torch_analyzer.py --help

demo: networks
	@echo "Running demo with test data..."
	@python my_torch_analyzer.py --predict my_torch_network_basic.nn test_chessboards.txt

help:
	@echo "Available targets:"
	@echo "  all       - Build binaries"
	@echo "  binaries  - Create executable symlinks"
	@echo "  generator - Prepare network generator"
	@echo "  analyzer  - Prepare analyzer"
	@echo "  networks  - Generate neural networks"
	@echo "  test      - Run neural network test"
	@echo "  clean     - Remove temp files and symlinks"
	@echo "  fclean    - Remove all generated files"
	@echo "  re        - Rebuild"
	@echo "  run       - Show help"
	@echo "  demo      - Run demo with test data"
	@echo "  help      - Show this help"
