##
## EPITECH PROJECT, 2024
## Zappy
## File description:
## Makefile for Zappy AI
##

# Variables
PYTHON = python3
BINARY_NAME = zappy_ai
MAIN_FILE = main.py
CONFIG_FILE = config.py

# Couleurs pour les messages
GREEN = \033[0;32m
RED = \033[0;31m
BLUE = \033[0;34m
YELLOW = \033[1;33m
NC = \033[0m # No Color

# Cibles par défaut
.PHONY: all clean fclean re test help install check

all: $(BINARY_NAME)
	@echo "$(GREEN)✅ Zappy AI ready!$(NC)"

# Créer l'exécutable Python
$(BINARY_NAME): $(MAIN_FILE)
	@echo "$(BLUE)🔧 Creating executable...$(NC)"
	@cp $(MAIN_FILE) $(BINARY_NAME)
	@chmod +x $(BINARY_NAME)
	@echo "$(GREEN)✅ Executable $(BINARY_NAME) created!$(NC)"

# Vérifier la syntaxe Python
check: $(MAIN_FILE) $(CONFIG_FILE)
	@echo "$(BLUE)🔍 Checking Python syntax...$(NC)"
	@$(PYTHON) -m py_compile $(MAIN_FILE) || (echo "$(RED)❌ Syntax error in $(MAIN_FILE)$(NC)" && exit 1)
	@if [ -f $(CONFIG_FILE) ]; then \
		$(PYTHON) -m py_compile $(CONFIG_FILE) || (echo "$(RED)❌ Syntax error in $(CONFIG_FILE)$(NC)" && exit 1); \
	fi
	@echo "$(GREEN)✅ Python syntax OK!$(NC)"

# Installer les dépendances (si nécessaire)
install:
	@echo "$(BLUE)📦 Installing Python dependencies...$(NC)"
	@$(PYTHON) -m pip install --user argparse || echo "$(YELLOW)⚠️  argparse already available$(NC)"
	@echo "$(GREEN)✅ Dependencies installed!$(NC)"

# Tests basiques
test: $(BINARY_NAME)
	@echo "$(BLUE)🧪 Running basic tests...$(NC)"
	@echo "$(YELLOW)Testing help message:$(NC)"
	@./$(BINARY_NAME) -help || echo "$(RED)❌ Help test failed$(NC)"
	@echo "$(GREEN)✅ Basic tests passed!$(NC)"

# Test avec serveur (optionnel)
test-server: $(BINARY_NAME)
	@echo "$(BLUE)🔧 Testing with server...$(NC)"
	@echo "$(YELLOW)Make sure your server is running on localhost:4242$(NC)"
	@echo "$(YELLOW)Starting AI in 3 seconds...$(NC)"
	@sleep 3
	@./$(BINARY_NAME) -p 4242 -n test_team -h localhost || echo "$(RED)❌ Server test failed$(NC)"

# Nettoyer les fichiers temporaires
clean:
	@echo "$(BLUE)🧹 Cleaning temporary files...$(NC)"
	@rm -f *.pyc
	@rm -f __pycache__/*.pyc
	@rm -rf __pycache__/
	@rm -f .pytest_cache
	@echo "$(GREEN)✅ Cleaned!$(NC)"

# Nettoyer tout (incluant l'exécutable)
fclean: clean
	@echo "$(BLUE)🗑️  Removing executable...$(NC)"
	@rm -f $(BINARY_NAME)
	@echo "$(GREEN)✅ Full clean completed!$(NC)"

# Tout reconstruire
re: fclean all

# Debug mode - lance avec plus de verbosité
debug: $(BINARY_NAME)
	@echo "$(BLUE)🐛 Debug mode enabled$(NC)"
	@echo "$(YELLOW)Usage: make debug-run ARGS='-p 4242 -n team1 -h localhost'$(NC)"

debug-run: $(BINARY_NAME)
	@echo "$(BLUE)🚀 Running in debug mode...$(NC)"
	@$(PYTHON) -u $(MAIN_FILE) $(ARGS)

lint: $(MAIN_FILE)
	@echo "$(BLUE)📊 Checking code quality...$(NC)"
	@$(PYTHON) -m pyflakes $(MAIN_FILE) 2>/dev/null || echo "$(YELLOW)⚠️  Install pyflakes for better linting$(NC)"
	@echo "$(GREEN)✅ Lint check completed!$(NC)"

package: fclean all
	@echo "$(BLUE)📦 Creating package...$(NC)"
	@mkdir -p zappy_ai_package
	@cp $(BINARY_NAME) zappy_ai_package/
	@cp $(MAIN_FILE) zappy_ai_package/
	@if [ -f $(CONFIG_FILE) ]; then cp $(CONFIG_FILE) zappy_ai_package/; fi
	@cp Makefile zappy_ai_package/
	@tar -czf zappy_ai.tar.gz zappy_ai_package/
	@rm -rf zappy_ai_package/
	@echo "$(GREEN)✅ Package created: zappy_ai.tar.gz$(NC)"

# Afficher l'aide
help:
	@echo "$(BLUE)🔧 Zappy AI Makefile Commands:$(NC)"
	@echo ""
	@echo "$(GREEN)Building:$(NC)"
	@echo "  make all      - Create the zappy_ai executable"
	@echo "  make re       - Clean and rebuild everything"
	@echo ""
	@echo "$(GREEN)Testing:$(NC)"
	@echo "  make check    - Check Python syntax"
	@echo "  make test     - Run basic tests"
	@echo "  make test-server - Test with running server"
	@echo ""
	@echo "$(GREEN)Development:$(NC)"
	@echo "  make debug    - Show debug usage"
	@echo "  make debug-run ARGS='...' - Run with custom arguments"
	@echo "  make lint     - Check code quality"
	@echo ""
	@echo "$(GREEN)Maintenance:$(NC)"
	@echo "  make clean    - Remove temporary files"
	@echo "  make fclean   - Remove all generated files"
	@echo "  make install  - Install Python dependencies"
	@echo "  make package  - Create distribution package"
	@echo ""
	@echo "$(GREEN)Examples:$(NC)"
	@echo "  make debug-run ARGS='-p 4242 -n team1 -h localhost'"
	@echo "  make test-server"

.DEFAULT_GOAL := all