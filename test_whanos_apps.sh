#!/bin/bash
set -e

# Couleurs
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}"
echo "================================================"
echo "  Testing Whanos Example Applications"
echo "================================================"
echo -e "${NC}"

# Vérifier que les images de base existent
echo -e "${YELLOW}Checking base images...${NC}"
MISSING_IMAGES=0

for lang in befunge c java javascript python; do
    if ! docker images | grep -q "whanos-$lang"; then
        echo -e "${RED}✗ whanos-$lang image not found${NC}"
        MISSING_IMAGES=$((MISSING_IMAGES + 1))
    else
        echo -e "${GREEN}✓ whanos-$lang image found${NC}"
    fi
done

if [ $MISSING_IMAGES -gt 0 ]; then
    echo ""
    echo -e "${RED}ERROR: Some Whanos base images are missing${NC}"
    echo "Please run: ./build_images.sh"
    exit 1
fi

echo ""

# Fonction pour tester une application
test_app() {
    local lang=$1
    local app_dir=$2
    local image_name="test-whanos-$lang"

    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Testing: ${YELLOW}${lang}${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    
    # Vérifier que le répertoire existe
    if [ ! -d "$app_dir" ]; then
        echo -e "${RED}✗ Directory not found: $app_dir${NC}"
        return 1
    fi
    
    # Build l'image - ✅ CORRECTION: Utiliser Dockerfile.standalone !
    echo -e "${YELLOW}[1/2]${NC} Building image..."
    if ! docker build -t $image_name -f images/$lang/Dockerfile.standalone $app_dir; then
        echo -e "${RED}✗ Build failed${NC}"
        return 1
    fi
    echo -e "${GREEN}✓ Build successful${NC}"
    
    # Run l'application
    echo -e "${YELLOW}[2/2]${NC} Running application..."
    echo -e "${BLUE}Output:${NC}"
    echo "─────────────────────────────────────────────"
    
    # Pour JS et Python qui tournent en continu, timeout de 2 secondes
    if [[ "$lang" == "javascript" ]] || [[ "$lang" == "python" ]]; then
        timeout 2 docker run --rm $image_name 2>&1 || true
    else
        docker run --rm $image_name 2>&1
    fi
    
    echo "─────────────────────────────────────────────"
    echo -e "${GREEN}✓ $lang test completed${NC}"
    echo ""
    
    # Cleanup
    docker rmi $image_name >/dev/null 2>&1 || true
    
    return 0
}

# Tests
SUCCESS=0
FAILED=0

# Test Befunge
if test_app "befunge" "whanos_example_apps/befunge-hello-world"; then
    SUCCESS=$((SUCCESS + 1))
else
    FAILED=$((FAILED + 1))
fi

# Test C
if test_app "c" "whanos_example_apps/c-hello-world"; then
    SUCCESS=$((SUCCESS + 1))
else
    FAILED=$((FAILED + 1))
fi

# Test Java
if test_app "java" "whanos_example_apps/java-hello-world"; then
    SUCCESS=$((SUCCESS + 1))
else
    FAILED=$((FAILED + 1))
fi

# Test JavaScript
echo -e "${YELLOW}Note: JavaScript app will run for 2 seconds then timeout (normal behavior)${NC}"
if test_app "javascript" "whanos_example_apps/js-hello-world"; then
    SUCCESS=$((SUCCESS + 1))
else
    FAILED=$((FAILED + 1))
fi

# Test Python
echo -e "${YELLOW}Note: Python app will run for 2 seconds then timeout (normal behavior)${NC}"
if test_app "python" "whanos_example_apps/python-hello-world"; then
    SUCCESS=$((SUCCESS + 1))
else
    FAILED=$((FAILED + 1))
fi

# Summary
echo ""
echo -e "${BLUE}================================================${NC}"
echo -e "${BLUE}Test Summary${NC}"
echo -e "${BLUE}================================================${NC}"
echo -e "  ${GREEN}Passed: $SUCCESS${NC}"
echo -e "  ${RED}Failed: $FAILED${NC}"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}✓ All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}✗ Some tests failed${NC}"
    exit 1
fi