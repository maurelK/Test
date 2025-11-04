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
echo "  Building Whanos Base and Standalone Images"
echo "================================================"
echo -e "${NC}"

LANGUAGES=("befunge" "c" "java" "javascript" "python")

# Vérifier que le répertoire images existe
if [ ! -d "images" ]; then
    echo -e "${RED}ERROR: images/ directory not found${NC}"
    echo "Please run this script from the root of the Whanos repository"
    exit 1
fi

SUCCESS_COUNT=0
FAIL_COUNT=0

for lang in "${LANGUAGES[@]}"; do
    echo ""
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    echo -e "${BLUE}Building images for: ${YELLOW}${lang}${NC}"
    echo -e "${BLUE}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${NC}"
    
    # Vérifier que le répertoire existe
    if [ ! -d "images/$lang" ]; then
        echo -e "${RED}ERROR: images/$lang/ directory not found${NC}"
        FAIL_COUNT=$((FAIL_COUNT + 1))
        continue
    fi
    
    # Vérifier que les Dockerfiles existent
    if [ ! -f "images/$lang/Dockerfile.base" ]; then
        echo -e "${RED}ERROR: images/$lang/Dockerfile.base not found${NC}"
        FAIL_COUNT=$((FAIL_COUNT + 1))
        continue
    fi
    
    # ✅ CORRECTION: Vérifier Dockerfile.standalone
    if [ ! -f "images/$lang/Dockerfile.standalone" ]; then
        echo -e "${RED}ERROR: images/$lang/Dockerfile.standalone not found${NC}"
        FAIL_COUNT=$((FAIL_COUNT + 1))
        continue
    fi
    
    cd images/$lang
    
    # Build base image
    echo -e "${YELLOW}[1/2]${NC} Building base image: whanos-$lang:base"
    if ! docker build -t whanos-$lang:base -f Dockerfile.base ../.. 2>&1 | tail -5; then
        echo -e "${RED}✗ Failed to build whanos-$lang:base${NC}"
        FAIL_COUNT=$((FAIL_COUNT + 1))
        cd ../..
        continue
    fi
    echo -e "${GREEN}✓ whanos-$lang:base built successfully${NC}"
    
    # Build standalone image - ✅ CORRECTION: Utiliser Dockerfile.standalone !
    echo -e "${YELLOW}[2/2]${NC} Building standalone image: whanos-$lang"
    if ! docker build -t whanos-$lang -f Dockerfile.standalone ../.. 2>&1 | tail -5; then
        echo -e "${RED}✗ Failed to build whanos-$lang${NC}"
        FAIL_COUNT=$((FAIL_COUNT + 1))
        cd ../..
        continue
    fi
    echo -e "${GREEN}✓ whanos-$lang built successfully${NC}"
    
    cd ../..
    
    SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
    echo -e "${GREEN}✓ $lang images built successfully${NC}"
done

echo ""
echo -e "${BLUE}================================================${NC}"
echo -e "${BLUE}Build Summary${NC}"
echo -e "${BLUE}================================================${NC}"
echo -e "  ${GREEN}Success: $SUCCESS_COUNT${NC}"
echo -e "  ${RED}Failed:  $FAIL_COUNT${NC}"
echo ""

if [ $FAIL_COUNT -eq 0 ]; then
    echo -e "${GREEN}✓ All Whanos images built successfully!${NC}"
    echo ""
    echo -e "${BLUE}Available images:${NC}"
    docker images | grep -E "whanos-|REPOSITORY" | grep -v "<none>"
    echo ""
    exit 0
else
    echo -e "${RED}✗ Some images failed to build${NC}"
    exit 1
fi