#!/bin/bash
set -e

# Fonction pour tester une image
test_image() {
    local lang=$1
    local image=$2
    local app_dir=$3

    echo "=============================="
    echo "Testing $lang..."
    echo "=============================="
    docker build -t $image -f images/$lang/Dockerfile.standalone $app_dir
    docker run --rm $image
    echo "$lang OK ✅"
    echo
}

echo "=============================="
echo "Testing all Whanos example apps"
echo "=============================="

# Tester Befunge
test_image "befunge" "whanos-befunge" "./whanos_example_apps/befunge-hello-world"

# Tester Python
test_image "python" "whanos-python" "./whanos_example_apps/python-hello-world"

# Tester C
test_image "c" "whanos-c" "./whanos_example_apps/c-hello-world"

# Tester Java
test_image "java" "whanos-java" "./whanos_example_apps/java-hello-world"

# Tester JavaScript
test_image "javascript" "whanos-javascript" "./whanos_example_apps/js-hello-world"

