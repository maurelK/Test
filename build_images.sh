#!/bin/bash
set -e

LANGUAGES=("c" "java" "javascript" "python" "befunge")

for lang in "${LANGUAGES[@]}"; do
  echo "Building base image for $lang..."
  docker build -t whanos-$lang:base -f images/$lang/Dockerfile.base .

  echo "Building standalone image for $lang..."
  docker build -t whanos-$lang -f images/$lang/Dockerfile.standalone .
done

echo "All images built successfully!"
