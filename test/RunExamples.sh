#!/bin/bash
# Build Naproche and run it on all examples.

# Change to parent directory.
cd ..
:

# Build Naproche.
stack build

# Run examples.
for FILE in examples/*.ftl
do
  stack exec Naproche-SAD -- "$FILE"
done