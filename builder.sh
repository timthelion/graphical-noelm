#!/bin/bash
echo "Paste your program here and press Control-D to build."
cat > program.elm
elm program.elm
firefox build/program.html
