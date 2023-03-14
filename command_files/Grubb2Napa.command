#!/bin/bash
cd $(dirname "$0")
echo "Enter file name or drag and drop file."
read fnm
./kwak-orth-exe -i "$fnm" --from G --to N
exit 0