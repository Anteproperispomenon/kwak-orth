#!/bin/bash
cd $(dirname "$0")
echo "Enter file name or drag and drop file."
read fnm
./kwak-orth-exe -i "$fnm" --from U --to N
exit 0