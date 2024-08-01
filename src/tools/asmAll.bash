# asmAll.bash: assemble all programs in Examples
# Usage (in Sigma16/src directory): bash tools/asmAll.bash

for file in ../Examples/*/*/*.asm.txt
do
    d=$(dirname "$file")
    f=$(basename "$file" ".asm.txt")
    (cd ./$(dirname "$file") ; echo "$d/$f"; ${RUNSIGMA16} assemble $f)
done
