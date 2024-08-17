# asmAll.bash: assemble all programs in Examples
# Usage (in Sigma16/src directory): bash tools/asmAll.bash

# E.g. Examples/Core/Arrays/*.asm.txt
echo 'Processing Examples/*/*/*.asm.txt'
for file in ../Examples/*/*/*.asm.txt
do
    d=$(dirname "$file")
    f=$(basename "$file" ".asm.txt")
    (cd ./$(dirname "$file") ; echo "$d/$f"; ${RUNSIGMA16} assemble $f)
done

# E.g. Examples/Standard/Testing/LinkTestB/*.asm.txt
echo 'Processing Examples/*/*/*/*.asm.txt'
for file in ../Examples/*/*/*/*.asm.txt
do
    d=$(dirname "$file")
    f=$(basename "$file" ".asm.txt")
    (cd ./$(dirname "$file") ; echo "$d/$f"; ${RUNSIGMA16} assemble $f)
done
