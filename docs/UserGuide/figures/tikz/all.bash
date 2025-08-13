for file in ./*.tex
do
    f=$(basename "$file" .tex)
    echo "$f"
    make "$f.eps"
    make "$f.svg"
done
