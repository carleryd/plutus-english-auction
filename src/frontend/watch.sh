fswatch -o src/* |
    while read -r filename event; do
        ./build.sh
    done
