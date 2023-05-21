#!/bin/bash

while read -r line; do
    if [[ ${#line} -gt 2 ]]; then
        echo ", \"$line\"" >> filteredWordList.txt
    fi
done < wordList.txt
