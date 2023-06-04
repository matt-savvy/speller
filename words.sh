#!/bin/bash

# Assumes new word list is words.txt in this dir
INPUT=words.txt

# Remove words less than 3 letters and greater than 10 letters
sed -i -r '/^.{3,10}$/!d' $INPUT
# Sort alphabetically
sort --output=$INPUT $INPUT

TARGET=src/Words.elm

# clear target file if it exists
if [[ -e $TARGET ]]; then
    rm $TARGET
fi

# add header
echo "module Words exposing (words)" > $TARGET
echo "words : List String" >> $TARGET
echo "words = [" >> $TARGET


# add lines as comma seperated
while read -r line; do
  echo "    ,\"$line\"" >> $TARGET
done <$INPUT

# # add closing bracket
echo "]" >> $TARGET

echo "$TARGET created"
echo "fix leading comma, then run \"elm-format --yes $TARGET\""
