#!/bin/bash

sources=$(find . -name '*.scala')
for file in $(find . -name '*.scala'); do
    classname=$(basename "$file" .scala)
    if ! egrep "(class|trait|object) $classname" "$file" &>/dev/null; then
        echo $file
    fi
done

