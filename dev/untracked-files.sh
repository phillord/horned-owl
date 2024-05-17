#!/bin/bash
set -eu

files=$(git ls-files --other --directory --exclude-standard)
if [ "$files" ]; then
    echo There are untracked files: $files
    exit 1
fi
