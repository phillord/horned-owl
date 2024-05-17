#!/bin/bash

branch=$(git branch --show-current)
if [ "$branch" = "rfc" ]; then
    exit 0
fi
