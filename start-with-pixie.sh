#!/bin/sh
SOLITAIR_PATH="$(pwd)/src"
pixie -l "$SOLITAIR_PATH" "$SOLITAIR_PATH/solitaire/main.pxi" $*
