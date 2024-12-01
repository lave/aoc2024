#!/bin/sh

SRC_FILE=${1:?Haskell source file must be specified as the first argument}
BIN_FILE=${SRC_FILE%%.hs}
OBJ_FILE=${SRC_FILE%%.hs}.o
INPUT_FILE=${2:-${SRC_FILE%%.hs}.input}

if [ "$SRC_FILE" -nt "$BIN_FILE" ]; then
    echo Compiling...
    ghc -Wno-x-partial "$SRC_FILE"
    rm "$OBJ_FILE"
fi

"./$BIN_FILE" "$INPUT_FILE"
