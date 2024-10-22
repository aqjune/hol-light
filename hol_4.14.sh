#!/bin/bash

# Makefile will replace __DIR__ with the path
export HOLLIGHT_DIR=__DIR__

if [ "$#" -eq 1 ]; then
  if [ "$1" == "-pp" ]; then
    echo "camlp5r pa_lexer.cmo pa_extend.cmo q_MLast.cmo -I "__DIR__" pa_j.cmo"
    exit 0
  elif [ "$1" == "-dir" ]; then
    echo "${HOLLIGHT_DIR}"
    exit 0
  fi
fi

# The default ocaml REPL does not accept arrow keys.
# Export LINE_EDITOR to a proper program to enable this before invoking this
# script. If not set, ledit will be used.
if [ "${LINE_EDITOR}" == "" ]; then
  LINE_EDITOR="ledit"
fi

export HOLLIGHT_USE_MODULE=__USE_MODULE__

# If a local OPAM is installed, use it
if [ -d "${HOLLIGHT_DIR}/_opam" ]; then
  eval $(opam env --switch "${HOLLIGHT_DIR}/" --set-switch)
fi

${LINE_EDITOR} ${HOLLIGHT_DIR}/ocaml-hol -init ${HOLLIGHT_DIR}/hol.ml -I ${HOLLIGHT_DIR}
