#!/bin/bash

# The default ocaml REPL does not accept arrow keys.
# Export LINE_EDITOR to a proper program to enable this before invoking this
# script. If not set, ledit will be used.
if [ "${LINE_EDITOR}" == "" ]; then
  LINE_EDITOR="ledit"
fi

# Makefile will replace __DIR__ with the path
export HOLLIGHT_DIR=__DIR__

# If a local OPAM is installed, use it
if [ -d "${HOLLIGHT_DIR}/_opam" ]; then
  eval $(opam env --switch "${HOLLIGHT_DIR}/" --set-switch)
fi

${LINE_EDITOR} ocamlnat -I `camlp5 -where` -init ${HOLLIGHT_DIR}/hol.ml
