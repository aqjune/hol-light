eval $(opam env)
ocamlfind ocamlc -linkpkg pa_j.cmo holLoader.ml hol.ml fusion.ml -package compiler-libs.toplevel,camlp5 -syntax camlp5o
