(* ========================================================================= *)
(*                               HOL LIGHT                                   *)
(*                                                                           *)
(*              Modern OCaml version of the HOL theorem prover               *)
(*                                                                           *)
(*                            John Harrison                                  *)
(*                                                                           *)
(*            (c) Copyright, University of Cambridge 1998                    *)
(*              (c) Copyright, John Harrison 1998-2007                       *)
(* ========================================================================= *)

let hol_version = "2.20++";;

#directory "+compiler-libs";;

(* ------------------------------------------------------------------------- *)
(* Should eventually change to "ref(Filename.temp_dir_name)".                *)
(* However that's not available in 3.08, which is still the default          *)
(* in Cygwin, and I don't want to force people to upgrade Ocaml.             *)
(* ------------------------------------------------------------------------- *)

let temp_path = ref "/tmp";;

#use "hol_loader.ml";;

file_loader := fun s -> Toploop.use_file Format.std_formatter s;;
(* Hide the definition of file_loader of hol_loader.ml.                      *)
let file_loader = ();;

(* ------------------------------------------------------------------------- *)
(* Load in parsing extensions.                                               *)
(* For Ocaml < 3.10, use the built-in camlp4                                 *)
(* and for Ocaml >= 3.10, use camlp5 instead.                                *)
(* For Ocaml >= 4.14, use ocamlfind to locate camlp5.                        *)
(* ------------------------------------------------------------------------- *)

let ocaml_version = String.sub Sys.ocaml_version 0 4;;
let version_ge_3_10 = ocaml_version >= "3.10";;
let version_ge_4_14 = ocaml_version >= "4.14";;

if version_ge_4_14
then loads "load_camlp5_topfind.ml"
else if version_ge_3_10
then loads "load_camlp5.ml"
else loads "load_camlp4.ml";;

Topdirs.dir_load Format.std_formatter (Filename.concat (!hol_dir) "pa_j.cmo");;

(*
To run HOL Light without precompiled hol_lib module:

Topdirs.dir_load Format.std_formatter
    (Filename.concat (!hol_dir) "bignum.cmo");;
loads "hol_lib.ml";;
*)

Topdirs.dir_load Format.std_formatter
    (Filename.concat (!hol_dir) "hol_lib.cma");;

include Hol_lib;;

(* ------------------------------------------------------------------------- *)
(* Install all the printers.                                                 *)
(* ------------------------------------------------------------------------- *)

(* bignum.ml *)
#install_printer pp_print_num;;

(* lib.ml *)
#install_printer pp_print_fpf;;

(* printer.ml *)
#install_printer pp_print_qtype;;
#install_printer pp_print_qterm;;
#install_printer pp_print_thm;;

(* tactics.ml *)
#install_printer pp_print_goal;;
#install_printer pp_print_goalstack;;

(* ------------------------------------------------------------------------- *)
(* The help system.                                                          *)
(* ------------------------------------------------------------------------- *)

loads "help.ml";;       (* Online help using the entries in Help directory   *)
loads "database.ml";;   (* List of name-theorem pairs for search system      *)
