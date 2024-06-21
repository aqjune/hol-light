if Array.length Sys.argv <> 3 then
  (Printf.printf "ocaml inline_load.ml <input .ml file> <output .ml file>\n"; exit 1);;

let filename = Sys.argv.(1);;
let fout = open_out (Sys.argv.(2));;

#use "hol_loader.ml";;
Printf.printf "hol_dir: %s\n" !hol_dir;;

let parse_load_statement fnname stmt: (string * string) option =
  let stmt = String.trim stmt in
  if not (String.starts_with ~prefix:fnname stmt) then None else
  let n = String.length fnname in
  let stmt = String.trim (String.sub stmt n (String.length stmt - n)) in
  if not (stmt.[0] = '"') then None else
  let stmt = String.sub stmt 1 (String.length stmt - 1) in
  let idx = String.index stmt '"' in
  if idx = -1 then None else
  let path = String.sub stmt 0 idx in
  let stmt = String.sub stmt (idx + 1) (String.length stmt - idx - 1) in
  let stmt = String.trim stmt in
  if not (String.starts_with ~prefix:";;" stmt) then None else
  let stmt = String.sub stmt 2 (String.length stmt - 2) in
  Some (path,stmt);;

let strings_of_file filename =
  let fd =
    try open_in filename
    with Sys_error _ -> failwith("strings_of_file: can't open "^filename) in
  let rec suck_lines acc =
    let l = try [input_line fd] with End_of_file -> [] in
     if l = [] then List.rev acc else suck_lines(List.hd l::acc) in
  let data = suck_lines [] in
  (close_in fd; data);;

file_loader := fun filename ->
  let lines = strings_of_file filename in
  List.iter
    (fun line ->
      let open Printf in
      match parse_load_statement "loadt" line with
      | Some (path,line') -> loadt path; fprintf fout "%s\n" line' | None ->
      (match parse_load_statement "loads" line with
      | Some (path,line') -> loads path; fprintf fout "%s\n" line' | None ->
      (match parse_load_statement "needs" line with
      | Some (path,line') -> needs path; fprintf fout "%s\n" line'
      | None -> fprintf fout "%s\n" line (* no linebreak needed! *))))
    lines;
  true;;

loadt filename;;

close_out fout;;
