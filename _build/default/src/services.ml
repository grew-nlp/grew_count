open Printf

open Amr
open Conll
open Grewlib

open Utils

let config = Conll_config.build "basic"

let file_of_dot dot =
  let (temp_file_name,out_ch) =
  Filename.open_temp_file
    ~mode:[Open_rdonly;Open_wronly;Open_text] "draw_" ".dot" in
  fprintf out_ch "%s" dot;
  close_out out_ch;
  let file = sprintf "%s.svg" (uid ()) in
  let filename = Filename.concat (Dream_config.get_string "static_dir") file in
  let command = sprintf "dot -Tsvg -o %s %s " filename temp_file_name in
  match Sys.command command with 
  | 0 -> `String (Filename.concat (Dream_config.get_string "static_url") file)
  | n -> raise (Error (sprintf "Fail to run (code=%d) command `%s`" n command))

let amr penman =
  penman
  |> Amr.parse
  |> Amr.to_json
  |> Graph.of_json
  |> Graph.to_dot ~config
  |> file_of_dot

let sbn sbn_string =
  sbn_string
  |> Sbn.to_json
  |> Graph.of_json
  |> Graph.to_dot ~config
  |> file_of_dot

let count _corpora_string _requests_string = 
  `Int 4242