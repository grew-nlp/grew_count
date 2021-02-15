open Printf
open Conllx
open Libgrew

exception Error of string

let corpus_dir =
  match Unix.gethostname () with
  | "tromboline.local" | "tromboline" -> "/users/guillaum/resources/ud-treebanks-v2.7"
  | "grew.atilf.fr" -> "/home/guillaum/resources/ud-treebanks-v2.7"
  | s -> failwith ("unknown hostname: " ^ s)

let buff = Buffer.create 32
let config = Conllx_config.build "ud"

let count corpora_string patterns_string =
  try
    let corpora = Yojson.Basic.from_string corpora_string in
    let patterns = Yojson.Basic.from_string patterns_string in

    let open Yojson.Basic.Util in
    Buffer.clear buff;

    let corpus_list =
      try corpora |> to_list |> List.map to_string
      with Type_error _ -> raise (Error "POST argument `corpora` must be list of strings") in

    let pattern_list =
      patterns
      |> (fun x -> try to_list x with Type_error _ -> raise (Error "POST argument `patterns` must be list"))
      |> List.map
        (fun json ->
           let id =
             try json |> member "id" |> to_string
             with Type_error _ -> raise (Error "missing `id` field in pattern descrition") in
           let string_pattern =
             try json |> member "pattern" |> to_string
             with Type_error _ -> raise (Error "missing `pattern` field in pattern descrition") in
           let pattern =
             try Pattern.parse ~config string_pattern
             with Libgrew.Error msg -> raise (Error (sprintf "Error in pattern `%s`: `%s`" id msg)) in
           (id, pattern)
        ) in

    bprintf buff "Corpus\t# sentences";
    List.iter (fun (id,_) -> bprintf buff "\t%s" id) pattern_list;
    bprintf buff "\n";

    List.iter (
      fun corpus ->
        try
          let marshal_file = Filename.concat corpus_dir (Filename.concat corpus (corpus ^ "@2.7.marshal")) in
          let in_ch = open_in_bin marshal_file in
          let data = (Marshal.from_channel in_ch : Corpus.t) in
          let _ = close_in in_ch in

          bprintf buff "%s" corpus;
          bprintf buff "\t%d" (Corpus.size data);

          List.iter
            (fun (_,pattern) ->
               let count =
                 Corpus.fold_left (fun acc _ graph ->
                     acc + (List.length (Graph.search_pattern ~config pattern graph))
                   ) 0 data in
               bprintf buff "\t%d" count
            ) pattern_list;
          bprintf buff "\n%!"
        with Sys_error _ -> raise (Error (sprintf "Unknown corpus `%s`" corpus))
    ) corpus_list;
    Buffer.contents buff
  with
  | Error e -> sprintf "ERROR: %s" e
  | Yojson.Json_error msg -> sprintf "ERROR: Incorrect json data `%s`" msg
  | exc -> sprintf "ERROR: unexpected exception `%s`" (Printexc.to_string exc)
