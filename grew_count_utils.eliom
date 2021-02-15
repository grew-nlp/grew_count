open Printf
open Conllx
open Libgrew

module String_map = Map.Make (String)

exception Error of string

(* ================================================================================ *)
(* global variables *)
(* ================================================================================ *)
let (global : string String_map.t ref) = ref String_map.empty
let set_global key value = global := String_map.add key value !global
let get_global key =
  try String_map.find key !global
  with Not_found -> raise (Error (sprintf "Config error: global parameter `%s` is not set" key))

(* ================================================================================ *)
(* read_config *)
(* ================================================================================ *)
let read_config () =
  try
    let elements =
      List.map
        (fun item ->
           Ocsigen_extensions.Configuration.element
             ~name: item
             ~pcdata: (fun x -> printf " INFO:  ---> set `%s` config parameter to `%s`\n%!" item x; set_global item x)
             ()
        ) ["log"; "corpus_dir"] in

    Ocsigen_extensions.Configuration.process_elements
      ~in_tag:"eliommodule"
      ~elements
      (Eliom_config.get_config ())
  with
  | Error msg -> printf " ERROR: ================ Starting error: %s ================\n%!" msg; exit 0

(* ================================================================================ *)
(* Log *)
(* ================================================================================ *)
module Log = struct
  let out_ch = ref stdout

  let time_stamp () =
    let gm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "%02d_%02d_%02d_%02d_%02d_%02d"
      (gm.Unix.tm_year - 100)
      (gm.Unix.tm_mon + 1)
      gm.Unix.tm_mday
      gm.Unix.tm_hour
      gm.Unix.tm_min
      gm.Unix.tm_sec

  let init () =
    let basename = Printf.sprintf "grew_back_%s.log" (time_stamp ()) in
    let filename = Filename.concat (get_global "log") basename in
    out_ch := open_out filename

  let _info s = Printf.fprintf !out_ch "[%s] %s\n%!" (time_stamp ()) s
  let info s = Printf.ksprintf _info s
end

(* ================================================================================ *)
(* main *)
(* ================================================================================ *)

let _ = read_config ()
let _ = Log.init ()

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
          let marshal_file = Filename.concat (get_global "corpus_dir") (Filename.concat corpus (corpus ^ "@2.7.marshal")) in
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
