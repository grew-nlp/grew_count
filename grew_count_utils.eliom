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
        ) ["log"; "grew_match_config_dir"] in

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

type corpus = {
  directory: string;
  config: string;
}

let current : corpus String_map.t ref = ref String_map.empty

let grew_match_list =
  let grew_match_config_dir = get_global "grew_match_config_dir" in
  let open Yojson.Basic.Util in
  let list_file = Filename.concat grew_match_config_dir "grew_match.list" in
  let json_list = CCIO.(with_in list_file read_lines_l) in
  List.iter
    (fun json_file ->
       let json = Yojson.Basic.from_file (Filename.concat grew_match_config_dir json_file) in
       List.iter (
         fun json ->
           let id = json |> member "id" |> to_string in
           let corpus = {
             directory = json |> member "directory" |> to_string;
             config = try json |> member "config" |> to_string with Type_error _ -> "no_config"
           } in
           current := String_map.add id corpus !current
       ) (json |> member "corpora" |> to_list)
    ) json_list;
  printf "--> %d corpora found\n%!" (String_map.cardinal !current)

let buff = Buffer.create 32

let config = Conllx_config.build "sud"

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
      try
        patterns
      |> to_assoc
      |> List.map
        (fun (id, json) ->
           try
             (id, json |> to_string |> Pattern.parse ~config)
           with
           | Type_error _ -> raise (Error (sprintf "Error in pattern `%s`: not a JSON string" id))
           | Libgrew.Error msg -> raise (Error (sprintf "Error in pattern `%s`: `%s`" id msg))
        )
        with Type_error _ -> raise (Error "patterns POST arg must be a dictionary") in

    bprintf buff "Corpus\t# sentences";
    List.iter (fun (id,_) -> bprintf buff "\t%s" id) pattern_list;
    bprintf buff "\n";

    List.iter (
      fun corpus_id  ->
        try
          match String_map.find_opt corpus_id  !current with
          | None -> raise (Error (sprintf "Unknown corpus `%s`" corpus_id ))
          | Some corpus ->
            let marshal_file = Filename.concat corpus.directory (corpus_id  ^ ".marshal") in
            let in_ch = open_in_bin marshal_file in
            let data = (Marshal.from_channel in_ch : Corpus.t) in
            let _ = close_in in_ch in

            bprintf buff "%s" corpus_id ;
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
        with Sys_error _ -> raise (Error (sprintf "Can't load corpus `%s`, please report to Bruno.Guillaume@loria.fr " corpus_id ))
    ) corpus_list;
    Buffer.contents buff
  with
  | Error e -> sprintf "ERROR: %s" e
  | Yojson.Json_error msg -> sprintf "ERROR: Incorrect json data `%s`" msg
  | exc -> sprintf "ERROR: unexpected exception `%s`" (Printexc.to_string exc)
