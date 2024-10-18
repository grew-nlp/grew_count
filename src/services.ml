open Printf

(* open Amr *)
open Conll
open Grewlib

open Utils

let config = Conll_config.build "basic"

let corpusbank = ref None
let load_corpusbank () =
  corpusbank := Some (Corpusbank.load (Dream_config.get_string "corpusbank"))

let get_corpus_desc corpus_id = 
  match !corpusbank with
  | None -> error "Corpusbank not loaded"
  | Some cb ->  
    match Corpusbank.get_corpus_desc_opt cb corpus_id with
    | None -> error "Cannot find corpus `%s`" corpus_id
    | Some c -> c

let buff = Buffer.create 32

let count corpora_string requests_string =
  let open Yojson.Basic.Util in

  let corpora = Yojson.Basic.from_string corpora_string in
  let requests = Yojson.Basic.from_string requests_string in

  Buffer.clear buff;

  let corpus_list =
    try corpora |> to_list |> List.map to_string
    with Type_error _ -> error "POST argument `corpora` must be list of strings" in

  let request_list =
    try
      requests
      |> to_assoc
      |> List.map
        (fun (id, json) ->
           try (id, json |> to_string |> Request.parse ~config)
           with
           | Type_error _ -> error "Error in request `%s`: not a JSON string" id
           | Grewlib.Error msg -> error "Error in request `%s`: `%s`" id msg
        )
    with Type_error _ -> raise (Error "requests POST arg must be a dictionary") in

  bprintf buff "Corpus\t# sentences";
  List.iter (fun (id,_) -> bprintf buff "\t%s" id) request_list;
  bprintf buff "\n";

  List.iter (
    fun corpus_id ->
      let corpus_desc = get_corpus_desc corpus_id in
      let corpus = match Corpus_desc.load_corpus_opt corpus_desc with
      | Some c -> c
      | None -> Corpus_desc.build_corpus corpus_desc in
        bprintf buff "%s" corpus_id ;
        bprintf buff "\t%d" (Corpus.size corpus);
        List.iter
          (fun (_,request) ->
            let count =
              Corpus.fold_left (fun acc _ graph ->
                acc + (List.length (Matching.search_request_in_graph ~config request graph))
              ) 0 corpus in
            bprintf buff "\t%d" count
          ) request_list;
        bprintf buff "\n%!"
    ) corpus_list;
  Buffer.contents buff

