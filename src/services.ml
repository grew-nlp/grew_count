(* open Printf *)

(* open Amr *)
open Conll
(* open Grewlib *)

(* open Utils *)

let config = Conll_config.build "basic"

let count _corpora_string _requests_string = 
  `Int 4242