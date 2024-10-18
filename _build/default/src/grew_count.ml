open Utils
open Services

let home_route = 
  Dream.get "/" (fun _ -> Dream.html "<h1>Welcome to grew_count!</h1>")

let ping_route = 
  Dream.get "/ping" (fun _ -> Dream.html "")

let static_route =
  Dream.get "/static/**" (Dream.static "static")

let count_route =
  Dream.post "count"
  (fun request ->
    match%lwt Dream.form ~csrf:false request with
    | `Ok param ->
      let corpora_string = List.assoc "corpora" param  in
      let requests_string = List.assoc "requests" param  in
      let json = wrap (count corpora_string) requests_string in
      Log.info "<count> corpora=[%s] requests=[%s] ==> %s" corpora_string requests_string (report_status json);
      Dream.respond (Yojson.Basic.pretty_to_string json)
    | _ -> Dream.empty `Bad_Request
  )

let all_routes = [
  home_route;
  ping_route;
  static_route;
  count_route;
]

let _ =
  let required = [ "port"] in
  Dream_config.load ~required ();
  Log.init();
  Dream.run
    ~error_handler:Dream.debug_error_handler
    ~port: (Dream_config.get_int "port")
  @@ Dream.logger
  @@ Dream.router all_routes
