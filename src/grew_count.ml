open Utils
open Services

let home_route = 
  Dream.get "/" (fun _ -> Dream.html "<h1>Welcome to <code>grew_count</code> service!</h1><p>See <a href=\"https://grew.fr/usage/grew_count/\">https://grew.fr/usage/grew_count/</a> for documentation.</p>")

let ping_route =
  Dream.post "ping" (fun _ -> Dream.html ~headers:["Content-Type", "text/plain"] "{}")

let count_route =
  Dream.post "count"
  (fun request ->
    match%lwt Dream.form ~csrf:false request with
    | `Ok param ->
      let corpora_string = List.assoc "corpora" param  in
      let requests_string = List.assoc "requests" param  in
      let response = wrap (count corpora_string) requests_string in
      Log.info "<count> corpora=[%s] requests=[%s]" corpora_string requests_string;
      Dream.respond response
    | _ -> Dream.empty `Bad_Request
  )

let set_config_route =
  Dream.post "set_config"
  (fun request ->
    match%lwt Dream.form ~csrf:false request with
    | `Ok param ->
      let config = List.assoc "config" param  in
      let response = wrap set_config config in
      Log.info "<set_config> config=[%s]" config;
      Dream.respond response
    | _ -> Dream.empty `Bad_Request
  )

let all_routes = [
  home_route;
  ping_route;
  set_config_route;
  count_route;
]

let _ =
  let required = [ "port"] in
  Dream_config.load ~required ();
  Log.init();
  load_corpusbank();
  Dream.run
    ~error_handler:Dream.debug_error_handler
    ~port: (Dream_config.get_int "port")
  @@ Dream.logger
  @@ Dream.router all_routes
