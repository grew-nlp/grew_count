[%%shared
open Eliom_lib
open Eliom_content
open Html.D
]

module Grew_count_app =
  Eliom_registration.App (
  struct
    let application_name = "grew_count"
    let global_data_path = None
  end)

let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let () =
  Grew_count_app.register
    ~service:main_service
    (fun () () ->
       Lwt.return
         (Eliom_tools.F.html
            ~title:"grew_count"
            ~css:[["css";"grew_count.css"]]
            Html.F.(body [
              h1 [txt "Welcome from Eliom's distillery!"];
            ])))
