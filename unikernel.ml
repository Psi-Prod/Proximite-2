module Main
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Time : Mirage_time.S)
    (Stack : Tcpip.Stack.V4V6) =
struct
  module Dream = Dream__mirage.Mirage.Make (Pclock) (Time) (Stack)

  module Razzia_io =
    Razzia_mirage.Make (Random) (Time) (Mclock) (Pclock) (Stack)

  open Lwt.Infix

  let i'm_a_teapot = Dream.int_to_status 418

  let http_of_gemini =
    let default_headers = [ ("Content-Type", "text/html; charset=utf-8") ] in
    function
    | Razzia.Input _ as resp ->
        (* TODO: input *)
        Format.asprintf "%a" Razzia.pp_response resp
        |> Dream.response ~headers:default_headers
    | Success { body; encoding; mime } ->
        let headers =
          match mime with
          | Gemtext { lang = None } -> default_headers
          | Gemtext { lang = Some lang } ->
              ("Content-Language", lang) :: default_headers
          | MimeType mime ->
              [
                ( "Content-Type",
                  Printf.sprintf "%s; charset=%s" mime
                    (Option.value encoding ~default:"utf-8") );
              ]
        in
        Dream.response ~status:`OK ~headers body
    | Redirect (s, location) ->
        (* TODO: redirection *)
        let _status =
          match s with
          | `Temp -> `Temporary_Redirect
          | `Perm -> `Permanent_Redirect
        in
        Printf.sprintf "Goto %S" location
        |> Dream.response ~headers:default_headers
    | TempFailure (s, meta) ->
        (* TODO: temp failure *)
        let _status =
          match s with
          | `Msg -> i'm_a_teapot
          | `ServerUnavailable -> `Service_Unavailable
          | `CGIError -> `Bad_Gateway
          | `ProxyError -> `Bad_Gateway
          | `SlowDown -> `Too_Many_Requests
        in
        Printf.sprintf "Temp failure: %S" meta
        |> Dream.response ~headers:default_headers
    | PermFailure (s, meta) ->
        (* TODO: perm failure *)
        let _status =
          match s with
          | `Msg -> i'm_a_teapot
          | `NotFound -> `Not_Found
          | `Gone -> `Gone
          | `ProxyRequestRefused -> i'm_a_teapot
          | `BadRequest -> `Bad_Request
        in
        Printf.sprintf "Perm failure: %S" meta
        |> Dream.response ~headers:default_headers
    | ClientCertReq (_, meta) ->
        Printf.sprintf "Client cert required: %S" meta
        |> Dream.response ~code:418

  (* TODO: handle error *)
  let proxy stack url =
    match Razzia.make_request url with
    | Ok request -> (
        Razzia_io.get stack request >|= function
        | Ok resp -> http_of_gemini resp
        | Error err -> Format.kasprintf failwith "Error: %a" Razzia.pp_err err)
    | Error err ->
        Format.kasprintf failwith "Error: %a" Razzia.pp_request_err err

  let get_query req =
    match Dream.query req "input" with
    | None -> []
    | Some value -> [ (value, []) ]

  let homepage stack host req =
    Uri.make ~scheme:"gemini" ~host ~path:"/" ~query:(get_query req) ()
    |> proxy stack

  (* TODO: handle port *)
  let default_proxy stack host req =
    Uri.make ~scheme:"gemini" ~host ~path:(Dream.param req "path")
      ~query:(get_query req) ()
    |> proxy stack

  let gemini_proxy stack req =
    let[@warning "-8"] (host :: path) =
      Dream.param req "path" |> String.split_on_char '/'
    in
    if path = String.empty then
      Uri.make ~scheme:"gemini" ~host ~path:(String.concat "/" path)
        ~query:(get_query req) ()
      |> proxy stack
    else failwith "Handle empty path"

  let start _ _ _ _ stack =
    let default_host = Key_gen.default_host () in
    [
      Dream.get "/" (homepage stack default_host);
      Dream.get "/gemini/:path" (gemini_proxy stack);
      Dream.get "/:path" (default_proxy stack default_host);
    ]
    |> Dream.router |> Dream.logger
    |> Dream.http ~port:8080 (Stack.tcp stack)
end
