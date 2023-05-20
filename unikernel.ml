module Html = struct
  open Tyxml_html

  let mk_head ~page_title () = head (title (txt page_title)) []

  let mk_input ~sensitive ~prompt () =
    html
      (mk_head ~page_title:prompt ())
      (body
         [
           form
             [
               h1 [ txt prompt ];
               input
                 ~a:
                   [
                     a_input_type (if sensitive then `Password else `Text);
                     a_name "input";
                     a_maxlength 1024;
                   ]
                 ();
               input ~a:[ a_input_type `Submit ] ();
             ];
         ])

  let mk_error kind info =
    let label =
      match kind with
      | `TempFailure -> "Temporary failure"
      | `PermFailure -> "Permanent failure"
      | `ClientCertReq -> "Client cert required !"
    in
    html
      (mk_head ~page_title:"Error" ())
      (body [ h1 [ txt label ]; p [ txt info ] ])
end

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
  let string_of_html = Format.asprintf "%a" (Tyxml_html.pp ())

  let http_of_gemini gemini_url =
    let default_headers = [ ("Content-Type", "text/html; charset=utf-8") ] in
    function
    | Razzia.Input { prompt; sensitive } ->
        Html.mk_input ~sensitive ~prompt ()
        |> string_of_html
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
        Dream.response ~headers body
    | Redirect (status, loc) ->
        let base =
          Uri.make ~scheme:"https" ~host:(Key_gen.default_host ())
            () (* https://hostname *)
        in
        let resolved = Uri.resolve "https" base (Uri.of_string loc) in
        (* https://hostname/loc if [loc] relative, [loc] else *)
        let location =
          "gemini/" ^ Option.get (Uri.host gemini_url) ^ Uri.path resolved
          |> Uri.with_path resolved (* Loc: https://hostname/gemini/host/path *)
          |> Fun.flip Uri.with_port (Some 8080)
          (* REMOVE THIS *)
        in
        let status =
          match status with
          | `Temp -> `Temporary_Redirect
          | `Perm -> `Permanent_Redirect
        in
        Dream.response ~status
          ~headers:[ ("Location", Uri.to_string location) ]
          ""
    | TempFailure (status, info) ->
        let status =
          match status with
          | `Msg -> i'm_a_teapot
          | `ServerUnavailable -> `Service_Unavailable
          | `CGIError -> `Bad_Gateway
          | `ProxyError -> `Bad_Gateway
          | `SlowDown -> `Too_Many_Requests
        in
        Html.mk_error `TempFailure info
        |> string_of_html
        |> Dream.response ~status ~headers:default_headers
    | PermFailure (status, info) ->
        let status =
          match status with
          | `Msg -> i'm_a_teapot
          | `NotFound -> `Not_Found
          | `Gone -> `Gone
          | `ProxyRequestRefused -> i'm_a_teapot
          | `BadRequest -> `Bad_Request
        in
        Html.mk_error `PermFailure info
        |> string_of_html
        |> Dream.response ~status ~headers:default_headers
    | ClientCertReq (_, info) ->
        Html.mk_error `ClientCertReq info
        |> string_of_html
        |> Dream.response ~status:i'm_a_teapot

  (* TODO: handle error *)
  let proxy stack url =
    let url =
      if Uri.path url = String.empty then Uri.with_path url "/" else url
    in
    match Razzia.make_request url with
    | Ok request -> (
        Razzia_io.get stack request >|= function
        | Ok resp -> http_of_gemini url resp
        | Error err -> Format.kasprintf failwith "Error: %a" Razzia.pp_err err)
    | Error err ->
        Format.kasprintf failwith "Error: %a" Razzia.pp_request_err err

  (* TODO: better handling *)
  let get_query req =
    match Dream.query req "input" with
    | None -> []
    | Some value -> [ (value, []) ]

  let homepage stack host req =
    Uri.make ~scheme:"gemini" ~host ~path:"/" ~query:(get_query req) ()
    |> proxy stack

  (* TODO: handle port, if impossible with Dream do it with a get parameter *)
  let default_proxy stack host req =
    Uri.make ~scheme:"gemini" ~host ~path:(Dream.param req "path")
      ~query:(get_query req) ()
    |> proxy stack

  let gemini_proxy stack =
    Dream.static String.empty ~loader:(fun _ path req ->
        let[@warning "-8"] (host :: path) = String.split_on_char '/' path in
        Uri.make ~scheme:"gemini" ~host ~path:(String.concat "/" path)
          ~query:(get_query req) ()
        |> proxy stack)

  let start _ _ _ _ stack =
    let default_host = Key_gen.default_host () in
    [
      Dream.get "/" (homepage stack default_host);
      (* TODO: Handle more than root *)
      Dream.get "/gemini" (homepage stack default_host);
      (* TODO: handle empty path *)
      Dream.get "/gemini/**" (gemini_proxy stack);
      Dream.get "/:path" (default_proxy stack default_host);
    ]
    |> Dream.router |> Dream.logger
    |> Dream.https ~port:(Key_gen.port ()) (Stack.tcp stack)
end
