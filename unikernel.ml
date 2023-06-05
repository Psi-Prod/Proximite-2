module Html = struct
  open Tyxml_html

  let mk_head ~page_title () =
    head
      (title (txt page_title))
      [ link ~rel:[ `Stylesheet ] ~href:"/static/styles.css" () ]

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

  let mk_page ~page_title ~body:b () = html (mk_head ~page_title ()) (body b)

  type err =
    [ `TempFailure of string
    | `PermFailure of string
    | `ClientCertReq of string
    | `Request of Razzia.request_err
    | `Response of Razzia.err ]

  let mk_error (err : err) =
    let title, label =
      match err with
      | `TempFailure info -> ("Temporary failure", info)
      | `PermFailure info -> ("Permanent failure", info)
      | `ClientCertReq info -> ("Client cert required", info)
      | `Request err -> (
          ( "Gemini error",
            match err with
            | `AboveMaxSize -> "URL is longer than 1024 characters"
            | `BeginWithBOM -> "URL begins with BOM"
            | `DomainNameError _ -> "Unknown host"
            | `MalformedUTF8 -> "URL contains malformed UTF-8"
            | `EmptyURL | `MissingHost | `MissingScheme | `UserInfoNotAllowed ->
                assert false (* Unreachable *) ))
      | `Response (`Header err) -> (
          ( "Server error",
            match err with
            | `InvalidCode c -> Printf.sprintf "Invalid status code %i" c
            | `Malformed -> "Response header is malformed"
            | `TooLong -> "Response header is longer than 1024 characters" ))
      | `Response (`Host (`BadDomainName msg)) -> ("Bad domain name", msg)
      | `Response (`Host (`InvalidHostname msg)) -> ("Invalid hostname", msg)
      | `Response (`Host (`UnknownHost msg)) -> ("Unknown host", msg)
      | `Response `NetErr -> ("Connection error", "")
    in
    let page_title = Printf.sprintf "%s ‐ %s" title (Key_gen.service_name ()) in
    html (mk_head ~page_title ()) (body [ h1 [ txt title ]; p [ txt label ] ])
end

module Main
    (Static : Mirage_kv.RO)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Random : Mirage_random.S)
    (Stack : Tcpip.Stack.V4V6)
    (Time : Mirage_time.S) =
struct
  module Dream = Dream__mirage.Mirage.Make (Pclock) (Time) (Stack)
  module Dns = Dns_client_mirage.Make (Random) (Time) (Mclock) (Pclock) (Stack)
  module Razzia_io = Razzia_mirage.Make (Pclock) (Stack) (Dns)
  open Lwt.Infix

  let i'm_a_teapot = Dream.int_to_status 418
  let string_of_html = Format.asprintf "%a" (Tyxml_html.pp ())
  let default_headers = [ ("Content-Type", "text/html; charset=utf-8") ]

  let http_of_gemini gemini_url = function
    | Razzia.Input { prompt; sensitive } ->
        Html.mk_input ~sensitive ~prompt ()
        |> string_of_html
        |> Dream.response ~headers:default_headers
    | Success { body; encoding; mime } ->
        let headers, body =
          match mime with
          | Gemtext { lang } ->
              ( (match lang with
                | None -> default_headers
                | Some lang -> ("Content-Language", lang) :: default_headers),
                let page_title, body =
                  Html_of_gemtext.hof ~url:gemini_url body
                in
                Html.mk_page ~page_title ~body () |> string_of_html )
          | MimeType mime ->
              ( [
                  ( "Content-Type",
                    Printf.sprintf "%s; charset=%s" mime
                      (Option.value encoding ~default:"utf-8") );
                ],
                body )
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
          |> Fun.flip Uri.with_port (Some (Key_gen.port ()))
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
        `TempFailure info |> Html.mk_error |> string_of_html
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
        `PermFailure info |> Html.mk_error |> string_of_html
        |> Dream.response ~status ~headers:default_headers
    | ClientCertReq (_, info) ->
        `ClientCertReq info |> Html.mk_error |> string_of_html
        |> Dream.response ~status:i'm_a_teapot

  let proxy stack url =
    let url =
      if Uri.path url = String.empty then Uri.with_path url "/" else url
    in
    match Razzia.make_request url with
    | Ok request -> (
        Razzia_io.get stack request >|= function
        | Ok resp -> http_of_gemini url resp
        | Error err ->
            `Response err |> Html.mk_error |> string_of_html
            |> Dream.response ~headers:default_headers)
    | Error err ->
        `Request err |> Html.mk_error |> string_of_html
        |> Dream.respond ~headers:default_headers

  (* TODO: Artichaut check *)
  let get_query req =
    match Dream.query req "input" with
    | None -> []
    | Some value -> [ (value, []) ]

  let get_port req = Option.bind (Dream.query req "port") int_of_string_opt

  let homepage stack host req =
    Uri.make ~scheme:"gemini" ~host ~path:"/" ~query:(get_query req)
      ?port:(get_port req) ()
    |> proxy stack

  let default_proxy stack host req =
    Uri.make ~scheme:"gemini" ~host ~path:(Dream.param req "path")
      ~query:(get_query req) ?port:(get_port req) ()
    |> proxy stack

  let gemini_proxy stack =
    Dream.static String.empty ~loader:(fun _ path req ->
        let[@warning "-8"] (host :: path) = Mirage_kv.Key.(segments (v path)) in
        Uri.make ~scheme:"gemini" ~host ~path:(String.concat "/" path)
          ~query:(get_query req) ?port:(get_port req) ()
        |> proxy stack)

  let serve_static fs =
    Dream.static String.empty ~loader:(fun _ path _ ->
        Static.get fs (Mirage_kv.Key.v path) >|= function
        | Ok body ->
            Dream.response
              ~headers:[ ("Content-Type", Magic_mime.lookup path) ]
              body
        | Error _ -> Dream.response ~status:`Not_Found "")

  (* TODO: Fetch TLS certs from git repo *)
  (* TODO: Inline image *)
  let start static _ _ _ stack _ =
    let default_host = Key_gen.default_host () in
    [
      Dream.get "/" (homepage stack default_host);
      Dream.get "/gemini" (homepage stack default_host);
      (* TODO: redirection about page *)
      Dream.get "/gemini/**" (gemini_proxy stack);
      Dream.get "/static/**" (serve_static static);
      Dream.get "/:path" (default_proxy stack default_host);
    ]
    |> Dream.router |> Dream.logger
    |> Dream.https ~port:(Key_gen.port ()) (Stack.tcp stack)
end
