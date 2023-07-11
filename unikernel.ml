module Main
    (Static : Mirage_kv.RO)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Tcpip.Stack.V4V6)
    (Time : Mirage_time.S)
    (Dns : Dns_client_mirage.S with type Transport.stack = Stack.t) =
struct
  module Dream = Dream__mirage.Mirage.Make (Pclock) (Time) (Stack)
  module Razzia_io = Razzia_mirage.Make (Pclock) (Stack) (Dns)
  open Lwt.Infix

  let i'm_a_teapot = Dream.int_to_status 418
  let string_of_html = Format.asprintf "%a" (Tyxml_html.pp ())
  let default_headers = [ ("Content-Type", "text/html; charset=utf-8") ]

  let http_of_gemini gemini_url resp =
    match resp with
    | Razzia.Input { prompt; sensitive } ->
        Html.mk_input ~gemini_url ~response:(Some resp) ~sensitive ~prompt ()
        |> string_of_html
        |> Dream.response ~headers:default_headers
    | Success { body; encoding; mime; _ } ->
        let headers, body =
          match mime with
          | Gemtext { lang } ->
              ( (match lang with
                | None -> default_headers
                | Some lang -> ("Content-Language", lang) :: default_headers),
                let page_title, body =
                  Html_of_gemtext.hof ~url:gemini_url body
                in
                Html.mk_page ~gemini_url ~response:(Some resp) ~page_title ~body
                  ()
                |> string_of_html )
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
        let port =
          Uri.query gemini_url
          |> List.find_map (function
               | "port", [ port ] when port <> "1965" -> int_of_string_opt port
               | _ -> None)
        in
        let location =
          "gemini/" ^ Option.get (Uri.host gemini_url) ^ Uri.path resolved
          |> Uri.with_path resolved (* Loc: https://hostname/gemini/host/path *)
          |> Fun.flip Uri.with_port port
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
        `TempFailure info
        |> Html.mk_error ~gemini_url ~response:(Some resp)
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
        `PermFailure info
        |> Html.mk_error ~gemini_url ~response:(Some resp)
        |> string_of_html
        |> Dream.response ~status ~headers:default_headers
    | ClientCertReq (_, info) ->
        `ClientCertReq info
        |> Html.mk_error ~gemini_url ~response:(Some resp)
        |> string_of_html
        |> Dream.response ~status:i'm_a_teapot

  let proxy stack url =
    let url =
      if Uri.path url = String.empty then Uri.with_path url "/" else url
    in
    let with_timeout f =
      Lwt.pick
        [
          f ();
          ( Time.sleep_ns (Duration.of_sec 5) >>= fun () ->
            Html.mk_error ~gemini_url:url ~response:None `Timeout
            |> string_of_html
            |> Dream.respond ~headers:default_headers );
        ]
    in
    match Razzia.make_request url with
    | Ok request ->
        with_timeout (fun () ->
            Razzia_io.get stack request >|= function
            | Ok resp -> http_of_gemini url resp
            | Error err ->
                `Response err
                |> Html.mk_error ~gemini_url:url ~response:None
                |> string_of_html
                |> Dream.response ~headers:default_headers)
    | Error err ->
        `Request err
        |> Html.mk_error ~gemini_url:url ~response:None
        |> string_of_html
        |> Dream.respond ~headers:default_headers

  let get_query req =
    match Dream.query req "input" with
    | None -> (
        match Dream.all_queries req with
        | (value, "") :: _ -> [ (value, []) ]
        | _ -> [])
    | Some value -> [ (value, []) ]

  let get_port req = Option.bind (Dream.query req "port") int_of_string_opt

  let homepage stack host req =
    Uri.make ~scheme:"gemini" ~host ~path:"/" ~query:(get_query req)
      ?port:(get_port req) ()
    |> proxy stack

  let default_proxy stack host =
    Dream.static String.empty ~loader:(fun _ path req ->
        Uri.make ~scheme:"gemini" ~host ~path ~query:(get_query req)
          ?port:(get_port req) ()
        |> proxy stack)

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

  let redirect_about _ =
    Dream.respond ~status:`Temporary_Redirect
      ~headers:[ ("Location", Key_gen.about_url ()) ]
      ""

  let error_handler err =
    Lwt.return
    @@
    match err with
    | { Dream.condition = `Response resp; _ } ->
        `HTTP (Dream.status resp)
        |> Html.mk_error
             ~gemini_url:
               (Uri.make ~scheme:"gemini" ~host:(Key_gen.default_host ())
                  ~path:"/" ())
             ~response:None
        |> string_of_html
        |> Dream.response ~headers:default_headers
        |> Option.some
    | { response; _ } -> response

  (* TODO: Improve <br /> adding algorithm *)
  let start static _ stack _ _ =
    let default_host = Key_gen.default_host () in
    [
      Dream.get "/" (homepage stack default_host);
      Dream.get "/gemini" redirect_about;
      Dream.get "/gemini/**" (gemini_proxy stack);
      Dream.get "/static/**" (serve_static static);
      Dream.get "/**" (default_proxy stack default_host);
    ]
    |> Dream.router |> Dream.logger
    |> Dream.https ~error_handler ~port:(Key_gen.port ()) (Stack.tcp stack)
end
