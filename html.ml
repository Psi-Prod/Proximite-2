open Tyxml_html

let fmt_page_title title =
  Printf.sprintf "%s ‐ %s" title (Key_gen.service_name ())

let mk_head ~page_title () =
  head
    (title (txt page_title))
    [ link ~rel:[ `Stylesheet ] ~href:"/static/styles.css" () ]

let without_trailing_slash url =
  match Uri.path url with
  | path when String.ends_with ~suffix:"/" (Uri.path url) ->
      String.sub path 0 (String.length path - 1) |> Uri.with_path url
  | _ -> url

let with_trailing_slash url =
  match Uri.path url with
  | _ when String.ends_with ~suffix:"/" (Uri.path url) -> url
  | path -> Uri.with_path url (path ^ "/")

let mk_header ~url () =
  let scheme =
    [
      a ~a:[ a_href (Key_gen.about_url ()) ] [ txt "gemini" ];
      span [ txt "://" ];
    ]
  in
  let host_url =
    let cleared =
      Uri.with_scheme url (Some "https")
      |> Fun.flip Uri.with_path "/" |> Fun.flip Uri.with_query []
    in
    match Uri.host url with
    | None -> cleared
    | Some h when Key_gen.default_host () = h -> cleared
    | Some _ -> Html_of_gemtext.proxy_url ~current:url url
  in
  let host =
    [
      a
        ~a:[ a_href (Uri.to_string (without_trailing_slash host_url)) ]
        [ txt (Option.get (Uri.host url)) ];
    ]
  in
  let path =
    match Mirage_kv.Key.(Uri.path url |> v |> segments) with
    | [] -> [ span [ txt "/" ] ]
    | segs ->
        List.fold_left
          (fun (url, acc) seg ->
            let url = Filename.concat (Uri.path url) seg |> Uri.with_path url in
            ( url,
              [
                a ~a:[ a_href (Uri.to_string url) ] [ txt seg ];
                span [ txt "/" ];
              ]
              @ acc ))
          (with_trailing_slash host_url, [])
          segs
        |> snd |> List.rev
  in
  let port =
    match Uri.port url with
    | None -> []
    | Some p -> [ span [ txt ":" ]; span [ txt (Int.to_string p) ] ]
  in
  let query =
    match Uri.verbatim_query url with
    | None -> []
    | Some q -> [ span ~a:[ a_id "query" ] [ txt ("?" ^ Uri.pct_decode q) ] ]
  in
  header [ nav (scheme @ host @ port @ path @ query) ]

let mk_footer ~url ~response () =
  let url = Uri.to_string url in
  let label =
    [ txt "Proxied content from "; a ~a:[ a_href url ] [ txt url ] ]
  in
  let img =
    let url = Key_gen.random_banner_url () in
    a
      ~a:[ a_href url; a_target "_blank" ]
      [ img ~src:url ~alt:"Heyplzlookatme's banner" () ]
  in
  match response with
  | None -> footer [ img ]
  | Some resp ->
      let status = Razzia.status_code resp |> Int.to_string in
      let meta = Razzia.meta resp in
      footer
        [
          details (summary label)
            [
              table
                [
                  tr [ td [ txt "Status" ]; td [ txt status ] ];
                  tr [ td [ txt "Meta" ]; td [ txt meta ] ];
                ];
            ];
          img;
        ]

let mk_page ~gemini_url:url ~response ~page_title ~body:b () =
  let footer =
    match response with
    | Some (Razzia.Input _) -> [ mk_footer ~url ~response () ]
    | Some _ | None -> [ hr (); mk_footer ~url ~response () ]
  in
  html (mk_head ~page_title ())
    (body [ main ([ mk_header ~url (); article b ] @ footer) ])

type err =
  [ `TempFailure of string
  | `PermFailure of string
  | `ClientCertReq of string
  | `Request of Razzia.request_err
  | `Response of Razzia.err
  | `Timeout ]

let mk_error ~gemini_url ~response (err : err) =
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
    | `Timeout -> ("Timeout", "for 5s")
  in
  mk_page ~gemini_url ~response ~page_title:(fmt_page_title title)
    ~body:[ h1 [ txt title ]; p [ txt label ] ]
    ()

let mk_input ~gemini_url ~response ~sensitive ~prompt () =
  mk_page ~gemini_url ~response ~page_title:(fmt_page_title prompt)
    ~body:
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
                  a_id "input-field";
                ]
              ();
            br ();
            input ~a:[ a_input_type `Submit; a_id "input-submit" ] ();
          ];
      ]
    ()
