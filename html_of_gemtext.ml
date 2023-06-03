let proxy_url ~current url =
  match Uri.scheme url with
  | Some "gemini" | None -> (
      let proxied = Uri.with_scheme url None |> Fun.flip Uri.with_host None in
      let proxied =
        match Uri.query url with
        | [] -> proxied
        | (input, _) :: _ -> Uri.with_query' proxied [ ("input", input) ]
      in
      match Uri.host url with
      | Some host ->
          if host = Key_gen.default_host () then proxied
          else
            let new_path = "/gemini/" ^ host ^ Uri.path url in
            Uri.with_path proxied new_path
      | None ->
          let current_host = Option.get (Uri.host current) in
          Uri.with_path proxied
            (if Filename.is_relative (Uri.path url) then
             "/gemini/" ^ current_host
             ^ Filename.concat (Uri.path current) (Uri.path url)
            else "/gemini/" ^ current_host ^ Uri.path url))
  | Some _ -> url

type ctx = {
  title : string option (* The first H1 heading, if found *);
  body : Html_types.flow5 Tyxml_html.elt list;
}

let set_title ctx t =
  match ctx.title with None -> { ctx with title = Some t } | Some _ -> ctx

let add ctx elt = { ctx with body = elt :: ctx.body }
let id_of_string = String.map (function ' ' -> '-' | c -> c)
let images_ext = [ ".gif"; ".jpg"; ".jpeg"; ".png"; ".webp" ]

let hof ~url:current gemtext =
  let { title; body } =
    Razzia.Gemtext.of_string gemtext
    |> List.rev
    (* TODO: Fix parsing reverse order in Razzia Gemtext parsing *)
    |> List.fold_left
         (fun acc ->
           let open Tyxml_html in
           function
           | Razzia.Gemtext.Text "" -> acc
           | Text t -> p [ txt t ] |> add acc
           | Link { url; name } ->
               let proxied_url =
                 proxy_url ~current (Uri.of_string url) |> Uri.to_string
               in
               let link_name = Option.value name ~default:url in
               (if List.mem (Filename.extension url) images_ext then
                figure
                  ~figcaption:(`Bottom (figcaption [ txt link_name ]))
                  [
                    a
                      ~a:[ a_href proxied_url ]
                      [ img ~src:proxied_url ~alt:link_name () ];
                  ]
               else a ~a:[ a_href proxied_url ] [ txt link_name ])
               |> add acc
           | Preformat { alt; text } ->
               let pre = pre [ txt text ] in
               Option.fold alt ~none:pre ~some:(fun caption ->
                   figure
                     ~figcaption:(`Top (figcaption [ txt caption ]))
                     [ pre ])
               |> add acc
           | Heading (level, heading) ->
               let h = match level with `H1 -> h1 | `H2 -> h2 | `H3 -> h3 in
               h ~a:[ a_id (id_of_string heading) ] [ txt heading ]
               |> add acc |> Fun.flip set_title heading
           | ListItem item -> ul [ li [ txt item ] ] |> add acc
           | Quote quote -> blockquote [ txt quote ] |> add acc)
         { title = None; body = [] }
  in
  let title = Option.value title ~default:(Uri.to_string current) in
  (Format.asprintf "%s ‐ %s" title (Key_gen.service_name ()), List.rev body)
