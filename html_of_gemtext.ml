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
             let path =
               Uri.path current |> Mirage_kv.Key.v |> Mirage_kv.Key.parent
               |> Mirage_kv.Key.to_string
             in
             "/gemini/" ^ current_host ^ Filename.concat path (Uri.path url)
            else "/gemini/" ^ current_host ^ Uri.path url))
  | Some _ -> url

type ctx = {
  title : string option (* The first H1 heading, if found *);
  body : Html_types.flow5 elts;
  items : Html_types.li elts; (* List items *)
  paragraph : Html_types.p_content_fun elts;
      (* We merge consecutive text line into a paragraph *)
  quote : Html_types.blockquote_content_fun elts;
      (* Also for quotation line into a blockquote *)
}

and 'a elts = 'a Tyxml_html.elt list

module Ctx = struct
  let empty =
    { title = None; body = []; items = []; paragraph = []; quote = [] }

  let[@inline] push_body elt ctx = { ctx with body = elt :: ctx.body }
  let break_lines = List.rev

  let render_items ctx =
    {
      ctx with
      body = Tyxml_html.ul (List.rev ctx.items) :: ctx.body;
      items = [];
    }

  let render_paragraph ctx =
    {
      ctx with
      body = Tyxml_html.p (break_lines ctx.paragraph) :: ctx.body;
      paragraph = [];
    }

  let render_blockquote ctx =
    {
      ctx with
      body = Tyxml_html.blockquote (break_lines ctx.quote) :: ctx.body;
      quote = [];
    }

  let helper ~body ~items ~paragraph ~quote ctx : ctx =
    match (ctx.items, ctx.paragraph, ctx.quote) with
    | [], [], [] -> body ctx
    | _, [], [] -> items ctx
    | [], _, [] -> paragraph ctx
    | [], [], _ -> quote ctx
    | _ -> assert false

  let add ctx elt =
    let push f ctx = f ctx |> push_body elt in
    helper ctx ~body:(push_body elt) ~items:(push render_items)
      ~paragraph:(push render_paragraph) ~quote:(push render_blockquote)

  let flush ctx =
    helper ctx ~body:Fun.id ~items:render_items ~paragraph:render_paragraph
      ~quote:render_blockquote

  let add_item ctx elt =
    let ctx = { ctx with items = elt :: ctx.items } in
    match (ctx.paragraph, ctx.quote) with
    | [], [] -> ctx
    | _, [] -> render_paragraph ctx
    | [], _ -> render_blockquote ctx
    | _, _ -> assert false

  let add_to_paragraph ctx elt =
    let ctx = { ctx with paragraph = elt :: ctx.paragraph } in
    match (ctx.items, ctx.quote) with
    | [], [] -> ctx
    | _, [] -> render_items ctx
    | [], _ -> render_blockquote ctx
    | _, _ -> assert false

  let add_to_blockquote ctx elt =
    let ctx = { ctx with quote = elt :: ctx.quote } in
    match (ctx.items, ctx.paragraph) with
    | [], [] -> ctx
    | _, [] -> render_items ctx
    | [], _ -> render_paragraph ctx
    | _, _ -> assert false
end

let set_title ctx t =
  match ctx.title with None -> { ctx with title = Some t } | Some _ -> ctx

let id_of_string =
  String.map (function ' ' -> '-' | c -> Char.lowercase_ascii c)

let is_image fname =
  List.mem (Filename.extension fname)
    [ ".gif"; ".jpg"; ".jpeg"; ".png"; ".webp" ]

let inline_image url name =
  let open Tyxml_html in
  if is_image url then
    match name with
    | None -> `Link (img ~src:url ~alt:"" ())
    | Some name ->
        `Figure
          (figure
             ~figcaption:(`Bottom (figcaption [ txt name ]))
             [ a ~a:[ a_href url ] [ img ~src:url ~alt:name () ] ])
  else
    `Link
      (match name with
      | None -> a ~a:[ a_href url ] []
      | Some name -> a ~a:[ a_href url ] [ txt name ])

let hof ~url:current gemtext =
  let ctx =
    Razzia.Gemtext.of_string gemtext
    |> List.rev
    (* TODO: Fix parsing reverse order in Razzia Gemtext parsing *)
    |> List.fold_left
         (fun acc ->
           let open Tyxml_html in
           function
           | Razzia.Gemtext.Text "" -> Ctx.flush acc
           | Text t -> txt t |> Ctx.add_to_paragraph acc
           | Link { url; name } -> (
               let proxied_url =
                 proxy_url ~current (Uri.of_string url) |> Uri.to_string
               in
               match inline_image proxied_url name with
               | `Figure i -> Ctx.add acc i
               | `Link l -> Ctx.add_to_paragraph acc l)
           | Preformat { alt; text } ->
               let pre = pre [ txt text ] in
               Option.fold alt ~none:pre ~some:(fun caption ->
                   figure
                     ~figcaption:(`Top (figcaption [ txt caption ]))
                     [ pre ])
               |> Ctx.add acc
           | Heading (level, heading) ->
               let h = match level with `H1 -> h1 | `H2 -> h2 | `H3 -> h3 in
               h ~a:[ a_id (id_of_string heading) ] [ txt heading ]
               |> Ctx.add acc |> Fun.flip set_title heading
           | ListItem item -> li [ txt item ] |> Ctx.add_item acc
           | Quote quote -> txt quote |> Ctx.add_to_blockquote acc)
         Ctx.empty
  in
  let { title; body; _ } = Ctx.flush ctx in
  let title = Option.value title ~default:(Uri.to_string current) in
  (Format.asprintf "%s ‐ %s" title (Key_gen.service_name ()), List.rev body)
