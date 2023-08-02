include struct
  open Html_types

  type ctx = {
    title : string option (* The first H1 heading, if found *);
    body : flow5 elts;
    items : li elts; (* List items *)
    paragraph : p_content_fun elts;
        (* We merge consecutive text line into a paragraph *)
    quote : blockquote_content_fun elts;
        (* Also for quotation line into a blockquote *)
  }

  and 'a elts = 'a Tyxml_html.elt list
end

module Ctx = struct
  let empty =
    { title = None; body = []; items = []; paragraph = []; quote = [] }

  let[@inline] push_body elt ctx = { ctx with body = elt :: ctx.body }

  (* Operates on a reversed list *)
  let break_lines l =
    let rec loop acc = function
      | [] -> acc
      | [ _ ] when acc = [] -> l
      | [ x ] -> x :: Tyxml_html.br () :: acc
      | x :: xs when acc = [] -> loop (x :: acc) xs
      | x :: xs -> loop (x :: Tyxml_html.br () :: acc) xs
    in
    loop [] l

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

  (* Render everything *)
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

let proxy_url ~current url =
  match Uri.scheme url with
  | Some "gemini" | None -> (
      let proxied =
        match Uri.port url with
        | None -> Uri.with_scheme url None |> Fun.flip Uri.with_host None
        | Some _ ->
            (* Don't reset host and scheme if port component is present *)
            Uri.with_scheme url (Some "https")
            |> Fun.flip Uri.with_host (Some (Key_gen.default_host ()))
      in
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
          if current_host = Key_gen.default_host () then proxied
          else
            let path =
              if Filename.is_relative (Uri.path url) then
                let path =
                  Uri.path current |> Mirage_kv.Key.v |> Mirage_kv.Key.parent
                  |> Mirage_kv.Key.to_string
                in
                Filename.concat path (Uri.path url)
              else
                (* Dream treats /foo and /foo/ differently *)
                match Uri.path url with "/" -> "" | p -> p
            in
            "/gemini/" ^ current_host ^ path |> Uri.with_path proxied)
  | Some _ -> url

let set_title ctx t =
  match ctx.title with None -> { ctx with title = Some t } | Some _ -> ctx

let id_of_string =
  String.map (function ' ' -> '-' | c -> Char.lowercase_ascii c)

let is_in_mimes mimes fname = List.mem (Filename.extension fname) mimes

let is_image =
  is_in_mimes
    [
      ".apng";
      ".avif";
      ".gif";
      ".jpg";
      ".jpeg";
      ".jpe";
      ".jif";
      ".jfif";
      ".png";
      ".webp";
    ]

let is_audio = is_in_mimes [ ".mp3"; ".ogg" ]
let is_video = is_in_mimes [ ".mp4"; ".webm" ]

(* Inline image and audio. *)
let handle_link url name =
  let open Tyxml_html in
  let handle_dynamic typ =
    let elt_fun, label =
      match typ with `Audio -> (audio, "audio") | `Video -> (video, "video")
    in
    let elt =
      elt_fun ~src:url
        ~a:[ a_controls (); a_preload `Metadata ]
        [ a ~a:[ a_href url ] [ txt ("Download " ^ label) ] ]
    in
    match name with
    | None -> `Inline elt
    | Some name ->
        `Figure (figure ~figcaption:(`Bottom (figcaption [ txt name ])) [ elt ])
  in
  if is_image url then
    let attr = [ a_href url; a_target "_blank" ] in
    let img_attr = [ Unsafe.string_attrib "loading" "lazy" ] in
    match name with
    | None -> `Inline (a ~a:attr [ img ~a:img_attr ~src:url ~alt:"" () ])
    | Some name ->
        `Figure
          (figure
             ~figcaption:(`Bottom (figcaption [ txt name ]))
             [ a ~a:attr [ img ~a:img_attr ~src:url ~alt:name () ] ])
  else if is_audio url then handle_dynamic `Audio
  else if is_video url then handle_dynamic `Video
  else
    `Inline
      (a
         ~a:[ a_href url ]
         [ txt (Option.value name ~default:url |> Uri.pct_decode) ])

let handle_preformat { Razzia.Gemtext.alt; text } =
  let open Tyxml_html in
  let pre = pre [ txt text ] in
  match alt with
  | None -> pre
  | Some lang -> (
      let figure =
        figure
          ~figcaption:(`Top (figcaption [ txt lang ]))
          ~a:[ a_class [ "preformat" ] ]
      in
      match
        Hilite.Syntax.src_code_to_html ~lang ~src:(text ^ "\n")
        (* An extra newline is needed to detect pattern on last line. *)
      with
      | (exception Failure _) | Error (`Msg _) -> figure [ pre ]
      | Ok highlight -> figure [ Tyxml_html.Unsafe.data highlight ])

let hof ~url:current gemtext =
  let ctx =
    Razzia.Gemtext.of_string gemtext
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
               match handle_link proxied_url name with
               | `Inline l -> Ctx.add_to_paragraph acc l
               | `Figure i -> Ctx.add acc i)
           | Preformat pf -> handle_preformat pf |> Ctx.add acc
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
