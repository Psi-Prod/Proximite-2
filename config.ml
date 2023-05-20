open Mirage

let port =
  let doc = Key.Arg.info ~doc:"HTTPS listen port." [ "port" ] in
  Key.(create "port" Arg.(opt int 8080 doc))

let default_page_title =
  let doc =
    Key.Arg.info ~doc:"The default page title" [ "default-page-title" ]
  in
  Key.(create "default-page-title" Arg.(required string doc))

let default_host =
  let doc = Key.Arg.info ~doc:"The default host proxied" [ "default-host" ] in
  Key.(create "default-host" Arg.(required string doc))

let main =
  main
    ~keys:[ Key.v port; Key.v default_page_title; Key.v default_host ]
    ~packages:
      [
        package "dream-mirage"
          ~pin:"git+https://github.com/tmcgilchrist/dream.git#patch-1";
        package "razzia" ~pin:"git+https://github.com/Psi-Prod/Razzia.git#dev";
        package "razzia-mirage"
          ~pin:"git+https://github.com/Psi-Prod/Razzia.git#dev";
        package "tyxml";
      ]
    "Unikernel.Main"
    (random @-> mclock @-> pclock @-> time @-> stackv4v6 @-> job)

let () =
  register "proximite"
    [
      main $ default_random $ default_monotonic_clock $ default_posix_clock
      $ default_time
      $ generic_stackv4v6 default_network;
    ]
