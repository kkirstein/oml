open Ocamlbuild_plugin
open Ocamlbuild_pack

let target_with_extension ext =
  List.exists (fun s -> Pathname.get_extension s = ext) !Options.targets

let rec copy_mlt_files path =
  Pathname.readdir path
  |> Array.iter
    (fun p ->
      if Pathname.is_directory (path / p) then
        copy_mlt_files (path / p)
      else if Pathname.check_extension p "mlt" then
        let src = path / p in
        let dst = !Options.build_dir / path / p in
        Shell.mkdir_p (!Options.build_dir / path);
        Pathname.copy src dst
      else
        ())

let () =
  let additional_rules =
    function
      | Before_hygiene  -> ()
      | After_hygiene   -> ()
      | Before_options  -> ()
      | After_options   -> ()
      | Before_rules    -> ()
      | After_rules     ->
          begin
            if target_with_extension "test" then begin
              rule "concat ml and mlt files"
                (*~insert:(`before "ocaml dependencies ml") *)
                ~insert:`top
                ~prod:"%.mlj"
                ~deps:["%.mlt"; "%.ml"]
                begin fun env _build ->
                  let () = Printf.printf "oh boy!\n" in
                  let ml  = env "%.ml" in
                  let mlt = env "%.mlt" in
                  let mlj = env "%.mlj" in
                  Cmd ( S [ A "cat" ; P ml ; P mlt ; Sh ">" ; P mlj ])
                end;

              rule "ocaml dependencies mlj"
                ~insert:(`before "ocaml dependencies ml")
                ~prod:"%.ml.depends"
                ~dep:"%.mlj"
                ~doc:"foo"
                (*Ocaml_tools.ocamldep_command "%.mlj" "%.ml.depends"); *)
                begin fun env _build ->
                  let ml  = env "%.ml" in
                  let mlt = env "%.mlt" in
                  let mlj = env "%.mlj" in
                  let mld = env "%.ml.depends" in
                  let tags = tags_of_pathname ml ++ "ocaml" ++ "ocamldep" in
                  Cmd(S[A "ocamlfind"; A "ocamldep"; T tags; A "-ml-synonym"; Sh "'.mlj'"; A "-modules"; P mlj; Sh ">"; Px mld])
                end;

              (*rule "ocaml: mlj & cmi -> cmo"
                ~insert:(`before "ocaml: ml & cmi -> cmo")
                ~deps:[ "%.mli"; "%.mlj"; "%.ml.depends"; "%.cmi" ]
                ~prods:[ "%.cmo" ]
                (Ocaml_compiler.byte_compile_ocaml_implem "%.mlj" "%.cmo"); *)

              rule "ocaml: mlj & cmi -> cmx & o"
                ~insert:(`before "ocaml: ml & cmi -> cmx & o")
                ~prods:["%.cmx"; "%" -.- !Options.ext_obj ]
                ~deps:["%.mlj"; "%.ml.depends"; "%.cmi"]
                begin fun env _build ->
                  let ml  = env "%.ml" in
                  let mlj = env "%.mlj" in
                  let cmx = env "%.cmx" in
                  let tags = tags_of_pathname ml ++ "ocaml" ++ "implem" in
                  Cmd(S[A "ocamlfind"; A "ocamlopt"; T tags; A "-I"; A "src/lib"; A "-o"; P cmx; A "-impl"; P mlj])
                end;
                (*Ocaml_compiler.native_compile_ocaml_implem "%.mlj"*)
            end;

            rule "Create a test target."
              ~prod:"%.test"
              ~dep:"%.native"
              begin fun env _build ->
                let test = env "%.test" and native = env "%.native" in
                Seq [ mv native test
                    ; Cmd (S [ A "ln"
                             ; A "-sf"
                             ; P (!Options.build_dir/test)
                             ; A Pathname.parent_dir_name])
                ]
              end;
          end
  in
  dispatch additional_rules
