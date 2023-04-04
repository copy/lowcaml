let () =
  match Sys.argv with
  | [| _; source; dest |] ->
    let source = In_channel.with_open_bin source In_channel.input_all in
    Out_channel.with_open_bin dest (fun c ->
        output_string c {|let data = "|};
        output_string c (String.escaped source);
        output_string c {|"|};
      )
  | _ ->
    print_endline "Usage: embed source dest";
    exit 1
