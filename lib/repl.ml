let rec read () =
  try
    let () = print_string "Prelude> " in
    let line = read_line () in
    let ast = Parser.parse line in
    Some ast
  with
  | End_of_file -> None
  | Failure _ ->
      print_endline "Error: Cannot parse input";
      read ()

let rec loop () =
  match read () with
  | Some ast ->
      (try
        ast
        |> Evaluator.eval Evaluator.Env.empty
        |> Evaluator.to_string
        |> (fun str -> "=> " ^ str)
        |> print_endline;
        loop ()
      with Not_found ->
        print_endline "Error: Identifier not found";
        loop ())
  | None -> print_endline ""
