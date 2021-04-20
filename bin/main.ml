let input =
  "(let (map (lambda (f)
               (lambda (xs)
                 (if (pair? xs)
                   ((f (fst xs)) . ((map f) (snd xs)))
                   xs))))
     ((map (lambda (x) (+ x 1)))
      (1 . (2 . (3 . nil)))))"

let () =
  input
  |> Lips.Parser.parse
  |> Lips.Evaluator.eval Lips.Evaluator.Env.empty
  |> Lips.Evaluator.to_string
  |> print_endline
