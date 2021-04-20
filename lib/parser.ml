open Angstrom
open Ast

let ws = skip_while (function ' ' | '\t' | '\r' | '\n' -> true | _ -> false)

let parens p = char '(' *> p <* char ')'

let alphanum = satisfy (function '0'..'9' | 'a'..'z' | 'A'..'Z' -> true | _ -> false)

let word = many1 alphanum >>| (fun cs -> cs |> List.to_seq |> String.of_seq)

let expr = fix (fun expr ->
  let int =
    take_while1 (function '0'..'9' -> true | _ -> false)
    >>| int_of_string
    >>| (fun n -> EInt n)
  in

  let bool =
    (string "true" <|> string "false")
    >>| bool_of_string
    >>| (fun b -> EBool b)
  in

  let add =
    char '+' *> lift2 (fun op1 op2 -> EAdd (op1, op2)) expr expr
  in

  let if_p =
    string "if" *> lift3 (fun cond if_true if_false -> EIf (cond, if_true, if_false)) expr expr expr
  in

  let pair =
    lift2 (fun fst snd -> EPair (fst, snd)) (expr <* char '.') expr
  in

  let fst =
    string "fst" *> expr >>| (fun e -> EFst e)
  in

  let snd =
    string "snd" *> expr >>| (fun e -> ESnd e)
  in

  let nil =
    string "nil" *> return ENil
  in

  let is_a_pair =
    string "pair?" *> expr >>| (fun e -> EIsAPair e)
  in

  let var =
    word >>| (fun w -> EVar w)
  in

  let let_p =
    string "let" *>
    (lift3
      (fun name value body -> ELet (name, value, body))
      (ws *> char '(' *> ws *> word)
      (expr <* ws <* char ')' <* ws)
      expr)
  in

  let lam =
    string "lambda" *> ws *>
    (lift2
      (fun param body -> ELam (param, body))
      (parens (ws *> word <* ws))
      expr)
  in

  let app =
    lift2 (fun e1 e2 -> EApp (e1, e2)) expr expr
  in

  let surrounded_exprs =
    add
    <|> if_p
    <|> pair
    <|> fst
    <|> snd
    <|> is_a_pair
    <|> let_p
    <|> lam
    <|> app
  in

  let misc_exprs =
    int
    <|> bool
    <|> nil
    <|> var
  in

  ws *> ((parens (ws *> surrounded_exprs <* ws)) <|> misc_exprs) <* ws)

let parse str =
  match parse_string ~consume:All expr str with
  | Ok v -> v
  | Error msg -> failwith msg
