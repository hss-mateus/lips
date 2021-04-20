type t =
  | EInt of int
  | EBool of bool
  | EAdd of t * t
  | EIf of t * t * t
  | EPair of t * t
  | EFst of t
  | ESnd of t
  | ENil
  | EIsAPair of t
  | EVar of string
  | ELet of string * t * t
  | ELam of string * t
  | EApp of t * t

let rec to_string = function
  | EInt n -> "(EInt " ^ string_of_int n ^ ")"
  | EBool b -> "(EBool " ^ string_of_bool b ^ ")"
  | EAdd (e1, e2) -> "(" ^ "EAdd (" ^ to_string e1 ^ ", " ^ to_string e2 ^ "))"
  | EIf (cond, if_true, if_false) -> "(EIf (" ^ to_string cond ^ ", " ^ to_string if_true ^ ", " ^ to_string if_false ^ "))"
  | EPair (fst, snd) -> "(EPair (" ^ to_string fst ^ ", " ^ to_string snd ^ "))"
  | EFst e -> "(EFst " ^ to_string e ^ ")"
  | ESnd e -> "(ESnd " ^ to_string e ^ ")"
  | ENil -> "ENil"
  | EIsAPair e -> "(EIsAPair " ^ to_string e ^ ")"
  | EVar str -> "(EVar " ^ str ^ ")"
  | ELet (name, value, body) -> "(ELet (" ^ name ^ ", " ^ to_string value ^ ", " ^ to_string body ^ "))"
  | ELam (param, body) -> "(ELam (" ^ param ^ ", " ^ to_string body ^ "))"
  | EApp (e1, e2) -> "(EApp (" ^ to_string e1 ^ ", " ^ to_string e2 ^ "))"
