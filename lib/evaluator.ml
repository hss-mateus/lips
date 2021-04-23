open Ast

module Env = Map.Make(String)

type value =
  | VClosure of value Env.t * string * t
  | VInt of int
  | VBool of bool
  | VPair of value * value
  | VNil
  | VFix of value Env.t * string * t

exception TypeError of string

let rec eval env = function
  | EInt n -> VInt n
  | EBool b -> VBool b
  | EAdd (e1, e2) ->
      (match (eval env e1, eval env e2) with
      | VInt x, VInt y -> VInt (x + y)
      | _ -> raise (TypeError "Expected numeric values in sum"))
  | EIf (cond, if_true, if_false) ->
      (match eval env cond with
      | VBool b ->
          let expr = if b then if_true else if_false
          in eval env expr
      | _ -> raise (TypeError "Expected boolean value in if expression"))
  | EPair (e1, e2) -> VPair (eval env e1, eval env e2)
  | EFst expr ->
      (match eval env expr with
      | VPair (v, _) -> v
      | _ -> raise (TypeError "Expected argument of type pair"))
  | ESnd expr ->
      (match eval env expr with
      | VPair (_, v) -> v
      | _ -> raise (TypeError "Expected argument of type pair"))
  | ENil -> VNil
  | EIsAPair expr ->
      (match eval env expr with
      | VPair _ -> VBool true
      | _ -> VBool false)
  | EVar name -> Env.find name env
  | ELet (name, expr, body) ->
      let v = eval (Env.add name (VFix (env, name, expr)) env) expr in
      eval (Env.add name v env) body
  | ELam (param, body) -> VClosure (env, param, body)
  | EApp (e1, e2) ->
      let arg = eval env e2 in
      (match eval env e1 with
      | VClosure (env', param, body) ->
          let env'' = Env.add param arg env' in
          eval env'' body
      | VFix (env', name, expr) ->
          let env'' = Env.add name (VFix (env', name, expr)) env' in
          (match eval env'' expr with
          | VClosure (env''', param, body) ->
              let env'''' = Env.add name (VFix (env'', name, expr)) (Env.add param arg env''') in
              eval env'''' body
          | _ -> raise (TypeError "Expected function value in application"))
      | _ -> raise (TypeError "Expected function value in application"))

let rec to_string = function
  | VClosure (_, param, _) -> "(lambda (" ^ param ^ ") ...)"
  | VInt n -> string_of_int n
  | VBool b -> string_of_bool b
  | VPair (v1, v2) -> "(" ^ to_string v1 ^ " . " ^ to_string v2 ^ ")"
  | VNil -> "nil"
  | VFix _ -> failwith "unreachable"

let rec to_string_debug = function
  | VClosure _ -> "fun () -> ..."
  | VInt n -> "(VInt " ^ string_of_int n ^ ")"
  | VBool b -> "(VBool " ^ string_of_bool b ^ ")"
  | VPair (v1, v2) -> "(VPair " ^ to_string_debug v1 ^ ", " ^ to_string_debug v2 ^ ")"
  | VNil -> "VNil"
  | VFix _ -> failwith "unreachable"
