open Ast
open Types


let parse (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let unwrap_bool = function
    Bool(a) -> a
  | _ -> raise NoRuleApplies;;

(******************************************************************************)
(*                       Big-step semantics of expressions                    *)
(******************************************************************************)

let rec eval_expr st = function
    True -> Bool(true)
  | False -> Bool(false)
  | Var(s) -> st s
  | Const(n) -> Nat(n)
  | Not(e) -> Bool(not (unwrap_bool(eval_expr st e)))
  | And(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
      (Bool(a), Bool(b)) -> Bool(a && b)
    | _ -> raise NoRuleApplies)
  | Or(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
      (Bool a, Bool b) -> Bool(a || b)
    | _ -> raise NoRuleApplies)
  | Add(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
      (Nat a, Nat b) -> Nat(a + b)
    | _ -> raise NoRuleApplies)
  | Sub(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
      (Nat a, Nat b) -> Nat(a - b)
    | _ -> raise NoRuleApplies)
  | Mul(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
      (Nat a, Nat b) -> Nat(a * b)
    | _ -> raise NoRuleApplies)
  | Eq(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
      (Nat a, Nat b) -> Bool(a = b)
    | (Bool a, Bool b) -> Bool(a = b)
    | _ -> raise NoRuleApplies)
  | Leq(e1, e2) -> (match (eval_expr st e1, eval_expr st e2) with
      (Nat a, Nat b) -> Bool(a <= b)
    | _ -> raise NoRuleApplies)
;;



(******************************************************************************)
(*                      Small-step semantics of commands                      *)
(******************************************************************************)
  
let bot = fun x -> raise (UnboundVar x)

let bind f x v = fun y -> if y=x then v else f y

let state_of_conf = function
  Cmd(_,state) -> state
  | _ -> failwith "no state in conf"

let rec trace1 = function
    Cmd(Skip, state) -> St state
  | Cmd(Assign(str, e), state) -> let st = fun s -> (if s = str then eval_expr state e else state s) in St st
  | Cmd(Seq(a, b), state) -> (match trace1 (Cmd(a,state)) with
        St st -> Cmd(b,st)
      | Cmd(a1, st) -> Cmd(Seq(a1,b),st))
  | Cmd(If(condition, a, b), state) -> (match eval_expr state condition with
        Bool true -> Cmd(a, state)
      | Bool false -> Cmd(b, state)
      | _ -> raise NoRuleApplies;
      )
  | Cmd(While(condition, a), state) -> (match eval_expr state condition with
        Bool true -> Cmd(Seq(a, While(condition, a)), state)
      | Bool false -> St state
      | _ -> raise NoRuleApplies 
      )
  | _ -> raise NoRuleApplies
;;

(**********************************************************************
 trace_rec : int-> conf -> conf list

 Usage: trace_rec n t performs n steps of the small-step semantics

 **********************************************************************)

let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1 t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t]

(**********************************************************************
 trace : int -> cmd -> conf list

 Usage: trace n t performs n steps of the small-step semantics
 **********************************************************************)

let trace n t = trace_rec n (Cmd(t,bot))

