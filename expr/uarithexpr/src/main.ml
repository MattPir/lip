open Ast


let rec string_of_expr = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Not(e) -> "Not " ^ string_of_expr e
  | And(e0,e1) -> (string_of_expr e0) ^ " and " ^ string_of_expr e1
  | Or(e0,e1) -> (string_of_expr e0) ^ " or " ^ string_of_expr e1
  | Zero -> "0"
  | Succ(e) -> "succ " ^ string_of_expr e
  | Pred(e) -> "pred " ^ string_of_expr e
  | IsZero(e) -> "iszero " ^ string_of_expr e
;;

let string_of_val = function
    n -> string_of_int n
;;



let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

exception NoRuleApplies
  
let rec trace1 = function
    True -> Succ(Zero)
  | False -> Zero
  | Not(False) -> True  
  | Not(True) -> False
  | Not(Zero) -> Not(False)
  | Not(e1) -> let e = trace1 e1 in Not(e)
  | And(Succ(_), Succ(_)) -> Succ(Zero)
  | And(Succ(Zero), e) -> e
  | And(Zero, _) -> False
  | And(e0,e1) -> let e = trace1 e0 in And(e,e1)
  | Or(_, Succ(_)) -> True
  | Or(Succ(_),_) -> True
  | Or(False,e1) -> e1
  | Or(e0,e1) -> let e = trace1 e0 in Or(e,e1)
  | If(Succ(_),e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 e0 in If(e0',e1,e2)
  | Succ(e) -> Succ(trace1 e)
  | Pred(Succ(e)) -> e
  | Pred(Zero) -> Zero
  | Pred(e) -> Pred(trace1 e)
  | IsZero(Zero) -> True
  | IsZero(Succ(_)) -> False
  | IsZero(e) -> IsZero(trace1 e)
  | _ -> raise NoRuleApplies
;;

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

let rec eval = function
    True -> 1
  | False -> 0
  | Not(e0) -> if eval e0 = 0 then 1 else 0
  | And(e0, e1) ->  if eval e0 = 0 || eval e1 = 0 then 0 else 1
  | Or(e0, e1) -> if eval e1 >= 1 || eval e0 >= 1 then 1 else 0
  | If(e0,e1,e2) -> if eval e0 = 1 then eval e1 else eval e2
  | Zero -> 0
  | Succ(e) -> 1 + eval e
  | Pred(e) -> let n = eval e in if n = 0 then 0 else n - 1
  | IsZero(e) -> if eval e = 0 then 1 else 0 (*eval(Not(e))*)
;;

let rec is_nv e = match e with
    Zero -> true
  | Succ(Zero) -> true
  | Succ(Succ(n)) -> is_nv (Succ(n))
  | _ -> false
;;