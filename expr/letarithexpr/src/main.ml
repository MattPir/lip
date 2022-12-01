open Ast

type exprval = Bool of bool | Nat of int;;


(* convert is_nv value to int *)
let rec int_of_nat = function
    Zero -> 0
  | Succ n -> 1 + int_of_nat n
  | _ -> failwith "int_of_nat on non-nat"
;;

let rec bool_of_bool = function
    True -> true
  | False -> false
  | And(a,b) -> bool_of_bool a && bool_of_bool b
  | Or(a,b) -> bool_of_bool a || bool_of_bool b
  | Not(a) -> bool_of_bool a
  | _-> failwith "bool_of_bool on non-bool"
;;
let string_of_val = function
    Bool b -> if b then "true" else "false"
  | Nat n -> string_of_int n

let rec string_of_expr = function
    True -> "true"
  | False -> "false"
  | Not(e) -> "not " ^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2                    
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "0"
  | Succ(e) -> "succ(" ^ string_of_expr e ^ ")"
  | Pred(e) -> "pred(" ^ string_of_expr e ^ ")"
  | IsZero(e) -> "iszero(" ^ string_of_expr e ^ ")"
  | Var(s) -> "var(" ^ s ^ ")"
  | Let(s, e1, e2) -> "let(" ^ s ^ "= " ^ string_of_expr e1 ^ " in " ^ string_of_expr e2 ^")"

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(******************************************************************************)
(*                            Small-step semantics                            *)
(******************************************************************************)

exception NoRuleApplies
exception PredOfZero

let rec is_nv = function
    Zero -> true
  | Succ(e) -> is_nv e
  | _ -> false
;;

let rec is_bl = function
    True -> true
  | False -> true
  | And(a,b) -> is_bl a && is_bl b
  | Or(a,b) -> is_bl a && is_bl b
  | Not(a) -> is_bl a
  | _ -> false
;;


let rec trace1 r = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(e0,e1,e2) -> let e0' = trace1 r e0 in If(e0',e1,e2)
  | Not(True) -> False
  | Not(False) -> True
  | Not(e) -> let e' = trace1 r e in Not(e')
  | And(True,e) -> e
  | And(False,_) -> False
  | And(e1,e2) -> let e1' = trace1 r e1 in And(e1',e2)
  | Or(True,_) -> True
  | Or(False,e) -> e
  | Or(e1,e2) -> let e1' = trace1 r e1 in Or(e1',e2)
  | Succ(e) -> let e' = trace1 r e in Succ(e')
  | Pred(Zero) -> raise NoRuleApplies
  | Pred(Succ(e)) when is_nv e -> e
  | Pred(e) -> let e' = trace1 r e in Pred(e')
  | IsZero(Zero) -> True
  | IsZero(Succ(e)) when is_nv e -> False    
  | IsZero(e) -> let e' = trace1 r e in IsZero(e')
  | Var(s) -> r s
  | Let(s, Succ(e1), e2) -> let r1 str = if s = str then trace1 r (Succ(e1)) else r str in trace1 r1 e2
  | Let(s, Pred(e1), e2) -> let r1 str = if s = str then trace1 r (Pred(e1)) else r str in trace1 r1 e2
  | Let(s, True, e2) -> let r1 str = if s = str then True else r str in trace1 r1 e2
  | Let(s, False, e2) -> let r1 str = if s = str then False else r str in trace1 r1 e2
  | Let(s, Zero, e2) -> let r1 str = if s = str then Zero else r str in trace1 r1 e2
  | Let(s, e1, e2) -> let e1' = trace1 r e1 in trace1 r (Let(s, e1', e2))
  | _ -> raise NoRuleApplies
;;


let rec trace e = try
  let rho = function _ -> failwith "error" in
    let e' = trace1 rho e
    in e::(trace e')
  with NoRuleApplies -> [e]
;;

(******************************************************************************)
(*                              Big-step semantics                            *)
(******************************************************************************)

exception TypeError of string



let eval e =
  let rho = function _ -> failwith "error" in
  let rec eval1 rho = function
    True -> Bool true
  | False -> Bool false
  | Not(e) -> (match eval1 rho e with
        Bool b -> Bool (not b)
      | _ -> raise (TypeError "Not on nat")
    )
  | And(e1,e2) -> (match (eval1 rho e1,eval1 rho e2) with
        (Bool b1,Bool b2) -> Bool (b1 && b2)
      | _ -> raise (TypeError "Or on nat")
    )
  | Or(e1,e2) -> (match (eval1 rho e1, eval1 rho e2) with
        (Bool b1,Bool b2) -> Bool (b1 || b2)
      | _ -> raise (TypeError "Or on nat")
    ) 
  | If(e0,e1,e2) -> (match eval1 rho e0 with
        Bool b -> if b then eval1 rho e1 else eval1 rho e2
      | _ -> raise (TypeError "If on nat guard")
    )
  | Zero -> Nat 0
  | Succ(e) -> (match eval1 rho e with
        Nat n -> Nat (n+1)
      | _ -> raise (TypeError "Succ on bool")
    )
  | Pred(e) -> (match eval1 rho e with
      | Nat n when n>0 -> Nat (n-1)
      | _ -> raise (TypeError "pred on 0")
    )
  | IsZero(e) -> (match eval1 rho e with
      | Nat n -> Bool (n=0)
      | _ -> raise (TypeError "IsZero on bool")
    )
  | Var(e) -> rho e
  | Let(s, e1, e2) -> let rho = fun str -> if str = s then eval1 rho e1 else rho str in eval1 rho e2
in eval1 rho e
;;
