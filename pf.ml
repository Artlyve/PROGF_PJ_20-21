#load "expression_scanner.cmo";;
open Expression_scanner;;

type operator = |Plus | Minus | Mult | Div
type tree = 
  | Var of char
  | Cst of int
  | Unary of tree
  | Binary of operator * tree * tree
;;

input_to_token_list();;
let f = string_to_token_list("a");;

let t x = x = Add;;
t (List.hd f);;

let aridity t =
  match t with
  | Add -> 2
  | Subtract -> 2
  | Multiply -> 2
  | Divide -> 2
  | Minus -> 2
  | Variable t-> 0
  | Number t-> 0
  | End -> 1
;;

let a = Binary(Plus, Cst(2), Cst(2));;


let parse exp =
  let rec parse_aux exp_aux arb = 
  match exp_aux with
  |[] -> failwith"Expression Vide !"
  |h::t -> if aridity h = 2
            then match abr with
            | Unary -> Binary(h, Unary, Unary)
            | Binary(v, l, r) -> Binary(h, Binary(v, l, r), Unary )
  ;; 