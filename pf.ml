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



let parse exp = 
  
  ;;