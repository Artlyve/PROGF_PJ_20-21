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


let to_tree (token, tree_list : token * tree list) =
  let hd::tl = tree_list in 
  let hd2::tl2 = tl in 
  match token with
  | Add       -> Binary(Plus, hd, hd2)::tl2
  | Subtract  -> Binary(Minus, hd, hd2)::tl2
  | Multiply  -> Binary(Mult, hd, hd2)::tl2
  | Divide    -> Binary(Div, hd, hd2)::tl2
  | Minus     -> Unary(hd)::tl
  | _ -> failwith("to_tree : token non valide")
  
;;

let rec parse_bis (exp,t_list) =
  if exp = []
  then failwith("parse_bis : fin d'expression attendue")
  else
    let hd::tl = exp in
    match hd with
    | Variable(x) -> parse_bis( tl, Var(x)::t_list)
    | Number(x)   -> parse_bis( tl, Cst(x)::t_list)
    | End         -> List.hd t_list
    | _           -> parse_bis( tl, to_tree(hd, t_list))
  ;;


let parse exp =
  parse_bis(exp,[])
  ;;



  let a = "4";; 

  Format.printf "test %s" a;;
  
  let change_op_in_expression v =
    match v with
    | Plus -> Format.printf "+";
    | Minus -> Format.printf "-";
    | Mult -> Format.printf "*";
    | Div -> Format.printf "/";
    ;;
  
    let change_tree_in_expression v =
      match v with
      | Unary(t)-> Format.printf "";
      | Cst(t)-> Format.printf "%d" t;
      | Var(t)-> Format.printf "%c" t;
      | Binary (t,t1,t2)-> Format.printf "";
      ;;
  
  let z = Cst(5);;
  Format.printf "%d" z;;
  change_tree_in_expression z;;
  let abr_to_list tree =
    let rec abr_to_list_bis tree lst =
     match tree with 
     | Binary(v, l, r) -> match l with 
                        | Cst c -> 
                        let p = c in abr_to_list_bis r (change_tree_in_expression p)::lst 
                        | Var c -> let p = c in abr_to_list_bis r (change_tree_in_expression p)::lst
                        | Unary c -> abr_to_list_bis r lst
                        | Binary(s, q, d) -> abr_to_list_bis q lst
                        
  in abr_to_list_bis tree []
    ;;