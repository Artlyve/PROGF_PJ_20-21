#load "expression_scanner.cmo";;
open Expression_scanner;;

type operator = |Plus | Minus | Mult | Div
type tree = 
  | Var of char
  | Cst of int
  | Unary of tree
  | Binary of operator * tree * tree
;;



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



  let simplify_bis t_tree =
    match t_tree with
    | Cst(x)  -> t_tree
    | Var(x)  -> t_tree
    | Unary(x)-> t_tree
    | Binary(op,lson,rson) -> match op with
                              |Plus   -> ( match lson with
                                          |Cst(x) -> (if(x = 0)
                                                      then rson
                                                      else
                                                      match rson with
                                                      |Cst(y) ->  if(y = 0)
                                                                  then lson
                                                                  else t_tree 
                                                      |_ -> t_tree )
                                          |Var(x) -> (match rson with
                                                      |Cst(y) ->  if(y = 0)
                                                                  then lson
                                                                  else t_tree 
                                                      |_ -> t_tree )
                                          |_ -> t_tree)
                              |Minus  -> ( match lson with
                                          |Cst(x) -> (match rson with
                                                      |Cst(y) ->  if (x=y)
                                                                  then Cst(0)
                                                                  else t_tree
                                                      |Var(y) ->  t_tree
                                                      |_ -> t_tree )
                                          |Var(x) -> (match rson with
                                                      |Cst(y) ->  t_tree
                                                      |Var(y) ->  if (x=y)
                                                                  then Cst(0)
                                                                  else t_tree
                                                      |_ -> t_tree )
                                          |_ -> t_tree )
                              |Div    -> ( match lson with
                                          |Cst(x) -> (match rson with
                                                      |Cst(y) ->  if (x=y)
                                                                  then Cst(1)
                                                                  else t_tree
                                                      |Var(y) ->  t_tree
                                                      |_ -> t_tree )
                                          |Var(x) -> (match rson with
                                                      |Cst(y) ->  t_tree
                                                      |Var(y) ->  if (x=y)
                                                                  then Cst(1)
                                                                  else t_tree
                                                      |_ -> t_tree )
                                          |_ -> t_tree )
                              |Mult   -> ( match lson with
                                          |Cst(x) ->  if(x = 0)
                                                      then Cst(0)
                                                      else
                                                        if(x = 1)
                                                        then rson
                                                        else t_tree
                                          |Var(x) -> (match rson with
                                                      |Cst(y) ->  if(y = 0)
                                                                  then Cst(0)
                                                                  else
                                                                    if(y = 1)
                                                                    then lson
                                                                    else t_tree
                                                      |Var(y) -> t_tree
                                                      |_ -> t_tree )
                                          |_ -> t_tree)                          
                              |_ -> failwith("simplify_bis : operateur non reconnu") 
;;

  

let rec simplify tree =
  match tree with
  |Binary(op, lson, rson) -> simplify_bis(Binary(op,simplify(lson),simplify(rson)))
  |_                      -> tree
  ;;



  let a = "4";; 

  Format.printf "test %s" a;;
  
let change_op_in_expression v =
    match v with
    | Plus ->  '+';
    | Minus ->'-';
    | Mult -> '*';
    | Div -> '/';
;;

  
let rec abr_to_list_bis( tree, lst) =
  match tree with 
  | Binary(v, l, r) -> match l with 
                      | Cst(c) -> abr_to_list_bis( r, char_of_int(c)::lst )
                      | Var(c) -> abr_to_list_bis( r, c::lst )
                      | Binary(s, q, d) -> abr_to_list_bis( q, change_op_in_expression s::lst )
                        
  | Binary(v, l, r) -> match r with
                      | Cst(c) -> abr_to_list_bis( l, char_of_int(c)::lst )
                      | Var(c) -> abr_to_list_bis( l, c::lst )
                      | Binary(s, q, d) -> abr_to_list_bis( d, change_op_in_expression s::lst )
;;


let abr_to_list tree =                   
  abr_to_list_bis(tree, [])
  
;;

let tree = Binary(Plus, Cst(5), Cst(5));;
#trace abr_to_list;;
abr_to_list tree;;