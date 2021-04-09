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



(*Converti un opérateur en string
  @Param v => operator
*)
let change_op_in_expression v =
  match v with
  | Plus ->  "+";
  | Minus ->"-";
  | Mult -> "*";
  | Div -> "/";
;;


(* converti un charactère en string*)
let string_of_char = String.make 1 ;;

(*Converti un arbre en une list de string
@Param tree => tree
@Param lst => string list
*)
let abr_to_list tree =  
let rec abr_to_list_bis( tree, lst) =
  match tree with 
  | Binary(v, l, r) -> if v = Mult
                      then match l with
                      |Cst(c) -> ( match r with
                                |Cst(a) -> "("::string_of_int(a)::change_op_in_expression v::string_of_int(c)::")"::lst
                                |Var(a) -> "("::string_of_char a::change_op_in_expression v::string_of_int(c)::")"::lst
                                |Binary(s, q, d) -> abr_to_list_bis( r, change_op_in_expression v::string_of_int(c)::")"::lst) )
                      |Var(c) -> ( match r with
                                |Cst(a) -> "("::string_of_int(a)::change_op_in_expression v::string_of_char c::")"::lst
                                |Var(a) -> "("::string_of_char a::change_op_in_expression v::string_of_char c::")"::lst
                                |Binary(s, q, d) -> abr_to_list_bis( r, change_op_in_expression v::string_of_char c::")"::lst) )
                      |Binary(s, q, d) -> ( match r with
                                          |Cst(a) -> abr_to_list_bis( l, "("::change_op_in_expression v::string_of_int(a)::lst)
                                          |Var(a) -> abr_to_list_bis( l, "("::change_op_in_expression v::string_of_char a::lst)
                                          |Binary(s, q, d) -> ["("]@abr_to_list_bis( r, change_op_in_expression v::lst@["("]@abr_to_list_bis(l, lst)) )
                      else if v = Div
                      then match l with
                      |Cst(c) -> ( match r with
                                |Cst(a) -> "("::string_of_int(a)::change_op_in_expression v::string_of_int(c)::")"::lst
                                |Var(a) -> "("::string_of_char a::change_op_in_expression v::string_of_int(c)::")"::lst
                                |Binary(s, q, d) -> abr_to_list_bis( r, change_op_in_expression v::string_of_int(c)::lst) )
                      |Var(c) ->( match r with
                                |Cst(a) -> "("::string_of_int(a)::change_op_in_expression v::string_of_char c::")"::lst
                                |Var(a) -> "("::string_of_char a::change_op_in_expression v::string_of_char c::")"::lst
                                |Binary(s, q, d) -> abr_to_list_bis( r, change_op_in_expression v::string_of_char c::"("::lst) )
                      |Binary(s, q, d) -> ( match r with
                                        |Cst(a) -> abr_to_list_bis( l,")"::change_op_in_expression v::string_of_int(a)::lst)
                                        |Var(a) -> abr_to_list_bis( l, ")"::change_op_in_expression v::string_of_char a::lst)
                                        |Binary(s, q, d) -> ["("]@abr_to_list_bis( r, change_op_in_expression v::lst@["("]@abr_to_list_bis(l, lst)) )
                      else match l with
                      |Cst(c) ->( match r with
                                |Cst(a) -> "("::string_of_int(a)::change_op_in_expression v::string_of_int(c)::")"::lst
                                |Var(a) -> "("::string_of_char a::change_op_in_expression v::string_of_int(c)::")"::lst
                                |Binary(s, q, d) -> abr_to_list_bis( r, change_op_in_expression v::")"::lst) )
                      |Var(c) -> ( match r with
                                |Cst(a) -> "("::string_of_int(a)::change_op_in_expression v::string_of_char c::")"::lst
                                |Var(a) -> "("::string_of_char a::change_op_in_expression v::string_of_char c::")"::lst
                                |Binary(s, q, d) -> abr_to_list_bis( r, change_op_in_expression v::")"::lst) )
                      |Binary(s, q, d) -> ( match r with
                                          |Cst(a) -> "("::abr_to_list_bis( l, change_op_in_expression v::string_of_int(a)::")"::lst)
                                          |Var(a) -> "("::abr_to_list_bis( l, change_op_in_expression v::string_of_char a::")"::lst)
                                          |Binary(s, q, d) -> ["("]@abr_to_list_bis( r, change_op_in_expression v::lst@abr_to_list_bis(l, lst)) )
in abr_to_list_bis(tree, [])
;; 
                                          

(*Fonction d'affichage sur la sortie
@Param lst => string list
*)
let rec display lst = 
match lst with
| [] -> print_newline()
|h::t -> (print_string(h); display t)
;;


               

let tree = Binary(Plus, Binary(Div, Cst(5), Var('x')), Binary(Mult, Cst(9), Binary(Minus, Cst(19), Cst(36))));;
let tree2 = simplify tree;;

let lst = abr_to_list tree;;

let lst2 = abr_to_list tree2;;
display lst;;
display lst2;;