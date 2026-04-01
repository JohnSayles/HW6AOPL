
(* This includes the json data type defined in json.ml. 
   You need to understand the json type and related concepts
   well to do this assignment. *)
include Json

(* provided helper function that deduplicates a list *)
let dedup xs = List.sort_uniq compare xs

(* provided helper function that sorts a given list *)
let sort xs = List.sort compare xs

(* provided helper function to convert a float to a string *)
(* OCaml's string_of_float is not quite RFC compliant due to its tendency
   to output whole numbers with trailing decimal points without a zero.
   But, printf does the job how we want. *)
let json_string_of_float f =
  Printf.sprintf "%g" f


(* Note:
   In the starter code, function arguments are prefixed with '_'
   (for example, _i,  _sep, -ss in the first two functions). This 
   indicates that the arguments are intentionally unused at the moment 
   and avoids compiler warnings. When you implement the function and 
   need to use an argument, simply remove the '_' prefix from its name.

   None of the functions in the starter code include the keyword 'rec'.
   If you implement a function that is recursive
   (that is, it calls itself directly or indirectly), you must add the
   keyword 'rec' to its definition. A function that only calls a
   separate recursive helper function that does not lead to a call itself, 
   is not recursive and should not be marked 'rec'.
*)  

(* 1 *)
let make_silly_json i =
  let rec make n =
    match n with
    | 0 -> []
    | _ -> Object [("n", Num (float_of_int n)); ("b", True)] :: make (n - 1)
  in Array (make i)
;;

(* 2 *)
let concat_with (sep, ss) =
  let rec cc l =
    match l with
    | [] -> ""
    | [w] -> w
    | w :: wa -> w ^ sep ^ cc wa
  in cc ss
;;

(* 3 *)
let quote_string s = 
  "\"" ^ s ^ "\""
;;

(* 4 *)
let rec string_of_json j =
  let rec array_string s = 
    match s with
    | [] -> []
    | s :: tail -> string_of_json s :: array_string tail
  in let rec object_string object_string =
    match object_string with
    | [] -> []
    | (x, y) :: tail -> (quote_string x ^ " : " ^ string_of_json y) :: object_string tail
  in match j with
    | Null -> "null"
    | True -> "true"
    | False -> "false"
    | Num n -> json_string_of_float n
    | String s -> quote_string s
    | Array a -> "[" ^ concat_with (", ", array_string a) ^ "]"
    | Object o -> "{" ^ concat_with (", ", object_string o) ^ "}" 
;;


(* 5 *)
let take (n,xs) = 
  let rec make c lst =
    match (c, lst) with
    | (0,_) -> []
    | (_, x :: tail) -> x :: make (c-1) tail
    | _ -> [] 
  in 
  make n xs
;;

(* 6 *)
let rec firsts xs = 
  match xs with
  | [] -> []
  | (a,_) :: tail -> a :: firsts tail
;;

(* 7 *)
(* Suppose xs has type (int * int) list,
 and let n be an integer between 0 and the length of xs (inclusive). 
 Consider the expressions firsts (take (n, xs)) and take (n, firsts xs). 

 Do these expressions always evaluate to the same value? 
 
 Yes, they always evaluate to the same value.

 Firsts (take (n, xs)) works by taking the first n tuples, then firsts takes the first number from each tuple
 take (n, firsts xs) works by taking the first number from each tuple, then take takes the first number from the list made by firsts
 These are equivalent, because you are always end up with a list of the first elements of a list of tuples

 Also, which of the two expressions has better time complexity? 

 Firsts (take (n, xs)) is faster because you don't have to operate on all the tuples, only the first n
 like you would do with take(n, firsts xs)

 Give the Big-O running time for each expression in terms of n and the length of the list, and briefly explain your rationale.
 
 The run-time for Firsts (take (n, xs)) is O(n) + O(n) = O(n)
 - The first O(n) is exctracting the first n tuples
 - The second O(n) is taking the first n first elements of the tuples
 The run-time for Take (n, firsts(xs)) is O(len(xs)) + O(n) = O(len(xs))
 - The first O(len(xs)) is putting the first element out of every tuple in the list into a list
 - The second O(n) is taking the first n elements out of the list
 *)


(* 8 *)
let rec assoc (k, xs) =
  match xs with
  | [] -> None
  | (k1,v1) :: tail -> if k = k1 then Some v1 else assoc (k, tail)
;;


(* 9 *)
let dot (j, f) = 
  match j with
  | Object o -> assoc (f, o)
  |_ -> None
;;
  
(* 10 *)
let rec dots (j, fs) =
  match fs with
  | [] -> Some j
  | f :: tail -> match dot (j, f) with
                | None -> None
                | Some nj -> dots (nj, tail)
;;
