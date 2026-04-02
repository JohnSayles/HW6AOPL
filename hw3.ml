
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
(**[make_silly_json] is a JSON array of JSON objects with two fields. The first
field is a float starting at 1 and ending at [i]. The second field is a bool [True].
There are always [i] objects.*)
let make_silly_json _i =
  let rec make_silly_json_tr _i acc =
    match _i with
    | 0 -> Array acc
    | n -> make_silly_json_tr (n-1) (acc @ [(Object [("n", Num (float_of_int n)); ("b", True)])])
  in make_silly_json_tr _i [];;


(* 2 *)
(** [concat_with] takes a string and a list of strings. It returns a string 
made of the list of strings separated by the first string. **)
let concat_with (_sep, _ss) =
  let rec concat_with_tr (_sep, _working_list) acc first =
    match _working_list with
    | [] -> acc
    | h::t -> concat_with_tr (_sep, t) (acc ^ (if first then "" else _sep) ^ h) false
  in concat_with_tr (_sep, _ss) "" true;;


(* 3 *)
(** [quote_string] takes a string and returns that string enclosed with double quotes. **)
let quote_string _s =
  "\"" ^ _s ^ "\"";;


(* 4 *)
(** [string_of_json_num] is a helper function that takes a JSON Num and returns a string. **)
let string_of_json_num n =
  if float_of_int (int_of_float n) = n
  then string_of_int (int_of_float n)
  else string_of_float n;;

(** [string_of_json_array] is a helper function that returns a string representation of an
array and also ensures that each item inside the array is also represented as a string. **)
let rec string_of_json_array arr acc =
  match arr with
  | [] -> "[" ^ (concat_with (", ", (List.rev acc))) ^ "]"
  | h::t -> string_of_json_array t ((string_of_json_rec h) :: acc)

(** [string_of_object_fields] is a helper function that returns a string representation of
a JSON object while also ensuring that each field inside is represented as a string. **)
and string_of_object_fields fields acc =
  match fields with
  | [] -> "{" ^ (concat_with (", ", (List.rev acc))) ^ "}"
  | (k,v)::t -> string_of_object_fields t (((quote_string k) ^ " : " ^ (string_of_json_rec v)) :: acc)

(** [string_of_json_rec] takes a JSON and returns a string representation of it. For more
complicated tasks like representing arrays or objects, it calls on its helper functions. **)
and string_of_json_rec _j =
  match _j with
  | Num n -> string_of_json_num n
  | String s -> quote_string s
  | False -> "false"
  | True -> "true"
  | Null -> "null"
  | Array a -> string_of_json_array a []
  | Object obj -> string_of_object_fields obj []

(** [string_of_json] simply calls a recursive function that handles all the work of
representing the JSON as a string. **)
let string_of_json _j =
  string_of_json_rec _j;;