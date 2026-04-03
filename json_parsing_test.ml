open OUnit2
open Json
open Hw3
open Json_parsing
open Json_structures.Parsed_small_bus
open Json_structures.Parsed_medium_bus
open Json_structures.Parsed_complete_bus


(* This file provides some example tests, and you need to add more. *)

(* [make_test_1arg test_info f argument expected] is a helper function that
   creates a test case for function [f] with the input [argument] and
   the [expected] output. [test_info] gives a test description, useful
   for identifying the test case in the error message if the test is
   not passed. *)
let make_test_1arg test_info f argument expected =
  test_info >:: (fun _ -> assert_equal expected (f argument))

let make_exn_test_1arg test_info f argument expected_exn =
  test_info >:: (fun _ -> assert_raises expected_exn (fun () -> f argument))


(* 1
let tests_consume_string_literal = "test suite for consume_string_literal" >::: [
  make_test_1arg
    "consume_string_literal: hello"
    consume_string_literal
    (char_list_of_string "\"hello\"")
    (StringLit "hello", []);
  make_test_1arg
    "consume_string_literal: hello   "
    consume_string_literal
    (char_list_of_string "\"hello\"   ")
    (StringLit "hello", [' '; ' '; ' ']);
  make_test_1arg
    "consume_string_literal: hello   "
    consume_string_literal
    (char_list_of_string "\"hello   \"")
    (StringLit "hello   ", []);
  make_test_1arg
    "consume_string_literal: hello! "
    consume_string_literal
    (char_list_of_string "\"hello\"! ")
    (StringLit "hello", ['!'; ' ']);
  make_test_1arg
    "consume_string_literal: 12345"
    consume_string_literal
    (char_list_of_string "\"12345\"")
    (StringLit "12345", []);
] (* add more tests *)


let tests_consume_string_literal_exceptions =
  "test suite for consume_string_literal exceptions" >::: [
    make_exn_test_1arg
      "no opening quote"
      consume_string_literal
      (char_list_of_string "hello")
      (LexicalError "Lexical error: Expecting string literal. No opening quote.");
    make_exn_test_1arg
      "single quote instead of double quote"
      consume_string_literal
      (char_list_of_string "\'hello\'")
      (LexicalError "Lexical error: Expecting string literal. No opening quote.");
    make_exn_test_1arg
      "no ending quote"
      consume_string_literal
      (char_list_of_string "\"hello")
      (LexicalError "Lexical error: Unterminated string literal.");
    make_exn_test_1arg
      "character outside of quotes"
      consume_string_literal
      (char_list_of_string "!\"hello\"")
      (LexicalError "Lexical error: Expecting string literal. No opening quote.");
  ]


(* 2 *)
let tests_consume_keyword = "test suite for consume_keyword" >::: [
  make_test_1arg
    "consume_keyword: true; "
    consume_keyword
    (char_list_of_string "true; ")
    (TrueTok, [';';' ']);
  make_test_1arg
    "consume_keyword: false ; true"
    consume_keyword
    (char_list_of_string "false ; true")
    (FalseTok, [' '; ';'; ' '; 't'; 'r'; 'u'; 'e']);
  make_test_1arg
    "consume_keyword: null: true"
    consume_keyword
    (char_list_of_string "null: true")
    (NullTok, [':'; ' '; 't'; 'r'; 'u'; 'e']);
] (* add more tests *)

let tests_consume_keyword_exceptions = "test suite for consume_keyword exceptions" >::: [
  make_exn_test_1arg
    "extra char in front of keyword"
    consume_keyword
    (char_list_of_string "ftrue")
    (LexicalError "Lexical error: Expecting keyword of true, false, or null.");
  make_exn_test_1arg
    "extra char after keyword"
    consume_keyword
    (char_list_of_string "truef")
    (LexicalError "Lexical error: Expecting keyword of true, false, or null.");
  make_exn_test_1arg
    "extra keywords"
    consume_keyword
    (char_list_of_string "nullnullnull")
    (LexicalError "Lexical error: Expecting keyword of true, false, or null.");
]

(* 4 *)
let tests_tokenize = "test suite for tokenize" >::: [
  make_test_1arg
    "tokenize: empty object"
    tokenize
    ""
    [];
  make_test_1arg
    "tokenize: simple object"
    tokenize
    "{ \"x\" : true }"
    [LBrace; StringLit "x"; Colon; TrueTok; RBrace];
  make_test_1arg
    "tokenize: complex object"
    tokenize
    "{ 
      \"abc\" : 123,
      \"my_list\" : [\"m\", \"y\"],
      \"my_object\" : {
                      \"item\" : \"my_item\"
                    }
    }"
    [LBrace; 
      StringLit "abc"; Colon; NumLit "123"; Comma;
      StringLit "my_list"; Colon; LBracket; StringLit "m"; Comma; StringLit "y"; RBracket; Comma;
      StringLit "my_object"; Colon; LBrace;
        StringLit "item"; Colon; StringLit "my_item";
      RBrace;
    RBrace];
  make_test_1arg
    "tokenize: nested lists"
    tokenize
    "{
      \"primary_colors\" :
        [\"blue\" : [\"indigo\", \"purple\"]],
        [\"yellow\" : [\"chartreuse\", \"lime\"]],
        [\"red\" : [\"pink\", \"orange\"]]
    }"
    [LBrace; StringLit "primary_colors"; Colon;
      LBracket; StringLit "blue"; Colon; LBracket; StringLit "indigo"; Comma; StringLit "purple"; RBracket; RBracket; Comma;
      LBracket; StringLit "yellow"; Colon; LBracket; StringLit "chartreuse"; Comma; StringLit "lime"; RBracket; RBracket; Comma;
      LBracket; StringLit "red"; Colon; LBracket; StringLit "pink"; Comma; StringLit "orange"; RBracket; RBracket;
    RBrace];
  make_test_1arg
    "tokenize: malformed JSON object"
    tokenize
    "{\"bad formatting\" : :}"
    [LBrace; StringLit "bad formatting"; Colon; Colon; RBrace];
] (* add more tests *)

let tests_tokenize_exceptions = "test suite for tokenize exceptions" >::: [
  make_exn_test_1arg
    "wrong kind of characters"
    tokenize
    "@@@"
    (LexicalError "Lexical error: Unknown character @");
] *)

(* 5 *)
let tests_parse_string = "test suite for parse_string" >::: [
  make_test_1arg
    "parse_string: string literal"
    parse_string 
    [StringLit "hello"; TrueTok]
    ("hello", [TrueTok]);
  make_test_1arg
    "parse_string: larger string"
    parse_string
    [StringLit "hello world"; FalseTok]
    ("hello world", [FalseTok]);
  make_test_1arg
    "parse_string: fancy escape characters"
    parse_string
    [StringLit "/'woahhhh/'"; LBrace; RBrace]
    ("/'woahhhh/'", [LBrace; RBrace]);
  make_test_1arg
    "parse_string: one item in token list"
    parse_string
    [StringLit "myString"]
    ("myString", []);
]

let tests_parse_string_exceptions = "test suite for parse_string exceptions" >::: [
  make_exn_test_1arg
    "parse_string: non-string token"
    parse_string
    [FalseTok]
    (SyntaxError "expected a string literal");
  make_exn_test_1arg
    "parse_string: empty list"
    parse_string
    []
    (SyntaxError "expected a string literal");
]

(* 6 *)
let tests_expect = "test suite for expect" >::: [
  make_test_1arg
    "expect: finds expected LBrace"
    expect 
    (LBrace, [LBrace; RBrace])
    [RBrace];
  make_test_1arg
    "expect: one item in list"
    expect 
    (LBrace, [LBrace])
    [];
]

let tests_expect_exceptions = "test suite for expect exceptions" >::: [
  make_exn_test_1arg
    "expect: wrong token given"
    expect 
    (LBrace, [RBrace; LBrace])
    (SyntaxError "Syntax error at }: unexpected token");
  make_exn_test_1arg
    "expect: empty list"
    expect 
    (LBrace, [])
    (LexicalError "Lexical error: No input")
]

(* 7 *)

let tests_parse_json = "test suite for parse_json" >::: [
  make_test_1arg
    "parse_json: string"
    parse_json [StringLit "hello"]
    (String "hello", []);
  make_test_1arg
    "parse_json: number"
    parse_json [NumLit "12345"]
    (Num 12345.0, []);  
  make_test_1arg
    "parse_json: TrueTok"
    parse_json [TrueTok]
    (True, []);
  make_test_1arg
    "parse_json: array"
    parse_json [LBracket; StringLit "my item"; RBracket]
    (parse_array [StringLit "my item"; RBracket]);
  make_test_1arg
    "parse_json: object"
    parse_json [LBrace; StringLit "my item"; Colon; TrueTok; RBrace]
    (parse_object [StringLit "my item"; Colon; TrueTok; RBrace]);
]

let tests_parse_json_exceptions = "test suite for parse_json exceptions" >::: [
  make_exn_test_1arg
    "parse_json: empty list"
    parse_json []
    (SyntaxError "JSON Value expected, found end of input");
  make_exn_test_1arg
    "parse_json: invalid JSON"
    parse_json [LBrace; StringLit "my item"; Colon; TrueTok]
    (LexicalError "Lexical error: No input");
]

let test_bus1 = "test suite for buses" >::: [
  "small bus" >:: (fun _ ->
    assert_equal
      ~printer:string_of_json
      small_bus_positions
      (parse_from_file "small_bus.json")
  );
]

let test_bus2 = "test suite for buses" >::: [
  "medium bus" >:: (fun _ ->
    assert_equal
      ~printer:string_of_json
      medium_bus_positions
      (parse_from_file "medium_bus.json")
  );
]

let test_bus3 = "test suite for buses" >::: [
  "big bus" >:: (fun _ ->
    assert_equal
      ~printer:string_of_json
      complete_bus_positions
      (parse_from_file "complete_bus.json")
  );
]

let all_tests = "all tests" >::: [
  (* tests_consume_string_literal;
  tests_consume_string_literal_exceptions;
  tests_consume_keyword;
  tests_consume_keyword_exceptions;
  tests_tokenize;
  tests_tokenize_exceptions; *)
  tests_parse_string;
  tests_parse_string_exceptions;
  tests_expect;
  tests_expect_exceptions;
  tests_parse_json;
  tests_parse_json_exceptions;
  test_bus1;
  test_bus2;
  test_bus3;
]

(* Run all tests *)
let () =
  run_test_tt_main all_tests