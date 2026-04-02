# AI Usage Report

We used ChatGPT to give advice for approaching the parse_json problem. First we gave it the problem's description, then we gave it the JSON standard we were using for the assignment.

It gave us the advice to use mutual recursion with helper functions rather than nest functions.

We also asked the chatbot several followup questions asking it to clarify whether the parse_json function was meant to parse the entire token list in one go or just meant to parse the first token in the list.

## List of Our Prompts

- We need help completing this OCaml function from our assignment. We would like a step-by-step explanation of how it works. Here is the function outline: 

    - "The main parsing function. Takes a token list and returns a pair of the parsed json value and the remaining token list. 
    This function handles all JSON value types: numbers, strings, booleans, arrays, and objects. 
    If the token list represents a complete and valid JSON value, parse_json returns that value paired with an empty token list. 
    Raises a syntax error if the token list does not represent a valid JSON value. 
    Parsing JSON is inherently recursive since arrays and objects contain nested JSON values. 
    Your implementation of parse_json will need to call itself recursively at suitable places, and you also need to design helper functions to handle arrays and objects. 
    Since these helpers need to make recursive calls back to parse_json, you have two design choices: 
    Local functions — defined inside parse_json using nested let ... in expressions. These are private and cannot be tested directly. 
    Mutually recursive functions — defined at the top level alongside parse_json using let rec ... and ... and ... These are accessible at the top level and can be tested independently. 
    The choice is yours. Keep in mind that if you use local functions, you will need to test parse_json extra thoroughly to ensure your helpers are working correctly, covering all JSON value types including nested arrays and objects. 
    Regardless of your design choice, you are required to add functional programming style comments to all your functions, including helper functions, whether they are local or mutually recursive. parse_string and expect are defined at the top level because they handle simple, self-contained tasks — consuming a string literal and consuming a specific token respectively — that do not require recursion back to parse_json. 
    They are also useful independently and can be tested on their own." 

- Here is the JSON standard we use: 

    - "type json = | Num of float | String of string | False | True | Null | Array of json list | Object of (string * json) list" and here are the two functions I already made that need to be used in this function: "let expect (exp, list) = match list with | h :: tail when exp = h -> tail | h :: _ -> syntax_error(h, "unexpected token") | [] -> lexical_error("No input")" and "let parse_string lst = match lst with | StringLit h :: t -> (h, t) | _ -> raise (SyntaxError "expected a string literal")"

- How do we make sure our solution handles the requirement below?

    - "If the token list represents a complete and valid JSON value, parse_json returns that value paired with an empty token list. Raises a syntax error if the token list does not represent a valid JSON value."

- I'm writing many OCaml test cases for a homework assignment. The functions I'm writing test cases for are called parse_string, expect, and parse_json. Attached are the files that have the original functions. Break down writing these test cases as a step-by-step process.