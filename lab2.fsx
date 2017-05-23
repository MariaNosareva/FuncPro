open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// почтовый адрес
let email = "nosareva.mv@phystech.edu"



// json.org
type JSON =
  | String of string
  | Number of int
  | Object of (string * JSON) list
  | Array of JSON list
  | Boolean of bool
  | Null

let explode (s : string) = [ for i in s -> i]

type token = 
  | OpenBrace 
  | CloseBrace 
  | OpenBracket
  | CloseBracket
  | Colon
  | Comma
  | Null
  | String of string 
  | Number of int 
  | Boolean of bool

let hex item = ((item >= '0') && (item <= '9') || 
                (item >='a') && (item <= 'f') ||
                (item >= 'A') && (item <='F'))


let rec parseString (acc : string) = function
  | '"' :: t -> (acc, t)  // end of string
  | '\\' :: 'n' :: t -> parseString (acc + "\n") t
  | '\\' :: 'b' :: t -> parseString (acc + "\b") t
  | '\\' :: 'f' :: t -> parseString (acc + "\f") t
  | '\\' :: 'r' :: t -> parseString (acc + "\r") t
  | '\\' :: 't' :: t -> parseString (acc + "\t") t
  | '\\' :: '\\' :: t -> parseString (acc + "\\") t
  | '\\' :: '/' :: t -> parseString (acc + "\/") t
  | '\\' :: 'u' :: a :: b :: c :: d :: t when (hex a) && (hex b) && (hex c) && (hex d) -> 
    parseString (acc + "\u" + a.ToString() + b.ToString() + c.ToString() + d.ToString()) t              
  | c :: t -> parseString (acc + c.ToString()) t
  | _ -> failwith "String parse error"
 
let rec parseNum (acc : string) = function
  | d :: t when (Char.IsDigit d) -> parseNum (acc + (Char.ToString d)) t
  | t -> (acc, t)


let tokenize source = 
  let rec tokenize' acc = function
    | w :: t when Char.IsWhiteSpace w -> tokenize' acc t 
    | '{' :: t -> tokenize' (OpenBrace :: acc) t
    | '}' :: t -> tokenize' (CloseBrace :: acc) t
    | '[' :: t -> tokenize' (OpenBracket :: acc) t
    | ']' :: t -> tokenize' (CloseBracket :: acc) t
    | ':' :: t -> tokenize' (Colon :: acc) t
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t -> tokenize' (Boolean false :: acc) t
    | 't' :: 'r' :: 'u' :: 'e' :: t -> tokenize' (Boolean true :: acc) t
    | 'n' :: 'u' :: 'l' :: 'l' :: t -> tokenize' (Null :: acc) t 
    | ',' :: t -> tokenize' (Comma :: acc) t
    | '\"' :: t -> 
      let s, t' = parseString "" t
      tokenize' ((String s) :: acc) t'
    | d :: t -> 
      let s, t' = parseNum (Char.ToString d) t
      tokenize' ((Number (int s)) :: acc) t'
    | [] -> List.rev acc
    | _ -> failwith "Error in tokenize function"
  tokenize' [] source

tokenize ['{';'"';'\\';'u';'0'; '3';'c';'3';'"';'}']



let parse_t str = 
  let rec parse_t' json = 

    let rec parseObject obj = function
      | CloseBrace :: t -> (Object (List.rev obj)), t
      | Comma :: t -> parseObject obj t
      | token.String key :: Colon :: t ->
        let value, t' = parse_t' t
        parseObject ((key, value) :: obj) t'
      | _ -> failwith "Incorrect object"

    let rec parseArray arr = function
      | CloseBracket :: t -> (Array (List.rev arr)), t
      | Comma :: t -> parseArray arr t
      | obj -> 
        let a, t' = parse_t' obj
        parseArray (a :: arr) t'

    match json with
      | token.Null :: t -> JSON.Null, t
      | token.String s :: t -> JSON.String s, t 
      | token.Number s :: t -> JSON.Number s, t
      | token.Boolean s :: t -> JSON.Boolean s, t
      | OpenBrace :: t -> parseObject [] t
      | OpenBracket :: t -> parseArray [] t
      | _ -> failwith "Error in parse_->parse_t' function"
  
  match parse_t' str with
    | res, [] -> res
    | _, _ -> failwith "Unexpexted end of parsing"

let parse source = parse_t (tokenize (explode source))



let lab3 = 
  let rec lab3' acc = function
    | Object list ->
      let d = List.collect (fun (x, y) -> lab3' [x] y) list
      acc @ d
    | Array arr ->
      let q = List.collect (fun x -> lab3' [] x) arr
      acc @ q
    | _ -> acc @ []
  lab3' []
let (^) l r = sprintf "%s%s" l r
let rem2 (str:string) = if str.Length > 2 then str.Remove(str.Length - 2)
                        else str

let stringify j= 
  let rec stringify' (result:string) = function
    | JSON.Null -> result ^ "null"
    | JSON.Number n-> result ^ n.ToString() 
    | JSON.String s -> result ^ "\"" ^ s ^ "\""
    | JSON.Boolean b -> result ^ (b.ToString()).ToLower()
    | JSON.Array arr -> "[" ^ rem2((List.fold (^) "" (List.collect (fun x -> [(stringify' "" x) ^ ", "]) arr)))  ^ "]"
    | JSON.Object obj -> "{" ^ (rem2 (List.fold (^) "" (List.collect (fun (x, y) -> [("\"" ^ x ^ "\" : ") ^ (stringify' "" y) ^ ", "]) obj)))  ^ "}"
    
  stringify' "" j


let test1 = "{
   \"cell_type\": \"markdown\",
   \"metadata\": {},
   \"source\": [
    \"# Математическая статистика\n\",
    \"## Практическое задание 4\n\"
   ]
  }"
let test2 = "{\"outputs\": [
    {
     \"data\": {
      \"text/plain\": [
       \"8.2600593032111647e-14\"
      ]
     },
     \"execution_count\": 17,
     \"metadata\": {},
     \"output_type\": \"execute_result\"
    }
   ]}"
let test3 = "{\"metadata\": [{\"order\" : 45}, {\"all\" : false}]}"


let generate = 
  let rnd = new Random()
  match rnd.Next(42) with
    | 0 -> Object []
    | _ -> Object [("random", Object [])]

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab2"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString