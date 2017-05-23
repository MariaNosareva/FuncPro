// подключаем FSharp.Data
#r "/home/maria/Dropbox/prog/functional/packages/FSharp.Data/lib/net40/FSharp.Data.dll"
open FSharp.Data
open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// почтовый адрес
let email = "nosareva.mv@phystech.edu"
let explode (s : string) = [ for i in s -> i]
let (^) l r = sprintf "%s%s" l r
let dict = Map<string, string list>

let editPhoneNumber str= 
  let rec editPhoneNumbers acc = function
    | [] -> List.rev acc
    // Нормализую номера (убираю все лишние символы)
    | (x:char)::t when Char.IsDigit(x) -> editPhoneNumbers (x::acc) t
    | (x:char)::t -> editPhoneNumbers acc t
  let phoneChars = editPhoneNumbers [] (explode str)
  List.fold (fun acc (x:char) -> acc^(x.ToString())) "" phoneChars

let editList (dict:Map<string, string list>) (x:string list): Map<string, string list> =  
  let name = (List.item 1 x); 
  let innerPhones = Array.map editPhoneNumber ((List.item 3 x).Split('\n')) |> Array.toList;
  let mobilePhones = Array.map editPhoneNumber (((List.item 4 x).TrimStart('+')).Split('+')) |> Array.toList;
  let person = if (name = "") then List.item 0 x else name;
  let lstPhones = List.append innerPhones mobilePhones

  List.fold (fun acc (y:string) -> 
            if not (Map.containsKey y acc) 
            then (Map.add y [person] acc) 
            else (Map.add y (person :: (Map.find y acc)) acc) ) dict lstPhones

let getPersonsList () = 
  let phones = HtmlDocument.Load("https://mipt.ru/about/general/contacts/phones.php")
  let blocks = phones.Descendants "tr"

  // При создании blocks в него помещаются как блоки с несколькими людьми, так и каждый человек отдельно, у них отсутствует тег 'colgroup'
  let persons = Seq.fold (fun (acc:HtmlNode list) (x:HtmlNode) -> if (Seq.isEmpty (x.Descendants "colgroup")) then x::acc else acc) [] blocks
  let personsWithoutTrash = List.map (fun (x:HtmlNode) -> List.map (fun (y:HtmlNode) -> y.InnerText()) (x.Elements())) persons
  
  let acc = Map.empty<string, string list>
  List.fold editList acc personsWithoutTrash

let rec writeList = function
  | x :: xs ->  printf "%s, " x 
                writeList xs
  | [] -> printf "\n-----------------------------------------------------------------------"
          printf "\n\n"

let printPersons () = 
  let contactsMap = Map.remove "" (getPersonsList ())
  Map.iter (fun k v -> if (List.length v) > 1 then 
                                                printf "Номер телефона: %s; Люди, разделяющие его:\n" k
                                                writeList v) contactsMap

printPersons ()

let lab3 () =
  let bases = HtmlDocument.Load("http://mipt.ru/diht/bases/")
  bases.Descendants ["ul"] 
    |> Seq.filter (fun x -> x.HasClass("right-menu")) 
    |> Seq.collect (fun (x:HtmlNode) -> x.Descendants ["a"])
    // для получения ссылок вместо InnerText нужно использовать методы TryGetAttribute, Attibute или AttributeValue
    // см. исходный код https://github.com/fsharp/FSharp.Data/blob/master/src/Html/HtmlOperations.fs
    |> Seq.map(fun x -> x.InnerText()) 
    |> Seq.toList

let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("result", lab3().ToString())
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab3"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString

main ()