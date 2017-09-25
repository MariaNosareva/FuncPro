open System
open System.IO
open System.Net
open System.Text

// 1
let lines (path:string) = seq { 
    use sr = new StreamReader(path)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

// 3
let wordsWithoutTrash (path:string) = 
    lines path
    |> Seq.map (fun str -> str.Split([|' '; ','; ';'; '\"'; '.'; ':'; '\''; '!'; '&'|]))
    // Здесь можно еще Seq.concat
    |> Seq.fold (fun acc x -> Seq.append acc x) Seq.empty 
    |> Seq.filter (fun x -> x.Length > 0)



// 4
let pairs s =
    let rec pairs acc = function
        | [||] -> acc
        | [|x|] -> acc
        | xs -> pairs (List.append acc [(xs.[0], xs.[1])]) (Array.tail xs) 
    pairs [] (Array.ofSeq s)

// Частотность пар 
let freqPairs = wordsWithoutTrash "test_file.txt" 
                |> pairs 
                |> List.groupBy (fun (x, y) -> (x, y)) 
                |> List.map (fun ((x, y), z) -> ((x, y), List.length z))
                |> List.sortBy (fun ((x, y), z) -> -z)

// Частотность слов

let freqWords = wordsWithoutTrash "test_file.txt"
                |> Seq.groupBy (fun x -> x.ToLower())
                |> Seq.map (fun (x, y) -> (x, Seq.length y))
                |> Seq.sortBy (fun (x, y) -> -y)
                |> Seq.take 5

let rec text (word:string) (i:int) =
    let rand = System.Random(45)
    match i with 
    | 100 -> printf ".\n"
    | i when i%rand.Next() = 0 -> let r = List.ofSeq freqPairs |> List.filter (fun ((x, y), z) -> (x = word))
                                  let rnd = System.Random(7)
                                  let next = snd( fst(List.item (rnd.Next()%(r.Length)) r ))
                                  text next (i+1)
    | _ -> printf "%s " word
           let next = snd (fst (List.ofSeq freqPairs |> List.find (fun ((x, y), z) -> (x = word))))
           text next (i+1)

text "Мария" 1
