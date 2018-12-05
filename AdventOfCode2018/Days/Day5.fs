module Day5

open Day5Input


let checkReaction left right =
    (int left) - (int right) 
    |> abs
    |> fun x -> x = 32
    

let rec doReaction before now after =
    match after with 
    | h :: t -> 
            match checkReaction now h with
            | false -> doReaction (now :: before) h t
            | true -> match before with
                        | hb :: tb -> doReaction tb hb t
                        | _ -> match t with
                            | ht :: tt -> doReaction before ht tt
                            | _ -> before
    | _ -> now :: before 
            |> List.rev

let startReaction polymers =
    match polymers with
    | h :: t -> doReaction [] h t
    | _ -> polymers


let day5Silver = 
    input
    |> Seq.toList
    |> startReaction 
    |> List.length
    |> string


let alphabet = "abcdefghijklmnopqrstuvwxyz"

let extractThisFromInput letter =
    input
    |> Seq.toList
    |> List.filter (fun x -> x <> letter &&  not (checkReaction x letter))

let day5Gold = 
    alphabet
    |> Seq.toList
    |> List.map extractThisFromInput
    |> List.map (fun polymer -> startReaction polymer |> List.length )
    |> List.min
    |> string

    