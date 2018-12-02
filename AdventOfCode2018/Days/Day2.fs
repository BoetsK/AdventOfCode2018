module Day2

open Day2Input

let oneIfNotEmpty l =
    match l with
    | h :: t -> 1
    | _ -> 0

let rec removeFromList v l =
    match l with
    | h :: t when h = v  -> t
    | h :: t -> h :: (removeFromList v t)
    | _ -> l

let incrementList value (once, twice, thrice) =
    match List.contains value twice with
    | true -> (once, (removeFromList value twice), value :: thrice )
    | false -> 
        match List.contains value once with
        | true -> ((removeFromList value once), value :: twice, thrice)
        | false -> (value :: once, twice, thrice)

let rec findDublicates toCheck (once,twice,thrice) =
    match toCheck with
    | h :: t -> incrementList h (once, twice, thrice) 
                    |> findDublicates t
    | _ -> (oneIfNotEmpty twice, oneIfNotEmpty thrice)



let day2Silver = 
    input
    |> fun s -> s.Split [|'\n'|]
    |> Seq.toList
    |> List.map Seq.toList
    |> List.map (fun x -> findDublicates x ([],[],[]))
    |> List.fold (fun (a,b) (c,d) -> (a+c, b+d)) (0,0)
    |> fun (a,b) -> a * b
    |> string


let rec matchLists aList bList missed matched =
    match (aList, bList, missed) with
    | ( aH :: aT, bH :: bT, _) when aH = bH -> matchLists aT bT missed (aH :: matched)
    | ( _ :: aT, _ :: bT, false) -> matchLists aT bT true matched
    | ( _ :: aT, _ :: bT, true) -> matched
    | _ -> matched


let rec checkOneOff curr todo  =
    match todo with 
    | h :: t ->
        let len = (List.length curr) - 1
        let matching = matchLists curr h false []
        match (List.length matching) with
        | x when x = len -> matching
        | _ -> checkOneOff curr t
    | _ -> []

let rec findOneOff ids = 
    match ids with
    | h :: t -> 
        let result = checkOneOff h t
        match List.length result with
        | 0 -> findOneOff t
        | _ -> result
    | _ -> []


let charsToString chars =  new string [|for c in chars -> c|]

let day2Gold = 
    input
    |> fun s -> s.Split [|'\n'|]
    |> Seq.toList
    |> List.map Seq.toList
    |> findOneOff
    |> List.rev
    |> charsToString