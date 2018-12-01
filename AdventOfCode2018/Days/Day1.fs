module Day1

open Day1Input

let day1Silver = 
    input
    |> fun s -> s.Split [|'\n'|]
    |> Seq.toList
    |> List.map int
    |> List.sum
    |> string


let rec rotate acc seen currentRotation fullRotation =
    let alreadySeen = List.contains acc seen
    match alreadySeen with
    | true -> acc
    | false -> 
        match currentRotation with
        | head :: tail -> rotate (acc + head) (acc :: seen) tail fullRotation
        | [] -> rotate acc seen fullRotation fullRotation

let day1Gold = 
    input
    |> fun s -> s.Split [|'\n'|]
    |> Seq.toList
    |> List.map int64
    |> fun changes -> rotate (int64 0) [] changes changes
    |> string