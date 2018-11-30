module TestDay

open TestDayInput

let inline charToInt c = int c - int '0'

let addLastToFront l = (List.last l) :: l 

let rec addCouples acc numbers =
    match numbers with
    | (a :: (b :: tail)) ->
        match a = b with
        | true -> addCouples (acc + a) (b :: tail)
        | false -> addCouples acc (b :: tail)
    | _ -> acc


let solveCaptcha input = 
    Seq.toList input
        |> addLastToFront
        |> List.map charToInt
        |> addCouples 0




let testDaySilver = solveCaptcha input |> string


let rec addPairs acc lstPair = 
    match lstPair with
    | ( h1 :: t1 , h2 :: t2) -> 
        match h1 = h2 with
        | true -> addPairs (acc + h1 + h2) (t1 , t2)
        | false -> addPairs acc (t1 , t2)
    | _ -> acc
let solveCaptchaHalf input = 
    let length = String.length input
    let halfLength = length / 2
    ( input.[0..(halfLength - 1)] |> Seq.toList |> List.map charToInt, input.[halfLength..(length-1)] |> Seq.toList |> List.map charToInt)
    |> addPairs 0


let testDayGold = solveCaptchaHalf input |> string