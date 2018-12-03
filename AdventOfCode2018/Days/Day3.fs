module Day3

open Day3Input


let extractDimensions (str:string) =
    let split = str.Split [|' '; 'x';':';',';'@'|]
    ((int split.[3]), (int split.[4]),(int split.[6]),(int split.[7]))

let rec doMoreStuff theMap theSet (a,b,d) =
    match d with
    | i when i <= 0 -> (theMap, theSet)
    | i when i > 0 ->
            match Map.containsKey (a,b) theMap with
            | false -> doMoreStuff (Map.add (a,b) 1 theMap) theSet (a,b+1,d-1)
            | true -> doMoreStuff theMap (Set.add (a,b) theSet) (a,b+1,d-1)

let rec doStuff theMap theSet (a,b,c,d) =
    match c with
    | i when i <= 0 -> (theMap, theSet)
    | i when i > 0 ->
        let (m,s) = doMoreStuff theMap theSet (a,b,d)
        doStuff m s (a+1,b,c-1,d)

let rec fillSet theMap theSet theList =
    match theList with
    | h :: t -> 
        let (a,b) = doStuff theMap theSet h
        fillSet a b t
    | _ -> theSet

let makeSet (str:string) =
    str
    |> fun s -> s.Split [|'\n'|]
    |> Seq.toList
    |> List.map extractDimensions
    |> fillSet Map.empty Set.empty

let day3Silver = 
    input
    |> makeSet
    |> Set.count
    |> string



let rec checkCleanRow badSpots (a,b,d) =
    match d with
    | i when i <= 0 -> true
    | i when i > 0 -> 
            match Set.contains (a,b) badSpots with
            | false -> checkCleanRow badSpots (a,b+1,d-1)
            | true -> false
        

let rec checkClean badSpots (a,b,c,d) =
    match c with
    | i when i <= 0 -> true
    | i when i > 0 ->
        let row = checkCleanRow badSpots (a,b,d)
        match row with
        | false -> false
        | true -> checkClean badSpots (a+1,b,c-1,d)


let rec findCleanClaim badSpots id claims =
    match claims with
    | h :: t -> 
        let tryFind = checkClean badSpots h
        match tryFind with
        | true -> id
        | false -> findCleanClaim badSpots (id+1) t
    | _ -> -1

let day3Gold = 
    let todo = input
    let madeSet = makeSet todo
    todo
    |> fun s -> s.Split [|'\n'|]
    |> Seq.toList
    |> List.map extractDimensions
    |> findCleanClaim madeSet 1 
    |> string