module Day4

open Day4Input

type State = NoStart of int * ((int*int) list) | WithStart of int * ((int*int) list) * int

type record = NewGuard of int | SwitchState of int


let extractDimensions (str:string) =
    let split = str.Split [|' '; ':';'#';']'|]
    match split.Length < 7 with
    | false -> NewGuard (int split.[6])
    | true -> SwitchState (int split.[2])


let drawList state input =
    match state with
    | NoStart (guard, theList) -> 
        match input with
        | NewGuard id -> NoStart (id, theList)
        | SwitchState minute -> WithStart (guard, theList, minute)
    | WithStart (guard, theList, start) ->
        match input with
        | SwitchState stop -> NoStart (guard,  [ for i in start .. (stop - 1) -> (guard,i) ] @ theList )
        | _ -> invalidOp "Can't switch guard while still sleeping!!"


let extractListFromState state =
    match state with
    | NoStart (_,theList) -> theList
    | WithStart (_,theList, _) -> theList

let listOfSleepingMinutes (records:string) =
    records 
    |> fun s -> s.Split [|'\n'|]
    |> Seq.toList
    |> List.sortBy (fun str -> (str.Split [|']'|]).[0])
    |> List.map extractDimensions
    |> List.fold drawList (NoStart (-1, List.empty))
    |> extractListFromState 

let findLongestSleepingGuard minutes =
    minutes
    |> List.countBy (fun (guard, _) -> guard)
    |> List.maxBy (fun (_,count) -> count)
    |> fun (guard,_) -> guard

let findMostSleptMinute guard minutes =
    minutes
    |> List.filter (fun (g,_) -> g = guard)
    |> List.countBy (fun (_,minute) -> minute)
    |> List.maxBy (fun (_,count) -> count)
    |> fun (minute,_) -> minute


let day4Silver = 
    let todo = input
    let sleepingMinutes = listOfSleepingMinutes todo
    let longestSleepingGuard = findLongestSleepingGuard sleepingMinutes
    let mostSleptMinute = findMostSleptMinute longestSleepingGuard sleepingMinutes
    longestSleepingGuard * mostSleptMinute
    |> string






let mostlyFrequentlySleeping minutes =
    minutes
    |> List.countBy (fun x -> x)
    |> List.maxBy (fun (_,count) -> count)
    |> fun (most,_) -> most

let day4Gold = 
    let todo = input
    let sleepingMinutes = listOfSleepingMinutes todo
    let (guard, minute) = mostlyFrequentlySleeping sleepingMinutes
    guard * minute
    |> string
