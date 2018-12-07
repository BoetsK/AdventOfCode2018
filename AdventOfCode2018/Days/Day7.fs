module Day7

open Day7Input


let extractLetters (str:string) =
    let split = str.Split [|' '|]
    (split.[1] , split.[7])

let addEmpty key  map =
    match Map.containsKey key map with
    | true -> map
    | false -> Map.add key [] map


let addToDependencyMap dependencyMap (before, this) =
    let addBefore = addEmpty before dependencyMap
    let addThis = addEmpty this addBefore
    Map.add this (before :: (Map.find this addThis)) addThis

let rec makeOrder dependencyMap =
    match List.isEmpty dependencyMap with
    | true -> ""
    | false -> 
        let (key,_) = List.find (fun (_, dependentOn) -> List.isEmpty dependentOn) dependencyMap
        let filterOutKey = List.filter (fun (x,_) -> x <> key) dependencyMap
        let removed = List.map (fun (k, dependencyList) -> (k, List.filter (fun x -> x <> key) dependencyList)) filterOutKey
        key + (makeOrder removed)

let day7Silver = 
    input
    |> fun s -> s.Split [|'\n'|]
    |> Seq.toList
    |> List.map extractLetters
    |> List.fold addToDependencyMap Map.empty
    |> Map.toList
    |> makeOrder




type Worker = Idle | Working of string * int

let makeWorkers amount = [ for i in 1..amount -> (i,Idle) ]

let hasReadyToStart dependencyList = List.exists (fun (_,dependencies) -> List.isEmpty dependencies) dependencyList
let hasIdleWorker workers = List.exists (fun (_,worker) -> worker = Idle) workers
let hasAllIdleWorker workers = (List.filter (fun (_,worker) -> worker <> Idle) workers) |> List.isEmpty


let addTaskToTime (time:int) (task:string) =
    time + (int task.[0]) - 4

let giveTaskToWorker task time id (wId,worker) =
    match id = wId with
    | false -> (wId, worker)
    | true -> (wId, Working (task, time))

let giveWork time workers dependencyList =
    let (key,_) = List.find (fun (_, dependentOn) -> List.isEmpty dependentOn) dependencyList
    let filterOutKey = List.filter (fun (x,_) -> x <> key) dependencyList
    let (idleId,_) = List.find (fun (_,w) -> w = Idle) workers
    let newTime = addTaskToTime time key
    let newWorkers = List.map (fun x -> giveTaskToWorker key newTime idleId x) workers
    (newWorkers, filterOutKey)

    
let extractTime (_,worker) =
    match worker with
    | Idle -> 100000
    | Working (_,time) -> time

let putFastestToIdle id workers =
    List.map (fun (i,w) ->
                    match i = id with
                    | true -> (i, Idle)
                    | false -> (i,w)) 
                workers

let moveTime time workers dependencyList =
    let (fastest, worker) = List.minBy (fun x -> extractTime x) workers
    match worker with 
    | Working (task,time) -> 
                let newWorkers = putFastestToIdle fastest workers
                let removed = List.map (fun (k, dependencyList) -> (k, List.filter (fun x -> x <> task) dependencyList)) dependencyList
                (time, newWorkers, removed, task)
    | Idle -> failwith "This guy cannot be idle right now!"

let rec buildWithWorkers time workers dependencyList =
    match hasReadyToStart dependencyList with
    | true -> 
        match hasIdleWorker workers with
        | true -> 
            let (newWorkers, newList) = giveWork time workers dependencyList
            buildWithWorkers time newWorkers newList
        | false -> 
            let (newTime, newWorkers, newDependencyList, taskDone) = moveTime time workers dependencyList
            buildWithWorkers newTime newWorkers newDependencyList
    | false ->
        match hasAllIdleWorker workers with
        | true -> time
        | false ->
            let (newTime, newWorkers, newDependencyList, taskDone) = moveTime time workers dependencyList
            buildWithWorkers newTime newWorkers newDependencyList


let day7Gold = 
    let workFolk = makeWorkers 5
    input
    |> fun s -> s.Split [|'\n'|]
    |> Seq.toList
    |> List.map extractLetters
    |> List.fold addToDependencyMap Map.empty
    |> Map.toList
    |> buildWithWorkers 0 workFolk
    |> string