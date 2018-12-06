module Day6

open Day6Input

let extractCoo (str:string) =
    let coo = str.Split [|','|]
    (int coo.[0], int coo.[1])

let squarePoints (x,y) offset =
  seq { for i in (0 - offset)..(x + offset) do
            for j in (0 - offset)..(y + offset) do
               yield (i,j) }
               |> Seq.toList

let manhattan (a,b) (c,d) =
    (abs (a - c)) + (abs (b - d))

let addOrIncrement sizeMap coo =
    match Map.containsKey coo sizeMap with
    | true -> Map.add coo ((Map.find coo sizeMap) + 1) sizeMap
    | false -> Map.add coo 1 sizeMap

let addToMap sizeMap listOfDist =
    match listOfDist with
    | (_,h1) :: (_,h2) :: _ when h1 = h2 -> sizeMap
    | (h,_) :: _ -> addOrIncrement sizeMap h
    | _ -> failwith "can't have empty list!"
    
let incrementClosestDistance sizeMap coo coors =
    coors
    |> List.map (fun x -> (x,manhattan x coo))
    |> List.sortBy (fun (_,x) -> x)
    |> addToMap sizeMap

let incrementClosestDistance2 sizeMap coo coors =
    coors
    |> List.map (fun (x,_) -> (x,manhattan x coo))
    |> List.sortBy (fun (_,x) -> x)
    |> addToMap sizeMap


let fillSquare dim coors =
    let func = (fun m co -> incrementClosestDistance m co coors)
    squarePoints dim 0
    |> List.fold func Map.empty

let rec keepExpanding mem sizeMap (a,b) (c,d) =
    let func = (fun m co -> incrementClosestDistance2 m co mem)
    let map1 = List.fold func sizeMap ([| for i in a .. c -> (i,b) |] |> Array.toList) 
    let map2 = List.fold func map1 ([| for i in a .. c -> (i,d) |] |> Array.toList) 
    let map3 = List.fold func map2 ([| for i in (b+1) .. (d-1) -> (a,i) |] |> Array.toList) 
    let map4 = List.fold func map3 ([| for i in (b+1) .. (d-1) -> (c,i) |] |> Array.toList) 
    match (List.exists (fun (k,v) -> (Map.find k map4) < v) mem) with
    | true -> 
        let newMemory = List.map (fun (k,_) -> (k, Map.find k map4)) mem
        keepExpanding newMemory map4 (a-1,b-1) (c+1,d+1)
    | false -> 
        List.filter (fun (k,v) -> (Map.find k map4) = v) mem

let expand sizeMap coors (x,y) =
    let memory = List.map (fun x -> (x,Map.find x sizeMap)) coors
    keepExpanding memory sizeMap (-1,-1) (x+1,y+1)


let day6Silver = 
    let formatted =
        input
        |> fun s -> s.Split [|'\n'|]
        |> Seq.toList
        |> List.map extractCoo
    let dimensions =
        formatted
        |> List.fold (fun (a,b) (x,y) -> (max a x, max b y)) (0,0) 
    let squared = fillSquare dimensions formatted
    let expanded = expand squared formatted dimensions
    expanded
    |> List.maxBy (fun (_,x) -> x)
    |> fun (_,x) -> string x






let withinXDistanceFrom coors maxSize co =
    let dist = List.fold (fun acc x -> acc + (manhattan x co)) 0 coors
    dist < maxSize

let day6Gold = 
    let formatted =
        input
        |> fun s -> s.Split [|'\n'|]
        |> Seq.toList
        |> List.map extractCoo
    let dimensions =
        formatted
        |> List.fold (fun (a,b) (x,y) -> (max a x, max b y)) (0,0) 
    squarePoints dimensions 100
    |> List.filter (fun x -> withinXDistanceFrom formatted 10000 x)
    |> List.length
    |> string