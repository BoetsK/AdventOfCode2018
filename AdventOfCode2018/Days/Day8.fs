module Day8

open Day8Input

type Nodes = Node of Nodes list * int list

let rec createChildNodes nbr input nodeCreateFunc =
    match nbr with
    | 0 -> ([], input)
    | _ -> 
        let (node, rest) = nodeCreateFunc input
        let (nextNodes, nextRest) = createChildNodes (nbr-1) rest nodeCreateFunc
        (node :: nextNodes, nextRest)

let rec createNode = function
    | (nbrChilds :: nbrMeta :: rest) ->
        let (childNodes, remaining) = createChildNodes nbrChilds rest createNode
        let metaData = List.take nbrMeta remaining
        let afterMeta = List.skip nbrMeta remaining
        (Node (childNodes,metaData), afterMeta)
    | _ ->     
        failwith "This can't happen!"

let createTree (input: string) =
    input 
    |> fun s -> s.Split [|' '|]
    |> Seq.toList
    |> List.map int
    |> createNode
    |> fun (node, _) -> node

let rec sumMetaData acc =
    function
    | Node (nodes, metaData) ->
        let addedToAcc = acc + (List.sum metaData)
        List.fold (fun sum node -> sumMetaData sum node) addedToAcc nodes




let day8Silver = 
    input
    |> createTree
    |> sumMetaData 0
    |> string



let addMetaValue acc meta nodes calculateFunc =
    match List.length nodes < meta with
    | true -> acc
    | false -> calculateFunc acc ( List.item (meta - 1) nodes )

let rec calculateValue acc node =
    match node with 
    | Node (nodes, meta) when nodes.Length > 0 -> List.fold (fun carry metaValue -> addMetaValue carry metaValue nodes calculateValue) acc meta
    | Node (_, meta)  -> acc + (List.sum meta)


let day8Gold = 
    input
    |> createTree
    |> calculateValue 0
    |> string