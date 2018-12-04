// Learn more about F# at http://fsharp.org

open System

open TestDay
open Day1
open Day2
open Day3
open Day4

let doDay x = 
    match x with
    | ("test", "silver" )-> testDaySilver
    | ("test", "gold" )-> testDayGold
    | ("1", "silver" ) -> day1Silver
    | ("1", "gold" ) -> day1Gold
    | ("2", "silver" ) -> day2Silver
    | ("2", "gold" ) -> day2Gold
    | ("3", "silver" ) -> day3Silver
    | ("3", "gold" ) -> day3Gold
    | ("4", "silver" ) -> day4Silver
    | ("4", "gold" ) -> day4Gold
    | _ -> "No such day is implemented!"

[<EntryPoint>]
let main argv =
    printfn "Which challenge would you like to execute? (day:int, silver/gold)"
    ( System.Console.ReadLine() , System.Console.ReadLine() )
        |> doDay
        |> printfn "\n%s"
    System.Console.ReadKey() |> ignore
    0 // return an integer exit code