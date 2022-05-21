module Day8

open System
open System.IO

let (<->) (s: string) (p: string) = s.Split([| p |], StringSplitOptions.RemoveEmptyEntries)

let intersect x y =
    (x |> Seq.toArray |> Set.ofArray, y |> Seq.toArray |> Set.ofArray) ||> Set.intersect |> Seq.length

let one _ _ s = s |> String.length = 2
let four _ _ s = s |> String.length = 4
let seven _ _ s = s |> String.length = 3
let eight _ _ s = s |> String.length = 7

let two _ four s =
    s |> String.length = 5
    && (s, four) ||> intersect = 2

let three one _ s =
    s |> String.length = 5
    && (s, one) ||> intersect = 2

let five _ four s =
    s |> String.length = 5
    && (s, four) ||> intersect = 3

let nine _ four s =
    s |> String.length = 6
    && (s, four) ||> intersect = 4

let six one four s =
    s |> String.length = 6
    && (s, one) ||> intersect = 1
    && (s, four) ||> intersect = 3

let zero one four s =
    s |> String.length = 6
    && (s, one) ||> intersect = 2
    && (s, four) ||> intersect = 3

let decodeTable = [|
    zero, "0";
    one, "1";
    two, "2";
    three, "3";
    four, "4";
    five, "5";
    six, "6";
    seven, "7";
    eight, "8";
    nine, "9"
|]

let decode decodeTable (left, right) =
    let one = [left; right]
                |> Array.concat
                |> Array.find (fun s -> s |> String.length = 2)
    let four = [left; right]
                |> Array.concat
                |> Array.find (fun s -> s |> String.length = 4)

    right
    |> Array.map (fun s ->
                    decodeTable |> Array.find (fun (code, _) -> code one four s)
                    |> snd)

let firstPart decodeTable input  =
    let unique = Set.ofList [ "1"; "4" ; "7"; "8"]
    input
    |> Array.map (decode decodeTable
                >> Array.filter unique.Contains
                >> Array.length)
    |> Array.sum

let secondPart decodeTable input =
    input
    |> Array.map (decode decodeTable
                >> String.concat ""
                >> int)
    |> Array.sum

let input =
    File.ReadAllLines "input.txt"
    |> Array.map (fun s ->
                    let lr = s <-> "|"
                    (lr.[0] <-> " ", lr.[1] <-> " "))

input |> (firstPart decodeTable) |> printfn "First part: %d"
input |> (secondPart decodeTable) |> printfn "Second part: %d"
