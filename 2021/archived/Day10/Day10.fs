module Day10

open System.IO

let corrupted = snd

let isClose symbol = List.contains symbol [ ')'; ']'; '}'; '>' ]

let score symbol =
    match symbol with
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwithf "Bang bang!! %c" symbol

let cost symbol =
    match symbol with
    | ')' -> 1UL
    | ']' -> 2UL
    | '}' -> 3UL
    | '>' -> 4UL
    | _ -> failwithf "Bang bang!! %c" symbol

let findClose symbol =
    match symbol with
    | '(' -> ')'
    | '{' -> '}'
    | '[' -> ']'
    | '<' -> '>'
    | _ -> failwithf "Bang bang!! %c" symbol

let analyze (state as (symbols, corrupted)) current  =
    if corrupted then state
    else
        match symbols with
        | [] -> if isClose current then ([current], true)
                else ([current], false)
        | s as (symbol :: symbols) ->
            match current with
            | ')' -> if symbol = '(' then (symbols, false) else (current :: s, true)
            | ']' -> if symbol = '[' then (symbols, false) else (current :: s, true)
            | '}' -> if symbol = '{' then (symbols, false) else (current :: s, true)
            | '>' -> if symbol = '<' then (symbols, false) else (current :: s, true)
            | _ -> (current :: s, false)

let repair symbols =
    let rec loop instructions symbols =
        match symbols with
        | [] -> instructions
        | symbol :: symbols ->
            loop ((symbol |> findClose) :: instructions) symbols

    loop [] symbols |> List.rev

let firstPart input =
    input
    |> Array.map (Array.fold analyze ([], false))
    |> Array.filter corrupted
    |> Array.map (fst >> List.head >> score)
    |> Array.sum

let secondPart input =
    let costs = input
                |> Array.map (Array.fold analyze ([], false))
                |> Array.filter (not << corrupted)
                |> Array.map (fst >> repair)
                |> Array.map (List.map cost >> List.fold (fun total cost -> total * 5UL + cost) 0UL)
                |> Array.sort

    costs.[costs.Length / 2]

let input = File.ReadAllLines "input.txt" |> Array.map Seq.toArray

input |> firstPart |> printfn "First part: %d"

input |> secondPart |> printfn "Second part: %d"
