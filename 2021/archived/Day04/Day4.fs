module Day4

open System.IO

let inline (^||^) f g a = f a || g a

type SingleBoard = (int * bool * int) [,]
type Board = SingleBoard List

let slice (boards: (int * bool * int) array array) index = boards.[index..index + 4] |> array2D
let row (board: SingleBoard) i = board.[i, *]
let col (board: SingleBoard) i = board.[*, i]

let numbers =
    (File.ReadAllLines "call.txt" |> Array.head)
        .Split [| ',' |]
    |> Array.map int
    |> Array.toList

let boards =
    let raw =
        File.ReadAllLines "boards.txt"
        |> Array.map (fun s ->
            s.Split [| ' ' |]
            |> Array.filter (fun c -> not <| (c.Trim() = ""))
            |> Array.map (fun s -> (s |> int, false, 0)))
        |> Array.filter (not << Array.isEmpty)

    let boardsNum = (raw |> Array.length) / 5

    [| for i in 0 .. boardsNum - 1 -> i * 5 |]
    |> Array.map (slice raw)
    |> Array.toList

let isWin rowOrCol =
    rowOrCol
    |> Array.forall (fun (_, selected, _) -> selected)

let isBoardWin board =
    [| 0 .. 4 |]
    |> Array.map ((row board >> isWin) ^||^ (col board >> isWin))
    |> Array.exists id

let winner boards =
    match boards |> List.tryFind isBoardWin with
    | None -> (boards, None)
    | win -> (boards |> List.filter (not << isBoardWin), win)

let mark boards num time =
    boards
    |> List.map (fun board ->
        board
        |> Array2D.map (fun (n, s, t) ->
            if n = num then
                (n, true, time)
            else
                (n, s, t)))

let sum (board: SingleBoard) =
    let lastCall =
        let checkWin (board: SingleBoard) direction =
            [| 0 .. 4 |]
            |> Array.map (direction board)
            |> Array.tryFind isWin
            |> Option.map (Array.maxBy (fun (_, _, time) -> time))
            |> Option.map (fun (n, _, _) -> n)

        checkWin board row
        |> Option.orElseWith (fun () -> checkWin board col)
        |> Option.get

    (board
     |> Array2D.map (fun (n, c, _) -> if c then 0 else n)
     |> Seq.cast<int>
     |> Seq.sum)
    * lastCall

let fisrtPart numbers boards =
    let rec loop numbers time boards win =
        match win with
        | None ->
            match numbers with
            | [] -> failwith "No one win!"
            | x :: xs ->
                mark boards x time
                |> winner
                ||> loop xs (time + 1)
        | Some (win) -> win

    loop numbers 1 boards None |> sum

let secondPart numbers boards =
    let rec loop numbers time boards lastWin =
        match boards with
        | [] -> lastWin |> Option.get
        | _ ->
            match numbers with
            | [] -> lastWin |> Option.get
            | x :: xs ->
                mark boards x time
                |> winner
                ||> fun boards win -> (boards, win |> Option.orElse lastWin)
                ||> loop xs (time + 1)

    loop numbers 1 boards None |> sum

fisrtPart numbers boards
|> printfn "First part: %d"

secondPart numbers boards
|> printfn "Second part: %d"
