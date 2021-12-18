module Day18

open System
open System.IO
open System.Text.RegularExpressions

let pairReg = "\[\d+,\d+\]"
let valueReg = "\d+"
let splitReg = "\d{2,}"

type Action =
    | Explode of int * int
    | Split of int * int

let (<.>) f x y = f y x

let extracPair pair =
    let values =
        Regex.Matches(pair, valueReg)
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Value |> int)
        |> Seq.toList

    (values.Head, values.Tail.Head)

let checExplode s left right =
    let take c = c = '[' || c = ']'

    let count chs ch =
        chs |> Seq.filter (fun c -> c = ch) |> Seq.length

    let rec goLeft (s: string) right openSquares =
        match right, count openSquares '[' with
        | -1, _
        | _, 4 -> openSquares
        | r, _ ->
            match openSquares, take s.[r] with
            | _, false -> goLeft s (r - 1) openSquares
            | [], _ -> goLeft s (r - 1) [ s.[r] ]
            | last :: rest, _ ->
                match s.[r] with
                | '[' ->
                    if last = ']' then
                        goLeft s (r - 1) rest
                    else
                        goLeft s (r - 1) ('[' :: last :: rest)
                | _ -> goLeft s (r - 1) (']' :: last :: rest)

    let rec goRight s left closeSquares =
        match left, count closeSquares ']' with
        | _, 4 -> closeSquares
        | l, _ when l = (s |> String.length) -> closeSquares
        | l, _ ->
            match closeSquares, take s.[l] with
            | _, false -> goRight s (l + 1) closeSquares
            | [||], _ -> goRight s (l + 1) [| s.[l] |]
            | cq, _ ->
                let last = cq |> Array.last

                match s.[l] with
                | ']' ->
                    if last = '[' then
                        goRight s (l + 1) cq.[0..cq.Length - 2]
                    else
                        goRight s (l + 1) (Array.append cq [| ']' |])
                | _ -> goRight s (l + 1) (Array.append cq [| '[' |])

    let left = goLeft s left []
    let right = goRight s right [||]
    (count left '[') = 4 && (count right ']') = 4

let getExplode s =
    let e =
        Regex.Matches(s, pairReg)
        |> Seq.cast<Match>
        |> Seq.filter (fun m -> checExplode s (m.Index - 1) (m.Index + m.Length))
        |> Seq.toList

    match e with
    | [] -> None
    | e :: _ -> Explode(e.Index - 1, e.Index + e.Length) |> Some

let explode (s: string) left right =
    let take = Char.IsDigit

    let firstNumber s start limit direction =
        let rec loop (s: string) idx digits =
            match idx with
            | idx when idx = limit -> digits
            | idx ->
                match digits, take s.[idx] with
                | [], false -> loop s (idx + direction) []
                | _, false -> digits
                | xs, true -> loop s (idx + direction) ((s.[idx], idx) :: xs)

        match loop s start [] |> List.sortBy snd with
        | [] -> None
        | s ->
            let left = s |> List.head |> snd
            let right = s |> List.last |> snd

            (left,
             right,
             s
             |> List.map (fst >> string)
             |> String.concat ""
             |> int)
            |> Some

    let doExplodeLeft (s: string) pl l r v =
        let seg1 = s.[..l - 1]
        let seg2 = s.[r + 1..left]
        let seg3 = s.[right..]

        seg1 + ((pl + v) |> string) + seg2 + "0" + seg3

    let doExplodeRight (s: string) pr l r v =
        let seg1 = s.[..left]
        let seg2 = s.[right..l - 1]
        let seg3 = s.[r + 1..]

        seg1 + "0" + seg2 + ((pr + v) |> string) + seg3

    let doExplodeBoth (s: string) pl pr (ll, lr, lv) (rl, rr, rv) =
        let seg1 = s.[..ll - 1]
        let seg2 = s.[lr + 1..left]
        let seg3 = s.[right..rl - 1]
        let seg4 = s.[rr + 1..]

        seg1
        + ((pl + lv) |> string)
        + seg2
        + "0"
        + seg3
        + ((pr + rv) |> string)
        + seg4

    let (pl, pr) = extracPair s.[left + 1..right - 1]

    let leftExplode = firstNumber s left 0 -1

    let rightExplode = firstNumber s right (s |> String.length) 1

    match leftExplode, rightExplode with
    | Some (l, r, v), None -> doExplodeLeft s pl l r v
    | None, Some (l, r, v) -> doExplodeRight s pr l r v
    | Some (ll, lr, lv), Some (rl, rr, rv) -> doExplodeBoth s pl pr (ll, lr, lv) (rl, rr, rv)
    | _ -> failwithf "Invalid explode None, None"

let getSplit s =
    let splits =
        Regex.Matches(s, splitReg)
        |> Seq.cast<Match>
        |> Seq.map (fun m -> (m.Index, m.Index + m.Length - 1))
        |> Seq.toList

    match splits with
    | [] -> None
    | s :: _ -> Split(s |> fst, s |> snd) |> Some

let split (s: string) left right =
    let num = s.[left..right] |> int
    let left = s.[..left - 1]
    let right = s.[right + 1..]

    left + $"[{num / 2},{(num + 1) / 2}]" + right

let reduce math =
    let rec loop math action =
        let math =
            match action with
            | Explode (left, right) -> explode math left right
            | Split (left, right) -> split math left right

        match getExplode math, getSplit math with
        | Some (e), _ -> loop math e
        | None, Some (s) -> loop math s
        | _ -> math

    match getExplode math, getSplit math with
    | Some (e), _ -> loop math e
    | None, Some (s) -> loop math s
    | _ -> math

let sum (number: string array) =
    number
    |> Array.reduce (fun s1 s2 -> reduce $"[{s1},{s2}]")

let total s =
    let rec loop (s: string) =
        let pairs =
            Regex.Matches(s, pairReg)
            |> Seq.cast<Match>
            |> Seq.toList

        match pairs with
        | [] ->
            Regex.Matches(s, valueReg)
            |> Seq.cast<Match>
            |> Seq.head
            |> (fun m -> m.Value |> int)
        | p :: _ ->
            let (pl, pr) =
                extracPair s.[p.Index..p.Index + p.Length - 1]

            let seg1 = s.[..p.Index - 1]
            let seg2 = s.[p.Index + p.Length..]

            loop (seg1 + $"{3 * pl + 2 * pr}" + seg2)

    loop s

let firstPart = sum >> total

let secondPart input =
    Seq.allPairs [ 0 .. input |> Array.length |> ((-) <.> 1) ] [ 0 .. input |> Array.length |> ((-) <.> 1) ]
    |> Seq.filter (fun (x, y) -> x <> y)
    |> Seq.map (fun (x, y) -> firstPart [| input.[x]; input.[y] |])
    |> Seq.max

let input = File.ReadAllLines "input.txt"

firstPart input |> printfn "First part: %d"
secondPart input |> printfn "Second part: %d"
