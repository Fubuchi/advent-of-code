module Day17

open System
open System.IO
open System.Text.RegularExpressions

let (<.>) f x y = f y x

let boundX x1 x2 =
    let l =
        [ x1 |> int |> abs; x2 |> int |> abs ]
        |> List.sort

    (l.Head, l.Tail.Head)

let boundY y1 y2 =
    let l = [ y1 |> int; y2 |> int ] |> List.sort
    (l.Head, l.Tail.Head)

let inZone minX maxX minY maxY x y =
    minX <= x && x <= maxX && minY <= y && y <= maxY

let shoot maxX minY vx vy =

    let rec loop vx vy x y h =
        if x > maxX || y < minY then
            x - vx, y - vy, h
        else
            let vx = max (vx - 1) 0
            let vy = vy - 1
            loop vx vy (x + vx) (y + vy) (max h (y + vy))

    loop vx vy vx vy Int32.MinValue

let multiShoot minX maxX minY maxY =
    let inZone = inZone minX maxX minY maxY

    [ for vx in maxX .. -1 .. 0 do
          for vy in minY .. (minY |> abs |> ((-) <.> 1)) -> vx, vy ]
    |> List.map (fun (vx, vy) -> shoot maxX minY vx vy)
    |> List.filter (fun (x, y, _) -> inZone x y)

let firstPart landing =
    landing
    |> Seq.maxBy (fun (_, _, h) -> h)
    |||> (fun _ _ h -> h)

let secondPart landing = landing |> Seq.length

let raw = File.ReadAllLines "input.txt" |> Array.head

let (minX, maxX), (minY, maxY) =
    match Regex.Matches(raw, "([0-9\-]+)")
          |> Seq.cast<Match>
          |> Seq.toList
        with
    | [ x1; x2; y1; y2 ] -> (boundX x1.Value x2.Value), (boundY y1.Value y2.Value)
    | _ -> failwithf "Invalid input %s" raw

let landing = multiShoot minX maxX minY maxY

firstPart landing |> printfn "First part: %d"
secondPart landing |> printfn "Second part: %d"
