module Day25

open System.IO

let (<.>) f x y = f y x

let (<&>) f g x = f x && g x

let goEast X (x, y) = if x + 1 > X then 0, y else x + 1, y

let goSouth Y (x, y) = if y + 1 > Y then x, 0 else x, y + 1

let parse X Y (ocean: string array) =
    [ for x in 0 .. X do
          for y in 0 .. Y -> x, y ]
    |> List.fold
        (fun (east, south) (x, y) ->
            match ocean.[y].[x] with
            | '>' -> (x, y) :: east, south
            | 'v' -> east, (x, y) :: south
            | _ -> east, south)
        ([], [])

    ||> fun e s -> e |> Set.ofList, s |> Set.ofList

let firstPart X Y east south =
    let goEast = goEast X
    let goSouth = goSouth Y

    let rec loop east south turn stop =
        if stop then
            turn
        else
            let toEast =
                east
                |> Set.filter (
                    goEast
                    >> ((not << (Set.contains <.> east))
                        <&> (not << (Set.contains <.> south)))
                )

            let moved = toEast |> Set.map goEast

            let east =
                east
                |> Set.union moved
                |> (Set.difference <.> toEast)

            let toSouth =
                south
                |> Set.filter (
                    goSouth
                    >> ((not << (Set.contains <.> east))
                        <&> (not << (Set.contains <.> south)))
                )

            let moved = toSouth |> Set.map goSouth

            let south =
                south
                |> Set.union moved
                |> (Set.difference <.> toSouth)

            loop east south (turn + 1) (toEast |> Set.isEmpty && toSouth |> Set.isEmpty)

    loop east south 0 false

let raw = File.ReadAllLines "input.txt"

let X = raw.[0].Length - 1
let Y = raw.Length - 1

parse X Y raw
||> firstPart X Y
|> printfn "First part: %d"

printfn "Second part: Merry Xmas"
