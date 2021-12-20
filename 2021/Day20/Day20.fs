module Day20

open System
open System.IO
open FSharpx.Collections

module PersistentHashMap =
    let tryFind k m =
        if m |> PersistentHashMap.containsKey k then PersistentHashMap.find k m |> Some
        else
            None

module HashMap = PersistentHashMap

let (<.>) f x y = f y x

let unit =
    [| (-1, 1); (0, 1); (1, 1)
       (-1, 0); (0, 0); (1, 0)
       (-1, -1); (0, -1); (1, -1) |]

let bin2dec s = Convert.ToInt32(s, 2)

let oddInfinity (code: string) = code.[0] |> string

let evenInfinity (code: string) =
    match oddInfinity code with
    | "0" -> "0"
    | "1" -> code.[511] |> string
    | oops -> failwithf "Unknown %s" oops

let enhance X Y code pixels steps =
    let oddInfinity = oddInfinity code
    let evenInfinity = evenInfinity code

    let rec loop X Y turn prev lights =
        let rec fold pixels lights points =
            match points with
            | [] -> (pixels, lights)
            | (x, y) :: ps ->
                let index =
                    unit
                    |> Array.map (fun (vx, vy) ->
                        match HashMap.tryFind (vx + x, vy + y) prev with
                        | None ->
                            if turn % 2 = 0 then
                                evenInfinity
                            else
                                oddInfinity
                        | Some (bit) -> bit)
                    |> String.concat ""

                let bit = code.[index |> bin2dec] |> string
                fold (HashMap.add (x, y) bit pixels) (bit |> int |> ((+) lights)) ps

        if turn = steps then
            lights
        else
            [ for x in -1 - turn .. X + 1 do
                  for y in turn + 1 .. -1 .. -Y - 1 -> x, y ]
            |> fold HashMap.empty 0
            ||> loop (X + 1) (Y + 1) (turn + 1)

    loop X Y 0 pixels 0

let raw = File.ReadAllLines "input.txt"

let code = raw.[0].Replace(".", "0").Replace("#", "1")

let X = raw[2..].[0] |> String.length |> ((-) <.> 1)
let Y = raw[2..] |> Array.length |> ((-) <.> 1)

let pixels =
    seq { for x in 0..X do
            for y in 0..Y -> (x, -y), if raw[2..].[y].[x] = '.' then "0" else "1" }
    |> Seq.fold (fun m (k, v) -> HashMap.add k v m) HashMap.empty

enhance X Y code pixels 2 |> printfn "First part: %d"
enhance X Y code pixels 50 |> printfn "Second part: %d"

