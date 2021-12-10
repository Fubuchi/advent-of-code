module Day3

open System.IO

let inline (<->) f x y = f y x

let binToInt x = System.Convert.ToInt32(x, 2)

let sum state curr =
    let merge ((one, zero), curr) =

        match curr with
        | '0' -> (one, zero + 1)
        | '1' -> (one + 1, zero)
        | _ -> failwith $"Bad input {curr} !"

    List.zip state curr |> List.map merge

let max (one, zero) = if one >= zero then '1' else '0'

let min (one, zero) = if one >= zero then '0' else '1'

let fisrtPart input init =
    let maxMin cell = (cell |> max, cell |> min)

    let merge (max, min) (cMax, cMin) = (max + string cMax, min + string cMin)

    input
    |> List.fold sum init
    |> List.map maxMin
    |> List.fold merge ("", "")
    ||> fun max min -> (max |> binToInt) * (min |> binToInt)

let secondPart input init =
    let analyze input choose index =
        input
        |> List.fold sum init
        |> List.item index
        |> choose

    let rec reduce choose index acc =
        match acc with
        | [ x ] -> [ x ]
        | xs ->
            let search = analyze xs choose index

            xs
            |> List.filter (fun x -> x |> List.item index = search)
            |> reduce choose (index + 1)

    let max =
        reduce max 0 input
        |> List.head
        |> List.map string
        |> String.concat ""
        |> binToInt

    let min =
        reduce min 0 input
        |> List.head
        |> List.map string
        |> String.concat ""
        |> binToInt

    max * min

let raw = File.ReadAllLines "input.txt"

let init =
    [ 0 .. raw |> Array.head |> String.length |> ((-) <-> 1) ]
    |> List.map (fun _ -> (0, 0))

let input =
    raw |> Array.map Seq.toList |> Seq.toList

fisrtPart input init |> printfn "%d"
secondPart input init |> printfn "%d"
