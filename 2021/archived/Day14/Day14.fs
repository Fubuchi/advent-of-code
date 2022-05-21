module Day14

open System.IO

let insert seq instructions steps =
    let count state _ =
        state
        |> Seq.map (fun (freq as (pair, count)) ->
            let chars = pair |> Seq.toArray |> Array.map string

            match instructions |> Map.tryFind pair with
            | Some (v) ->
                [| (chars.[0] + v, count)
                   (v + chars.[1], count) |]
            | _ -> [| freq |])

        |> Seq.concat
        |> Seq.groupBy fst
        |> Seq.map (fun (pair, counts) -> (pair, counts |> Seq.sumBy snd))

    [| 1 .. steps |] |> Array.fold count seq

let solve text instructions steps =
    let last = text |> Seq.last

    let freqs =
        text
        |> Seq.windowed 2
        |> Seq.map (Seq.map string >> String.concat "")
        |> Seq.countBy id
        |> Seq.map (fun (pair, count) -> (pair, int64 count))


    let result =
        insert freqs instructions steps
        |> Seq.groupBy (fun (pair, _) -> pair |> Seq.toArray |> Array.head)
        |> Seq.map (fun (c, counts) ->
            let s = counts |> Seq.sumBy snd
            if c = last then s + 1L else s)

    let max = result |> Seq.max
    let min = result |> Seq.min
    max - min

let text = "CKKOHNSBPCPCHVNKHFFK"

let instructions =
    File.ReadAllLines "instructions.txt"
    |> Array.map (fun s ->
        let pair = s.Split(" -> ")
        (pair.[0], pair.[1]))
    |> Map.ofArray

solve text instructions 10
|> printfn "First part: %d"

solve text instructions 40
|> printfn "First part: %d"
