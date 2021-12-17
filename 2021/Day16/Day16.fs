module Day16

open System
open System.IO
open System.Text.RegularExpressions

type Packet =
    | Literal of int * int64
    | Operation of int * int * Packet list

let hex2bin =
    Map [ ('0', "0000")
          ('1', "0001")
          ('2', "0010")
          ('3', "0011")
          ('4', "0100")
          ('5', "0101")
          ('6', "0110")
          ('7', "0111")
          ('8', "1000")
          ('9', "1001")
          ('A', "1010")
          ('B', "1011")
          ('C', "1100")
          ('D', "1101")
          ('E', "1110")
          ('F', "1111") ]

let (<.>) f x y = f y x

let endString s =
    Regex.IsMatch(s, "^[0]+$")
    || s |> String.length = 0

let version (s: string) = Convert.ToInt32(s.[0..2], 2)

let packageType (s: string) = Convert.ToInt32(s.[3..5], 2)

let operation (s: string) = s.[6] |> string |> int

let literalLast s =
    let rec loop (s: string) i =
        if s.[i] = '0' then
            i + 4
        else
            loop s (i + 5)

    loop s 0

let totalVersion package =
    let rec loop package acc =
        match package with
        | Literal (v, _) -> acc + v
        | Operation (v, _, ps) -> acc + v + (ps |> List.sumBy (fun p -> loop p 0))

    loop package 0

let rec eval package =
    match package with
    | Literal (_, value) -> value
    | Operation (_, t, l) ->
        match t with
        | 0 -> l |> List.sumBy (fun p -> eval p)
        | 1 -> l |> List.fold (fun acc p -> acc * eval p) 1L
        | 2 -> l |> List.map eval |> List.min
        | 3 -> l |> List.map eval |> List.max
        | 5 ->
            match l with
            | [ s; f ] -> if eval f > eval s then 1 else 0
            | _ -> failwithf "Invalid type 5 payload %A" package
        | 6 ->
            match l with
            | [ s; f ] -> if eval f < eval s then 1 else 0
            | _ -> failwithf "Invalid type 6 payload %A" package
        | 7 ->
            match l with
            | [ s; f ] -> if eval f = eval s then 1 else 0
            | _ -> failwithf "Invalid type 7 payload %A" package
        | _ -> failwithf "Unsupported payload %A" package

let buildPackage s =
    let literal (s: string) =
        let v, idx = version s, literalLast s.[6..]

        let value =
            [| for i in 0 .. idx do
                   if i % 5 <> 0 then yield s.[i + 6] |]
            |> String

        6 + idx + 1, Literal(v, Convert.ToInt64(value, 2))

    let rec loop s packets limit =

        match endString s, limit with
        | true, _
        | _, Some (0) -> s, packets
        | _, _ ->
            let v, t, o = version s, packageType s, operation s
            let limit = limit |> Option.map ((-) <.> 1)

            match t, o with
            | 4, _ ->
                let next, literal = literal s
                loop s.[next..] (literal :: packets) limit
            | _, 0 ->
                let range = Convert.ToInt32(s.[7..21], 2)

                let _, inner = loop s.[22..22 + range - 1] [] None
                let next, other = loop s.[22 + range..] [] limit
                next, other @ Operation(v, t, inner) :: packets
            | _, _ ->
                let innerL = Convert.ToInt32(s.[7..17], 2)

                let next, inner = loop s.[18..] [] (Some innerL)
                let next, other = loop next [] limit
                next, other @ Operation(v, t, inner) :: packets

    loop s [] (Some 1)

let input =
    File.ReadAllLines "input.txt"
    |> Array.head
    |> Seq.map (fun s -> hex2bin |> Map.find s)
    |> String.concat ""
    |> (buildPackage >> snd >> List.head)

totalVersion input |> printfn "First part: %d"
eval input |> printfn "Second part: %d"
