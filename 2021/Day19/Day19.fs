module Day19

open System
open System.IO

type Vector = Vector of x: int * y: int * z: int

let (<.>) f x y = f y x

let keys m = m |> Map.keys |> Set.ofSeq

let transform ((Vector (x1, y1, z1)), (Vector (x2, y2, z2)), (Vector (x3, y3, z3))) (Vector (x, y, z)) =
    Vector(x1 * x + y1 * y + z1 * z, x2 * x + y2 * y + z2 * z, x3 * x + y3 * y + z3 * z)

let (|-|) (Vector (x1, y1, z1)) (Vector (x2, y2, z2)) = Vector(x1 - x2, y1 - y2, z1 - z2)

let (|+|) (Vector (x1, y1, z1)) (Vector (x2, y2, z2)) = Vector(x1 + x2, y1 + y2, z1 + z2)

let (|||) (Vector (x, y, z)) = Vector(abs (x), abs (y), abs (z))

let distance a b =
    let (Vector (dx, dy, dz)) = a |-| b
    (dx * dx) + (dy * dy) + (dz * dz)

// See cross product
// https://people.eecs.ku.edu/~jrmiller/Courses/VectorGeometry/VectorOperations.html
let cross (Vector (ax, ay, az)) (Vector (bx, by, bz)) =
    Vector(ay * bz - az * by, az * bx - ax * bz, ax * by - ay * bx)

let unit =
    [| [| Vector(0, 0, 1); Vector(0, 0, -1) |]
       [| Vector(0, 1, 0); Vector(0, -1, 0) |]
       [| Vector(1, 0, 0); Vector(-1, 0, 0) |] |]

let permutation source =
    let limit = source |> Array.length |> ((-) <.> 1)

    [| for i in 0 .. limit do
           for j in 0 .. limit do
               if i <> j then
                   yield (source.[i], source.[j]) |]

let rotations =
    [| for x in 0 .. 2 do
           for y in 0 .. 2 do
               for i in 0 .. 1 do
                   for j in 0 .. 1 do
                       if x <> y then
                           yield unit.[x].[i], unit.[y].[j], cross unit.[x].[i] unit.[y].[j] |]

let parse (raw: string array) =
    raw
    |> Array.fold
        (fun vectors line ->
            if line |> (String.length >> (=) 0) then
                vectors
            else if line.Contains "scanner" then
                [||] :: vectors
            else
                let xyz =
                    line.Split(",", StringSplitOptions.RemoveEmptyEntries)
                    |> Array.map int

                let vec = (Vector(xyz.[0], xyz.[1], xyz.[2]))

                match vectors with
                | [] -> [ [| vec |] ]
                | vs :: rest -> (vs |> Array.append [| vec |]) :: rest)
        []

    |> List.rev

let normalize scanners =
    let distances scanner =
        permutation  scanner
        |> Array.groupBy (fun (a, b) -> distance a b)
        |> Map.ofArray

    let deltaVector fromZero fromEvaluating =
        let evaluate zero evaluating =
            let (vz1, vz2) = zero
            let (ve1, ve2) = evaluating

            if (vz1 |-| ve1) = (vz2 |-| ve2) then
                Some(vz1 |-| ve1)
            else if (vz1 |-| ve2) = (vz2 |-| ve1) then
                Some(vz1 |-| ve2)
            else
                None

        let rec loop toBeTested =
            match toBeTested with
            | [] -> None
            | (z, e) :: rest ->
                match evaluate z e with
                | (d as Some (_)) -> d
                | _ -> loop rest

        loop (
            (Seq.allPairs fromZero fromEvaluating)
            |> List.ofSeq
        )

    let checkDistance zero scanner =
        let rec loop intersected baseDis dis deltas =
            match intersected with
            | [] -> deltas
            | xs when
                deltas
                |> List.length
                |> ((+) xs.Length)
                |> ((<) <.> 12)
                ->
                []
            | x :: xs ->
                match deltaVector (baseDis |> Map.find x) (dis |> Map.find x) with
                | None -> loop xs baseDis dis deltas
                | Some (d) -> loop xs baseDis dis (d :: deltas)

        let baseDis = distances zero

        rotations
        |> Array.fold
            (fun state matrix ->
                if state |> Option.isSome then
                    state
                else
                    let rotated = scanner |> Array.map (transform matrix)
                    let dis = distances rotated

                    let intersected =
                        Set.intersect (baseDis |> keys) (dis |> keys)
                        |> Set.toList

                    if intersected |> List.length |> ((<) <.> 12) then
                        None
                    else
                        match loop intersected baseDis dis []
                              |> List.countBy id
                              |> List.tryFind (fun (_, c) -> c >= 12)
                              |> Option.map fst
                            with
                        | None -> state
                        | Some (d) -> Some(d, rotated |> Array.map ((|+|) d)))
            None

    let rec loop normalized zeroes remaining =
        match remaining with
        | [||] -> normalized
        | remaining ->
            let remainingi = remaining |> Array.indexed

            let (_, faileds, rotateds) =
                Seq.allPairs zeroes remainingi
                |> Seq.map (fun ((_, z), (ir, r)) ->
                    match checkDistance z r with
                    | None -> (ir, None)
                    | Some (d, rotated) -> (ir, Some(d, rotated)))
                |> Seq.fold
                    (fun (state as (finished, failed, rotateds)) next ->
                        match next with
                        | ir, None ->
                            if finished |> Set.contains ir then
                                state
                            else
                                (finished, Set.add ir failed, rotateds)
                        | ir, Some (rotated) -> (Set.add ir finished, Set.remove ir failed, rotated :: rotateds))
                    (Set.empty, Set.empty, [])

            let remaining =
                faileds
                |> Set.map (fun i -> remaining.[i])
                |> Set.toArray

            loop (rotateds @ normalized) rotateds remaining

    let normalized, remaining =
        scanners |> List.head, scanners |> List.tail |> List.toArray

    loop [ (Vector(0, 0, 0), normalized) ] [ (Vector(0, 0, 0), normalized) ] remaining


let firstPart input =
    input
    |> List.map (snd >> Set.ofArray)
    |> Set.unionMany
    |> Set.count

let secondPart input =
    permutation  (input |> List.map fst |> List.toArray)
    |> Array.map (fun (v1, v2) ->
        let (Vector (dx, dy, dz)) = v1 |-| v2 |> (|||)
        dx + dy + dz)
    |> Array.max

let input =
    File.ReadAllLines "input.txt"
    |> (parse >> normalize)

firstPart input |> printfn "First part: %d"
secondPart input |> printfn "Second part: %d"
