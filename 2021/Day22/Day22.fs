module Day22

open System.Text.RegularExpressions
open System.IO

// length of 0 -> 1 is 2
let (<->) a b = a - b |> abs |> (+) 1

type Point = { x: int; y: int; z: int }

type Operation = { on: bool; min: Point; max: Point }

// https://developer.mozilla.org/en-US/docs/Games/Techniques/3D_collision_detection
let aabb a b =
    a.min.x <= b.max.x
    && a.max.x >= b.min.x
    && a.min.y <= b.max.y
    && a.max.y >= b.min.y
    && a.min.z <= b.max.z
    && a.max.z >= b.min.z

let intersect a b on =
    if aabb a b then
        { on = on
          min =
            { x = max a.min.x b.min.x
              y = max a.min.y b.min.y
              z = max a.min.z b.min.z }
          max =
            { x = min a.max.x b.max.x
              y = min a.max.y b.max.y
              z = min a.max.z b.max.z }

        }
        |> Some
    else
        None

let volume a =
    (a.min.x <-> a.max.x |> int64)
    * (a.min.y <-> a.max.y |> int64)
    * (a.min.z <-> a.max.z |> int64)
    * (if a.on then 1L else -1L)

let reboot operations =
    operations
    |> Seq.fold
        (fun currentOps o ->
            let newOps =
                currentOps
                |> List.choose (fun op -> intersect op o (not op.on))
                |> fun newOps -> if o.on then o :: newOps else newOps

            List.append currentOps newOps)
        []

let parse s =
    let pattern = "[0-9\-]+"

    let num =
        Regex.Matches(s, pattern)
        |> Seq.cast<Match>
        |> Seq.map (fun m -> m.Value |> int)
        |> Seq.toArray

    { on = s.Contains("on")
      min =
        { x = num.[0]
          y = num.[2]
          z = num.[4] }
      max =
        { x = num.[1]
          y = num.[3]
          z = num.[5] } }

let firstPart operations =
    operations
    |> Seq.choose (fun o ->
        intersect
            o
            { on = true // doesn't matter
              min = { x = -50; y = -50; z = -50 }
              max = { x = 50; y = 50; z = 50 } }
            o.on)
    |> reboot
    |> Seq.sumBy volume

let seondPart operations =
    operations |> reboot |> Seq.sumBy volume

let operations =
    File.ReadAllLines "input.txt" |> Array.map parse

firstPart operations |> printfn "First part: %d"
seondPart operations |> printfn "Second part: %d"
