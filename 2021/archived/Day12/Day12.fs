module Day12

open System
open System.IO

type Route =
    { current: string
      visited: string Set
      dupSmall: string option }

let (^^) f g x = f x || g x

let (<->) (s: string) (pattern: string) =
    s.Split([| pattern |], StringSplitOptions.RemoveEmptyEntries)

let extract current (s, e) = if s = current then e else s

let addEdge a b map =
    match Map.tryFind a map with
    | None -> Map.add a (Set.singleton b) map
    | Some (s) -> Map.add a (Set.add b s) map

let isSmall node =
    node |> Seq.toArray |> Array.forall Char.IsLower

let endWith node bracnh = bracnh |> snd |> (=) node

let startWith node bracnh = bracnh |> fst |> (=) node

let rec goBack map doubleVisit route =
    let { current = current
          visited = visited
          dupSmall = dupSmall } =
        route

    match current, Set.contains current visited with
    | "start", _ -> 1
    | "end", _ -> 0
    | node, true when
        isSmall node
        && (not doubleVisit || dupSmall.IsSome)
        ->
        0
    | node, seen ->
        let visited = Set.add current visited

        let dupSmall =
            match dupSmall with
            | None ->
                if seen && doubleVisit && isSmall node then
                    Some(current)
                else
                    None
            | s as Some (_) -> s

        Map.find current map
        |> Seq.sumBy (fun next ->
            goBack
                map
                doubleVisit
                { current = next
                  visited = visited
                  dupSmall = dupSmall })

let solve doubleVisist input =
    let map =
        input
        |> List.fold (fun m (l, r) -> m |> addEdge l r |> addEdge r l) Map.empty

    input
    |> List.filter (startWith "end" ^^ endWith "end")
    |> List.map (fun branch ->
        { current = extract "end" branch
          visited = Set.singleton "end"
          dupSmall = None })
    |> List.map (goBack map doubleVisist)
    |> List.sum

let input =
    File.ReadAllLines "input.txt"
    |> Array.map (fun s ->
        let pair = s <-> "-"
        (pair.[0], pair.[1]))
    |> Array.toList

solve false input |> printfn "First part: %d"
solve true input |> printfn "Seond part: %d"
