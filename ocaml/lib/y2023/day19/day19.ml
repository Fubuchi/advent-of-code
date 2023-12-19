open Containers
open Share.Data
open Share.Func
open Share.Operator
module P = Share.Printer

let workflow_re = Pcre.regexp "(\\w+){(.*)}"
let expression_re = Pcre.regexp "(x|m|a|s)(<|>)(\\d+)"

type workflow_outcome =
  | Next of string
  | Accept
  | Reject

type workflow =
  | Expression of string * string * int * workflow_outcome
  | Literal of string

let to_outcome = function
  | "A" -> Accept
  | "R" -> Reject
  | label -> Next label

let reduce_workflow workflows =
  let compute_true_path (var, op, value) formular =
    let (x, y) = StringMap.find var formular in
    (match op with
    | "<" when value <= x -> None
    | ">" when value >= y -> None
    | "<" -> Some (x, min (value - 1) y)
    | ">" -> Some (max x (value + 1), y)
    | s -> unreachable P.string s)
    |> Option.map (fun (x, y) ->
           formular
           |> StringMap.update var (function
                | None -> None
                | _ -> Some (x, y)))
  in

  let compute_false_path (var, op, value) formular =
    match op with
    | "<" -> compute_true_path (var, ">", value - 1) formular
    | ">" -> compute_true_path (var, "<", value + 1) formular
    | s -> unreachable P.string s
  in

  let rec reduce_workflow' formular expressions =
    let handle_result formular = function
      | Reject -> []
      | Accept -> [ formular ]
      | Next label -> reduce_workflow' formular (StringMap.find label workflows)
    in

    match expressions with
    | [] -> P.failwith "Workflow do not return any results"
    | expression :: ws -> (
        match expression with
        | Literal label -> to_outcome label |> handle_result formular
        | Expression (var, op, value, result) -> (
            let true_path = compute_true_path (var, op, value) formular in
            let false_path = compute_false_path (var, op, value) formular in
            match (true_path, false_path) with
            | (None, None) -> []
            | (Some p, None) | (None, Some p) -> handle_result p result
            | (Some t, Some f) -> handle_result t result @ reduce_workflow' f ws
            ))
  in
  let base_formular =
    StringMap.of_list
      [ ("x", (1, 4000)); ("m", (1, 4000)); ("a", (1, 4000)); ("s", (1, 4000)) ]
  in
  reduce_workflow' base_formular (StringMap.find "in" workflows)

let parse_workflow =
  List.fold_left
    (fun workflows line ->
      match Pcre.extract ~rex:workflow_re line with
      | [| _; label; operations |] ->
          let operations =
            operations
            |> String.split ~by:","
            |> List.map (String.split ~by:":")
            |> List.map (function
                 | [ label ] -> Literal label
                 | [ expression; result ] -> (
                     match Pcre.extract ~rex:expression_re expression with
                     | [| _; var; op; value |] ->
                         Expression
                           (var, op, Int.of_string_exn value, to_outcome result)
                     | e -> unreachable P.(arr string) e)
                 | e -> unreachable P.(lst string) e)
          in
          StringMap.add label operations workflows
      | e -> unreachable P.(arr string) e)
    StringMap.empty

let parse_part =
  List.map
    (String.drop 1
    >> String.replace ~sub:"}" ~by:""
    >> String.split ~by:","
    >> List.map (String.split ~by:"=")
    >> List.map pair_of_list
    >> List.map (fun (k, v) -> (k, Int.of_string_exn v)))

let parse input =
  input
  |> List.group_succ ~eq:(fun a b -> Stdlib.(a <> "" && b <> ""))
  |> List.filter (function
       | [ "" ] -> false
       | _ -> true)
  |> function
  | [ workflows; parts ] ->
      (workflows |> parse_workflow |> reduce_workflow, parse_part parts)
  | e -> unreachable P.(lst (lst string)) e

let part_one input =
  let (workflows, parts) = parse input in
  parts
  |> List.filter (fun data ->
         workflows
         |> List.exists (fun accept ->
                data
                |> List.for_all (fun (k, v) ->
                       let (x0, y0) = StringMap.find k accept in
                       x0 <= v && v <= y0)))
  |> List.flat_map (fun data -> data |> List.map snd)
  |> List.fold_left ( + ) 0

let part_two input =
  let (workflows, _) = parse input in
  workflows
  |> List.map StringMap.bindings
  |> List.map (fun result ->
         result |> List.fold_left (fun acc (_, (x, y)) -> acc * (y - x + 1)) 1)
  |> List.fold_left ( + ) 0
