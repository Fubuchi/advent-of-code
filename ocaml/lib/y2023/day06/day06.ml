open Containers
open Share.Func
module P = Share.Printer

let re_time = Pcre.regexp "Time:\\s+"
let re_dist = Pcre.regexp "Distance:\\s+"
let re_split = Pcre.regexp "\\s+"
let record_breaking t d a = a * (t - a) > d

let rec race t d a count = function
  | 1 ->
      if record_breaking t d a then
        race t d (a + 1) (count + 1) 1
      else
        count
  | 0 ->
      if record_breaking t d a then
        race t d (a + 1) (count + 1) 1
      else
        race t d (a + 1) count 0
  | hit -> unreachable P.int hit

let part_one = function
  | [ time; distance ] ->
      let time =
        time
        |> Pcre.replace ~rex:re_time ~templ:""
        |> Pcre.split ~rex:re_split
        |> List.map Int.of_string_exn
      in
      let distance =
        distance
        |> Pcre.replace ~rex:re_dist ~templ:""
        |> Pcre.split ~rex:re_split
        |> List.map Int.of_string_exn
      in
      List.combine time distance
      |> List.map (fun (t, d) -> race t d 0 0 0)
      |> List.fold_left ( * ) 1
  | e -> unreachable P.(lst string) e

let part_two = function
  | [ time; distance ] ->
      let time =
        time
        |> Pcre.replace ~rex:re_time ~templ:""
        |> Pcre.split ~rex:re_split
        |> String.concat ""
        |> Int.of_string_exn
      in
      let distance =
        distance
        |> Pcre.replace ~rex:re_dist ~templ:""
        |> Pcre.split ~rex:re_split
        |> String.concat ""
        |> Int.of_string_exn
      in
      race time distance 0 0 0
  | e -> unreachable P.(lst string) e
