let solve key pattern =
  Seq.iterate (( + ) 1) 0
  |> Seq.find (fun num ->
         Format.sprintf "%s%d" key num
         |> Digest.string
         |> Digest.to_hex
         |> String.starts_with ~prefix:pattern)
  |> Option.get

let part_one key = solve key "00000"
let part_two key = solve key "000000"
