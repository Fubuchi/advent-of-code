import gleam/io
import gleam/regex.{Match}
import gleam/list
import gleam/string
import gleam/result
import gleam/option.{type Option, None, Some, unwrap}
import gleam/int.{max, parse}

type Game {
  Game(id: Int, red: Option(Int), blue: Option(Int), green: Option(Int))
}

fn update_pull(pull, val) {
  let assert Ok(val) = parse(val)
  case pull {
    None -> Some(val)
    Some(old_val) -> Some(max(val, old_val))
  }
}

fn parse_game(input, id_re, split_re) {
  input
  |> list.index_map (fn (id, game) {
      let assert [Match(head, _)] = regex.scan(id_re, game)
      string.replace(game, head, "")
      |> regex.split(split_re, _)
      |> list.map (fn (pulls) { string.split(pulls, " ") })
      |> list.fold (Game(id + 1, None, None, None), fn (game, pull) {
        case pull {
          [n, "red"] -> Game(..game, red: update_pull(game.red, n))
          [n, "blue"] -> Game(..game, blue: update_pull(game.blue, n))
          [n, "green"] -> Game(..game, green: update_pull(game.green, n))
          _ -> game
        }
      })
  })
}

fn validate(game, value, less_than) {
  case value(game) {
    Some(v) -> case v <= less_than {
      True -> Ok(game)
      _ -> Error(#(game, less_than))
    }
    _ -> Ok(game)
  }
}

fn part_one(input) {
  let assert Ok(id_re) = regex.from_string("Game \\d+: ")
  let assert Ok(split_re) = regex.from_string("; |, ")
  input
  |> parse_game(id_re, split_re)
  |> list.filter_map(fn (game) {
    Ok(game)
    |> result.then(fn (game) { validate(game, fn (g) { g.red }, 12) })
    |> result.then(fn (game) { validate(game, fn (g) { g.green }, 13) })
    |> result.then(fn (game) { validate(game, fn (g) { g.blue}, 14) })
    |> result.map (fn (game) { game.id })
  })
  |> list.fold(0, fn (acc, id) { acc + id })
}

fn part_two(input) {
  let assert Ok(id_re) = regex.from_string("Game \\d+: ")
  let assert Ok(split_re) = regex.from_string("; |, ")
  input
  |> parse_game(id_re, split_re)
  |> list.fold(0, fn (acc, game) {
    acc + unwrap(game.red, 1) * unwrap(game.blue, 1) * unwrap(game.green, 1)
  })
}
