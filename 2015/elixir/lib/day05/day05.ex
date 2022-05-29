defmodule AdventOfCode.Day05 do
  @spec part1(list(String.t())) :: integer
  def part1(input) do
    bad_words = MapSet.new(["ab", "cd", "pq", "xy"])
    vowels = MapSet.new(["a", "e", "i", "o", "u"])

    put_vowel = fn list, ch ->
      if MapSet.member?(vowels, ch) do
        [ch | list]
      else
        list
      end
    end

    input
    |> Enum.count(fn str ->
      str
      |> String.graphemes()
      |> Enum.chunk_every(2, 1)
      |> Enum.reduce(
        {_vowels = [], _has_double = false, _has_bad_word = false},
        fn
          [a, b], {vowels, has_double, has_bad_word} ->
            {
              put_vowel.(vowels, a),
              has_double or a === b,
              has_bad_word or MapSet.member?(bad_words, a <> b)
            }

          [a], {vowels, has_double, has_bad_word} ->
            {
              put_vowel.(vowels, a),
              has_double,
              has_bad_word
            }
        end
      )
      |> then(fn {vowels, has_double, has_bad_word} ->
        not has_bad_word and length(vowels) > 2 and has_double
      end)
    end)
  end

  @spec part2(list(String.t())) :: integer
  def part2(input) do
    rule_1 = fn chs ->
      chs
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.with_index()
      |> Enum.reduce(_words = %{}, fn {[a, b], i}, words ->
        ab = a <> b

        case Map.get(words, ab) do
          {_has_double = true, _} ->
            words

          {_has_double = false, location} when location + 1 < i ->
            %{words | ab => {true, {location, i}}}

          {_has_double = false, _location} ->
            words

          nil ->
            Map.put(words, ab, {false, i})
        end
      end)
      |> Map.values()
      |> Enum.any?(fn {has_double, _} -> has_double end)
    end

    rule_2 = fn chs ->
      chs
      |> Enum.chunk_every(3, 1, :discard)
      |> Enum.any?(fn [a, _, c] -> a === c end)
    end

    input
    |> Enum.map(&String.graphemes/1)
    |> Enum.count(fn chs -> rule_1.(chs) and rule_2.(chs) end)
  end
end
