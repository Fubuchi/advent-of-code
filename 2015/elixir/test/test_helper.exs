defmodule TestHelper do
  import ExUnit.Assertions

  def one_line(%{file: file}, expected, fun) do
    file
    |> File.read!()
    |> String.split("\n")
    |> Enum.at(0)
    |> one_line(expected, fun)
  end

  def one_line(text, expected, fun) do
    text
    |> fun.()
    |> (&ExUnit.Assertions.assert(&1 === expected)).()
  end

  def one_line_and_split(%{file: file}, expected, fun) do
    file
    |> File.read!()
    |> String.split("\n")
    |> Enum.at(0)
    |> one_line_and_split(expected, fun)
  end

  def one_line_and_split(text, expected, fun) do
    text
    |> String.graphemes()
    |> fun.()
    |> (&ExUnit.Assertions.assert(&1 === expected)).()
  end

  def multiple_lines(%{file: file}, expected, fun) do
    file
    |> File.read!()
    |> String.split("\n", trim: true)
    |> multiple_lines(expected, fun)
  end

  def multiple_lines(lines, expected, fun) do
    lines
    |> fun.()
    |> (&ExUnit.Assertions.assert(&1 === expected)).()
  end
end

ExUnit.start(exclude: [:skip])
