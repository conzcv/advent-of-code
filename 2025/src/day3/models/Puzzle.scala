package `2025`.day3.models

enum Puzzle[+A]:
  case Init[+A](value: A) extends Puzzle[A]
  case After[+A](value: A, prev: Puzzle[A]) extends Puzzle[A]
