package `2025`.day4.models

import cats.data.RepresentableStore

type GridStore[A] = RepresentableStore[Grid, Coordinates, A]
