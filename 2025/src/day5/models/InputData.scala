package `2025`.day5.models

final case class InputData(
    identifiers: List[Long],
    intervals: Option[DisjointUnion]
)
