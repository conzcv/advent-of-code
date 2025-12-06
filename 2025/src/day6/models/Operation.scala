package `2025`.day6.models

enum Operation:
  case Plus
  case Times

object Operation:
  val fromString: PartialFunction[String, Operation] =
    case "+" => Operation.Plus
    case "*" => Operation.Times
