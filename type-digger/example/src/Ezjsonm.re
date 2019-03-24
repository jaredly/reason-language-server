
type value =
  [ `Null
  | `Bool(bool)
  | `Float(float)
  | `String(string)
  | `A(list(value))
  | `O(list((string, value)))]
