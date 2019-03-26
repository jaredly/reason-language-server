[@ocaml.warning "-34"];
module Types1 = {
  type _AllTypes__All__normalRecord =
    AllTypes.All.normalRecord = {
      a: int,
      b: string,
      c: (int, (string, float)),
      d: array(int),
      e: list(float),
      f: option(int),
    }
  and _AllTypes__All__normalVariant =
    AllTypes.All.normalVariant =
      | A | B | C(int) | D(_AllTypes__All__normalRecord)
  and _AllTypes__All__parameterizedRecord('a, 'b) =
    AllTypes.All.parameterizedRecord('a, 'b) = {
      a: 'a,
      b: 'b,
      c: (int, float),
      d: _AllTypes__All__recursive,
    }
  and _AllTypes__All__parameterizedVariant('a, 'b) =
    AllTypes.All.parameterizedVariant('a, 'b) =
      | PA
      | PB('a)
      | PC('a, 'b)
      | PD(_AllTypes__All__parameterizedRecord('a, 'b))
      | PE(_AllTypes__All__normalVariant)
      | PF(_AllTypes__All__normalRecord)
  and _AllTypes__All__recursive =
    AllTypes.All.recursive = | A | B(_AllTypes__All__recursive)
  and _AllTypes__All__rename = _AllTypes__All__top
  and _AllTypes__All__top =
    AllTypes.All.top = {
      contents: _AllTypes__All__parameterizedVariant(int, array(float)),
      title: string,
    };
};
let currentVersion = 1;
