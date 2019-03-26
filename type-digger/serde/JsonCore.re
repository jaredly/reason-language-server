

open Parsetree;
let loc = Location.none;

// TODO: use these at all
module Deserialize = {

  let listExn = [%expr
    {
      let rec loop = (items) => switch items {
        | [] => []
        | [one, ...rest] => [transformer(one), ...loop(rest)]
      };
      Belt.List.toArray(loop(items))
    }
  ];

  let listTailExn = [%expr
    {
      let rec loop = (items, collected) => switch items {
        | [] => Belt.List.reverse(collected)
        | [one, ...rest] => loop(rest, [transformer(one), ...collected])
      };
      Belt.List.toArray(Belt.List.reverse(loop(items, [])))
    }
  ];

  // let listTrackExn = [%expr
  //   {
  //     let rec loop = (i, items) => switch items {
  //       | [] => []
  //       | [one, ...rest] => [transformer(["array item " ++ string_of_int(i), ...crumbs], one), ...loop(i + 1, rest)]
  //     };
  //     Belt.List.toArray(loop(0, items))
  //   }
  // ];

  let list = [%expr
    {
      let rec loop = (i, items) => switch items {
        | [] => Belt.Result.Ok([])
        | [one, ...rest] => switch (transformer(one)) {
          | Belt.Result.Error(error) => Belt.Result.Error(["array element " ++ string_of_int(i), ...error])
          | Belt.Result.Ok(value) => switch (loop(i + 1, rest)) {
            | Belt.Result.Error(error) => Belt.Result.Error(error)
            | Ok(rest) => Belt.Result.Ok([value, ...rest])
          }
        }
      };
      switch (loop(0, [], items)) {
        | Belt.Result.Error(error) => Belt.Result.Error(error)
        | Belt.Result.Ok(value) => Belt.Result.Ok(Belt.List.toArray(value))
      }
    }
  ];

  let listTailRecursive = [%expr
    {
      let rec loop = (i, collected, items) => switch items {
        | [] => Belt.Result.Ok(Belt.List.reverse(collected))
        | [one, ...rest] => switch (transformer(one)) {
          | Belt.Result.Error(error) => Belt.Result.Error(["array element", ...error])
          | Belt.Result.Ok(value) => loop(i + 1, [value, ...collected], rest)
        }
      };
      switch (loop(0, [], items)) {
        | Belt.Result.Error(error) => Belt.Result.Error(error)
        | Belt.Result.Ok(value) => Belt.Result.Ok(Belt.List.toArray(value))
      }
    }
  ];

}