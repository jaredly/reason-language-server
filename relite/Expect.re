open Types;

let default = {
  string: actual => {
    toEqual: (~message=?, expected) =>
      if (actual != expected) {
        failwith(
          Printf.sprintf(
            "Expected \"%s\" to equal \"%s\" : %s",
            actual,
            expected,
            switch (message) {
            | None => ""
            | Some(m) => m
            },
          ),
        );
      },
  },
  int: actual => {
    toEqual: (~message=?, expected) =>
      if (actual != expected) {
        failwith(
          Printf.sprintf(
            "Expected %d to equal %d : %s",
            actual,
            expected,
            switch (message) {
            | None => ""
            | Some(m) => m
            },
          ),
        );
      },
  },
};
