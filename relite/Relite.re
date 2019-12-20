Printexc.record_backtrace(true);

open Types;

let expect = {
  string: actual => {
    toEqual: (~message=?, expected) =>
      if (actual != expected) {
        failwith(
          Printf.sprintf(
            "Expected %s to equal %s : %s",
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

let root = Suite.root();
let describe = Suite.describe(root);

describe.withLifecycle(
  "A",
  {
    beforeAll: () => 10,
    beforeEach: num => {string_of_int(num)},
    afterEach: (s: string) => (),
    afterAll: (x: int) => (),
  },
  ({it, describe}) => {
  it("a1", ({expect, ctx}) => {
    expect.string(ctx).toEqual("10")
  });

  describe.withLifecycle(
    "B",
    {
      beforeAll: aAll => aAll * 2,
      beforeEach: num => {string_of_int(num)},
      afterEach: (s: string) => (),
      afterAll: (x: int) => (),
    },
    ({it}) => {
    it("b1", ({expect, ctx}) => {
      expect.string(ctx).toEqual("20")
    })
    it("b2", ({expect, ctx}) => {
      expect.string(ctx).toEqual("10")
    })
  });
  ()
});


let showTrail = trail => List.rev(trail) |> String.concat(":");

let showEvent = fun
  | Runner.SuiteStart(name, trail) => ">> [" ++ name ++ "] " ++ showTrail(trail)
  | SuiteEnd(name, trail, result) => "<< " ++ name ++ "] " ++ showTrail(trail)
  | TestStart(name, trail) => "#> [" ++ name ++ "] " ++ showTrail(trail)
  | TestEnd(name, trail, testResult) => "#< [" ++ name ++ "] " ++ showTrail(trail)

// let report = x => {
//   print_endline(showEvent(x))
// };
Runner.run(~report=Reporter.report, expect, root)
// runSuite(report, [], LockedSuite(root), {
//   v: (),
//   beforeEach: () => {
//     print_endline("root before each")
//   },
//   afterEach: () => {
//     print_endline("root after each")
//   }
// });