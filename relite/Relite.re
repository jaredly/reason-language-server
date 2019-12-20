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

// let composeLifecycles =
//     (
//       parent: lifecycle('grandParentEnv, 'parentAll, 'parentEach),
//       child: lifecycle('parentAll, 'allEnv, 'eachEnv),
//     ): lifecycle('grandParentEnv, ('parentAll, 'allEnv), ('parentEach, 'eachEnv)) => {
//   beforeAll: env => {
//     let parentAll = parent.beforeAll(env);
//     let childAll = child.beforeAll(parentAll);
//     (parentAll, childAll)
//   },
//   beforeEach: ((parentAll, childAll)) => {
//     let parentEach = parent.beforeEach(parentAll);
//     let childEach = child.beforeEach(childAll);
//     (parentEach, childEach)
//   },
//   afterEach: ((parentEach, childEach)) => {
//     parent.afterEach(parentEach);
//     child.afterEach(childEach);
//   },
//   afterAll: ((parentAll, childAll)) => {
//     parent.afterAll(parentAll);
//     child.afterAll(childAll);
//   }
// };


open Types;

let root = Suite.root();
let describe = Suite.describe(root);

describe.withLifecycle(
  "A",
  {
    beforeAll: () => {print_endline("A before all"); 10},
    beforeEach: num => {print_endline("A before each"); string_of_int(num)},
    afterEach: (s: string) => print_endline("A after each" ++ s),
    afterAll: (x: int) => print_endline("A after all"),
  },
  ({it, describe}) => {
  it("a1", ({expect, ctx}) => {
    expect.string(ctx).toEqual("10")
  });

  describe.withLifecycle(
    "B",
    {
      beforeAll: aAll => {print_endline("B before all"); aAll * 2},
      beforeEach: num => {print_endline("B before each"); string_of_int(num)},
      afterEach: (s: string) => print_endline("B after each " ++ s),
      afterAll: (x: int) => print_endline("B after all"),
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
  | Runner.SuiteStart(trail) => ">> " ++ showTrail(trail)
  | SuiteEnd(trail, result) => "<< " ++ showTrail(trail)
  | TestStart(name, trail) => "#> [" ++ name ++ "] " ++ showTrail(trail)
  | TestEnd(name, trail, testResult) => "#< [" ++ name ++ "] " ++ showTrail(trail)

let report = x => {
  print_endline(showEvent(x))
};
Runner.run(~report, expect, root)
// runSuite(report, [], LockedSuite(root), {
//   v: (),
//   beforeEach: () => {
//     print_endline("root before each")
//   },
//   afterEach: () => {
//     print_endline("root after each")
//   }
// });