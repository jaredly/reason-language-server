Printexc.record_backtrace(true);

exception Expect(string);

type stringExpect = {toEqual: (~message: string=?, string) => unit};
type intExpect = {toEqual: (~message: string=?, int) => unit};
type expect = {
  string: string => stringExpect,
  int: int => intExpect,
};

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

type lifecycle('parentEnv, 'allEnv, 'eachEnv) = {
  beforeAll: 'parentEnv => 'allEnv,
  beforeEach: 'allEnv => 'eachEnv,
  afterEach: 'eachEnv => unit,
  afterAll: 'allEnv => unit,
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

let plainLc = {beforeAll: x => x, beforeEach: x => x, afterEach: _ => (), afterAll: _ => ()};

let rootLc = {beforeAll: () => (), beforeEach: () => (), afterEach: () => (), afterAll: () => ()};

type describeArgs('env) = {it: (string, testArgs('env) => unit) => unit}
and describe('parentEnv) = {
  plain: (string, describeArgs('parentEnv) => unit) => unit,
  withLifecycle:
    'allEnv 'eachEnv.
    (string, lifecycle('parentEnv, 'allEnv, 'eachEnv), describeArgs('eachEnv) => unit) => unit,

}
and describeWithOptions('parentEnv) = {
  plain: (string, describeArgs('parentEnv) => unit) => unit,
  withLifecycle:
    'allEnv 'eachEnv.
    (string, lifecycle('parentEnv, 'allEnv, 'eachEnv), describeArgs('eachEnv) => unit) => unit,

  skip: describe('parentEnv),
  // only: describe('parentEnv),
}
and testArgs('env) = {
  expect,
  ctx: 'env,
};

// type test('eachEnv) = (string, (testArgs('eachEnv)) => unit);
type child('allEnv, 'eachEnv) =
  | Test(string, testArgs('eachEnv) => unit)
  | Suite(lockedSuite('allEnv))
and lockedSuite('parentEnv) = LockedSuite(suite('parentEnv, 'allEnv, 'eachEnv)) : lockedSuite('parentEnv)
and suite('parentEnv, 'allEnv, 'eachEnv) = {
  name: string,
  skipped: bool,
  lc: lifecycle('parentEnv, 'allEnv, 'eachEnv),
  mutable children: list(child('allEnv, 'eachEnv)),
};
// let suites = ref([]);

let suite = suite => Suite(LockedSuite(suite));

let makeDescribe = parent => {
  let describeWithOptions = {
    plain: (name, body) => {
      let children = ref([]);
      body({it: (name, body) => children.contents = [Test(name, body), ...children.contents]});
      parent.children = [suite({name, lc: plainLc, skipped: false, children: children.contents}), ...parent.children];
    },
    withLifecycle: (name, lc, body) => {
      let children = ref([]);
      body({it: (name, body) => children.contents = [Test(name, body), ...children.contents]});
      parent.children = [suite({name, lc, skipped: false, children: children.contents}), ...parent.children];
    },
    skip: {
      plain: (name, body) => {
        let children = ref([]);
        body({it: (name, body) => children.contents = [Test(name, body), ...children.contents]});
        parent.children = [suite({name, lc: plainLc, skipped: true, children: children.contents}), ...parent.children];
      },
      withLifecycle: (name, lc, body) => {
        let children = ref([]);
        body({it: (name, body) => children.contents = [Test(name, body), ...children.contents]});
        parent.children = [suite({name, lc, skipped: true, children: children.contents}), ...parent.children];
      },
    },
  };
  describeWithOptions
};

let rootSuite = {
  name: "Root",
  lc: rootLc,
  skipped: false,
  children: [],
};
let describe = makeDescribe(rootSuite);

// let describe = {
//   plain: (name, body) => {
//     let children = ref([]);
//     body({it: (name, body) => children.contents = [Test(name, body), ...children.contents]});
//     suites.contents = [Suite({name, lc: plainLc, skipped: false, children: children.contents}), ...suites.contents];
//   },
//   withLifecycle: (name, lc, body) => {
//     let children = ref([]);
//     body({it: (name, body) => children.contents = [Test(name, body), ...children.contents]});
//     suites.contents = [Suite({name, lc: lc, skipped: false, children: children.contents}), ...suites.contents];
//   },
// };

let beforeAll = beforeAll => {
  beforeAll,
  beforeEach: x => x,
  afterEach: _ => (),
  afterAll: () => (),
};

let beforeEach = beforeEach => {
  beforeAll: () => (),
  beforeEach,
  afterEach: _ => (),
  afterAll: () => (),
};

let beforeAfterEach = (before, after) => {
  beforeAll: () => (),
  beforeEach: before,
  afterEach: after,
  afterAll: () => (),
};

describe.withLifecycle(
  "A",
  {
    beforeAll: () => 10,
    beforeEach: string_of_int,
    afterEach: (s: string) => (),
    afterAll: (x: int) => (),
  },
  ({it}) => {
  it("x", ({expect, ctx}) => {
    expect.string(ctx).toEqual("10")
  })
});

/*

A
- a1
- B
  - b1
  - b2

describe.withLifecycle("A", lc, ({describe, test}) => {
  test.it("a1", ({expect, ctx}) => {

  });

  describe.withLifecycle("B", {
    beforeAll: parentCtx => ctx,
    beforeEach: // it's fine actually
  })
  
})

aAll = A.beforeAll()
  aEach = A.beforeEach(aAll)
    a1(aEach)
  A.afterEach(aEach)
  bAll = B.beforeAll(aAll)
    aEach = A.beforeEach(aAll)
    bEach = B.beforeEach(bAll) // also aEach?
      b1(bEach)
    B.afterEach(bEach)
    A.afterEach(aEach)
    aEach = A.beforeEach(aAll)
    bEach = B.beforeEach(bEach) // also aEach
      b2(bEach)
    B.afterEach(bEach)
    A.afterEach(aEach)
  B.afterAll(bAll)
A.afterAll(aAll)

*/

type suiteResult =
  | BeforeError(string)
  | Results({
      afterErr: option(string),
      tests: list(childResult),
    })
and testResult =
  | BeforeEachError(string)
  | TestResult({
      after: option(string),
      err: option(string),
    })
and childResult =
  | SuiteResult({
      name: string,
      result: suiteResult,
    })
  | TestResult({
      name: string,
      result: testResult,
    });

let catcher = fn => switch (fn()) {
  | exception (Expect(err)) => Error("Expectation error: " ++ err)
  | exception (Failure(err)) => Error("Failure: " ++ err)
  | exception exn => Error("Other error: " ++ Printexc.to_string(exn))
  | x => Ok(x)
}

// let runTest = (name, body, suite) => {

// }

type parentArgs('all, 'each) = {
  v: 'all,
  beforeEach: unit => 'each,
  afterEach: 'each => unit,
};

let rec runSuite:
  type parentEnv parentEach.
  (lockedSuite(parentEnv), parentArgs(parentEnv, parentEach)) => suiteResult
 =
  (LockedSuite(suite): lockedSuite(parentEnv), parent: parentArgs(parentEnv, parentEach)) => (
    {
      switch (catcher(() => suite.lc.beforeAll(parent.v))) {
      | Error(err) => BeforeError(err)
      | Ok(beforeAll) =>
        let tests =
          suite.children
          ->Belt.List.map(child =>
              switch (child) {
              | Suite(LockedSuite({name}) as childSuite) =>
                SuiteResult({
                  name,
                  result:
                    runSuite(
                      childSuite,
                      {
                        v: beforeAll,
                        beforeEach: () => {
                          let parentEach = parent.beforeEach();
                          switch (suite.lc.beforeEach(beforeAll)) {
                          | exception exn =>
                            try(parent.afterEach(parentEach)) {
                            | _ => ()
                            };
                            raise(exn);
                          | before => (parentEach, before)
                          };
                        },
                        afterEach: ((parentEach, each)) => {
                          let after =
                            switch (suite.lc.afterEach(each)) {
                            | exception exn => Some(exn)
                            | () => None
                            };
                          let parent =
                            switch (parent.afterEach(parentEach)) {
                            | exception exn => Some(exn)
                            | () => None
                            };
                          switch (parent, after) {
                          | (Some(exn), _) => raise(exn)
                          | (_, Some(exn)) => raise(exn)
                          | _ => ()
                          };
                        },
                      },
                    ),
                })
              | Test(name, body) =>
                TestResult({
                  name,
                  result: {
                    switch (catcher(parent.beforeEach)) {
                    | Error(err) => BeforeEachError(err)
                    | Ok(parentEach) =>
                      switch (catcher(() => suite.lc.beforeEach(beforeAll))) {
                      | Error(err) => BeforeEachError(err)
                      | Ok(ctx) =>
                        let err =
                          switch (catcher(() => body({expect, ctx}))) {
                          | Error(err) => Some(err)
                          | Ok () => None
                          };
                        TestResult({
                          err,
                          after: {
                            let parentErr = catcher(() => parent.afterEach(parentEach));
                            let afterErr = catcher(() => suite.lc.afterEach(ctx));
                            switch (parentErr, afterErr) {
                            | (Error(err), _) => Some(err)
                            | (_, Error(err)) => Some(err)
                            | _ => None
                            };
                          },
                        });
                      }
                    };
                  },
                })
              }
            );
        Results({
          tests,
          afterErr:
            switch (catcher(() => suite.lc.afterAll(beforeAll))) {
            | Error(err) => Some(err)
            | _ => None
            },
        });
      };
    }: suiteResult
    // TODO have a way to report both errors
  );
runSuite(LockedSuite(rootSuite), {v: (), beforeEach: () => (), afterEach: () => ()});

// suite.children->Belt.List.forEach((Suite({name, lc, children})) => {
//     print_endline(name);
//     let allEnv = lc.beforeAll();
//     children->Belt.List.forEach(child => {
//       switch child {
//         | Test(name, body) =>
//       switch (
//         {
//           lc.beforeEach(allEnv);
//         }
//       ) {
//       | exception (Expect(err)) => print_endline("Expectation error: " ++ err)
//       | exception (Failure(err)) => print_endline("Failure: " ++ err)
//       | exception exn => print_endline("Other error: " ++ Printexc.to_string(exn))
//       | before =>
//         print_endline(name);
//         switch (
//           {
//             body({env: before, expect});
//           }
//         ) {
//         | exception (Expect(err)) => print_endline("Expectation error: " ++ err)
//         | exception (Failure(err)) => print_endline("Failure: " ++ err)
//         | exception exn => print_endline("Other error: " ++ Printexc.to_string(exn))
//         | () =>
//           switch (
//             {
//               lc.afterEach(before);
//             }
//           ) {
//           | exception (Expect(err)) => print_endline("Expectation error: " ++ err)
//           | exception (Failure(err)) => print_endline("Failure: " ++ err)
//           | exception exn => print_endline("Other error: " ++ Printexc.to_string(exn))
//           | () => print_endline("Done")
//           }
//         };
//       }
//       | _ => ()
//       }
//     });
//   });
// }

/*

 type suite('parentEnv, 'allEnv, 'eachEnv) = {
   namespace: list(string),
   lifecycle: lifecycle('parentEnv, 'allEnv, 'eachEnv),
   mutable children: list(suiteOrTest('eachEnv)),
 }

 and lifecycle('parentEnv, 'allEnv, 'eachEnv) = {
   beforeAll: 'parentEnv => 'allEnv,
   beforeEach: 'allEnv => 'eachEnv,
   afterEach: 'eachEnv => unit,
   afterAll: 'allEnv => unit,
 }

 and test('eachEnv) = {
   tnamespace: list(string),
   body: itArgs('eachEnv) => unit,
 }
 and suiteOrTest('eachEnv) =
   | Suite(suite('eachEnv, 'a, 'b)): suiteOrTest('eachEnv)
   | Test(test('eachEnv))

 and describeFn('parentEnv, 'allEnv, 'eachEnv) =
   (
     string,
     ~lifecycle: lifecycle('parentEnv, 'allEnv, 'eachEnv),
     describeArgs('eachEnv) => unit
   ) =>
   unit
 and itFn('eachEnv) = (string, itArgs('eachEnv) => unit) => unit
 and describeArgs('eachEnv) = {
   describe: 'a 'b. describeFn('eachEnv, 'a, 'b),
   it: itFn('eachEnv),
 }
 and itArgs('eachEnv) = {
   env: 'eachEnv,
   expect,
   fail: string => unit,
 };

 let add = (suite, child) => {
   suite.children = [child, ...suite.children];
 };

 let it = (parent, testName, body) => {
   parent->add(Test({tnamespace: [testName, ...parent.namespace], body}));
 };

 let rec describe: 'b 'c. describeFn(unit, 'b, 'c) =
   (suiteName, ~lifecycle: lifecycle(unit, 'b, 'c), body: describeArgs('c) => unit) => {
     let suite = {
       namespace: [suiteName], // , ...parent.namespace],
       children: [],
       lifecycle,
     };
     body({describe, it: it(suite)});
   };

 // describe("A", ({beforeEach, it, describe}) => {
 //   beforeAll(() => 10);
 //   beforeEach(env => {env * 2});

 //   it("B", ({expect, env}) => {
 //     failwith("Bad news bears")
 //   });

 //   describe("C", ({beforeEach, it}) => {
 //     beforeEach()
 //   });
 // });
 */