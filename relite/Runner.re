open Types;

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
  | ChildTestResult({
      name: string,
      result: testResult,
    });

let catcher = fn =>
  switch (fn()) {
  | exception (Expect(err)) => Error("Expectation error: " ++ err)
  | exception (Failure(err)) => Error("Failure: " ++ err)
  | exception exn => Error("Other error: " ++ Printexc.to_string(exn))
  | x => Ok(x)
  };

type parentArgs('all, 'each) = {
  v: 'all,
  beforeEach: unit => 'each,
  afterEach: 'each => unit,
};

type event =
  | SuiteStart(list(string))
  | SuiteEnd(list(string), suiteResult)
  | TestStart(string, list(string))
  | TestEnd(string, list(string), testResult)

let rec runSuite:
  type parentEnv parentEach.
    (expect, event => unit, list(string), lockedSuite(parentEnv), parentArgs(parentEnv, parentEach)) => suiteResult =
  (
    expect,
    report,
    trail,
    LockedSuite(suite): lockedSuite(parentEnv),
    parent: parentArgs(parentEnv, parentEach),
  ) => {
    let trail = [suite.name, ...trail];
    report(SuiteStart(trail));
    let result = switch (catcher(() => suite.lc.beforeAll(parent.v))) {
    | Error(err) => BeforeError(err)
    | Ok(beforeAll) =>
      let tests =
        suite.children->List.rev
        ->Belt.List.map(child =>
            switch (child) {
            | Suite(LockedSuite({name}) as childSuite) =>
              SuiteResult({
                name,
                result:
                  runSuite(
                    expect,
                    report,
                    trail,
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
              report(TestStart(name, trail));
              let result = {
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
                          let afterErr = catcher(() => suite.lc.afterEach(ctx));
                          let parentErr = catcher(() => parent.afterEach(parentEach));
                          switch (parentErr, afterErr) {
                          | (Error(err), _) => Some(err)
                          | (_, Error(err)) => Some(err)
                          | _ => None
                          };
                        },
                      });
                    }
                  };
                }
              report(TestEnd(name, trail, result));
              ChildTestResult({
                name,
                result,
              });
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
    // TODO have a way to report both errors
    };
    report(SuiteEnd(trail, result));
    result
  };

let run = (~report=x => (), expect, suite) => runSuite(expect, report, [], LockedSuite(suite), {
  v: (),
  beforeEach: () => (),
  afterEach: () => (),
});