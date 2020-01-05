open Types;

type filter =
  | Substr(string)
  | Exactly(list(list(string)))

let contains = (haystack, needle) => {
  switch (Str.search_forward(Str.regexp_string(needle), haystack, 0)) {
    | exception Not_found => false
    | _ => true
  }
};

let rec filterChild
: 'a 'b . (list(string), filter, child('a, 'b)) => option(child('a, 'b))
 = (trail, filter, child) => switch child {
  | Test(name, body) => (switch filter {
    | Substr(needle) => name->contains(needle)
    | Exactly(items) => items->Belt.List.has([name, ...trail], (==))
  }) ? Some(child) : None
  | Suite(LockedSuite({name, children} as suite)) => (switch filter {
    | Substr(needle) => name->contains(needle)
    | Exactly(items) => items->Belt.List.has([name, ...trail], (==))
  }) ? Some(child) : {
    let children = children->Belt.List.keepMap(filterChild([name, ...trail], filter));
    if (children != []) {
      Some(Suite(LockedSuite({...suite, children})))
    } else {
      None
    }
  }
};

let filterSuite = (suite, filter) => {
  let children = suite.children->Belt.List.keepMap(filterChild([], filter));
  {...suite, children}
}

type suiteResult =
  | BeforeError(string, int)
  | SuiteSkipped(int)
  | Results({
      afterErr: option(string),
      tests: list(childResult),
    })
and testResult =
  | BeforeEachError(string)
  | TestSkipped
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

let success = fun
  | TestResult({after: None, err: None}) => true
  | _ => false;

// let suiteSuccess = fun

type summary = {
  succeeded: int,
  skipped: int,
  errors: int,
  failed: int,
};

let addSummaries = (a, b) => {
  succeeded: a.succeeded + b.succeeded,
  skipped: a.skipped + b.skipped,
  errors: a.errors + b.errors,
  failed: a.failed + b.failed,
}

let rec summarize = childResults => childResults->Belt.List.reduce(
  {succeeded: 0, skipped: 0, errors: 0, failed: 0},
  (summary, result) => switch result {
  | ChildTestResult({result: BeforeEachError(_)}) => {...summary, errors: summary.errors + 1, skipped: summary.skipped + 1}
  | ChildTestResult({result: TestResult({after, err: None})}) => {...summary, succeeded: summary.succeeded + 1, errors: summary.errors + (after == None ? 0 : 1)}
  | ChildTestResult({result: TestSkipped}) => {...summary, skipped: summary.skipped + 1}
  | ChildTestResult({result: TestResult({after})}) => {...summary, failed: summary.failed + 1, errors: summary.errors + (after == None ? 0 : 1)}
  | SuiteResult({result: BeforeError(_, count)}) => {...summary, errors: summary.errors + 1, skipped: summary.skipped + count}
  | SuiteResult({result: SuiteSkipped(count)}) => {...summary, skipped: summary.skipped + count}
  | SuiteResult({result: Results({afterErr, tests})}) => addSummaries(
    {...summary, errors: summary.errors + (afterErr == None ? 0 : 1)},
    summarize(tests)
  )
});

let rec failingPaths = (trail, childResults) => childResults->Belt.List.reduce(
  [],
  (paths, result) => switch result {
  // Success or skipped
  | ChildTestResult({result: TestResult({err: None})})
  | ChildTestResult({result: TestSkipped})
  | SuiteResult({result: SuiteSkipped(_)}) => paths
  // Failing
  | ChildTestResult({name, result: BeforeEachError(_)})
  | ChildTestResult({name, result: TestResult(_)}) => [[name, ...trail], ...paths]
  | SuiteResult({name, result: BeforeError(_, count)}) => [[name, ...trail], ...paths]
  | SuiteResult({name, result: Results({tests})}) => failingPaths([name, ...trail], tests) @ paths
});

let suiteFailingPaths = fun
  | Results({tests}) => failingPaths([], tests)
  | _ => [];

let summarizeSuite = fun
  | BeforeError(_, count) => {failed: 0, succeeded: 0, skipped: count, errors: 1}
  | SuiteSkipped(count) => {failed: 0, succeeded: 0, skipped: count, errors: 0}
  | Results({
      afterErr,
      tests,
    }) => {
      let inner = summarize(tests);
      {...inner, errors: inner.errors + (afterErr == None ? 0 : 1)}
    };

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
  | Start((int, int))
  | SuiteStart(string, list(string))
  | SuiteEnd(string, list(string), suiteResult, float)
  | TestStart(string, list(string))
  | TestEnd(string, list(string), testResult)

let rec count
: 'a 'b 'c . Types.suite('a, 'b, 'c) => (int, int)
 = suite => suite.children->Belt.List.reduce((0, 0), ((tests, suites), child) => switch child {
  | Test(_) => (tests + 1, suites)
  | Suite(LockedSuite(suite)) => 
    let (ctests, csuites) = count(suite);
    (tests + ctests, suites + csuites + 1)
});

let rec runSuite:
  type parentEnv parentEach.
    (
      expect,
      event => unit,
      list(string),
      lockedSuite(parentEnv),
      parentArgs(parentEnv, parentEach),
    ) => suiteResult =
  (
    expect,
    report,
    trail,
    LockedSuite(suite): lockedSuite(parentEnv),
    parent: parentArgs(parentEnv, parentEach),
  ) => {
    let startTime = Unix.gettimeofday();
    report(SuiteStart(suite.name, trail));
    let skip = switch (suite.skip) {
      | None => false
      | Some(f) => f(parent.v)
    };
    let result = if (skip) { SuiteSkipped(count(suite) |> fst) } else {
      switch (catcher(() => suite.lc.beforeAll(parent.v))) {
    | Error(err) => BeforeError(err, count(suite) |> fst)
    | Ok(beforeAll) =>
      let trail = [suite.name, ...trail];
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
    }};
    report(SuiteEnd(suite.name, trail, result, Unix.gettimeofday() -. startTime));
    result
  };

let run = (~report=x => (), expect, suite) => {
  report(Start(count(suite)));
  runSuite(expect, report, [], LockedSuite(suite), {
    v: (),
    beforeEach: () => (),
    afterEach: () => (),
  })
};