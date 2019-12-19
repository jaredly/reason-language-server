Printexc.record_backtrace(true);

exception Expect(string);

type stringExpect = {toEqual: (~message: string=?, string) => unit};
type intExpect = {toEqual: (~message: string=?, int) => unit};
type expect = {
  string: string => stringExpect,
  int: int => intExpect,
};

let expect = {
  string: actual => {toEqual: (~message=?, expected) => {
    if (actual != expected) {
      failwith(Printf.sprintf("Expected %s to equal %s : %s", actual, expected, switch message {
        | None => ""
        | Some(m) => m
      }))
    }
  }},
  int: actual => {toEqual: (~message=?, expected) => {
    if (actual != expected) {
      failwith(Printf.sprintf("Expected %d to equal %d : %s", actual, expected, switch message {
        | None => ""
        | Some(m) => m
      }))
    }
  }}
}

type lifecycle('parentEnv, 'allEnv, 'eachEnv) = {
  beforeAll: 'parentEnv => 'allEnv,
  beforeEach: 'allEnv => 'eachEnv,
  afterEach: 'eachEnv => unit,
  afterAll: 'allEnv => unit,
}

type describeArgs('env) = {
  it: (string, testArgs('env) => unit) => unit
}
and describe('parentEnv) = {
  withLifecycle: 'allEnv 'eachEnv . (string, lifecycle('parentEnv, 'allEnv, 'eachEnv), describeArgs('eachEnv) => unit) => unit,
}
and testArgs('env) = {
  expect,
  env: 'env
};

type test('eachEnv) = (string, (testArgs('eachEnv)) => unit);
type suite('parentEnv) = Suite(string, lifecycle('parentEnv, 'allEnv, 'eachEnv), list(test('eachEnv))) : suite('parentEnv)
let suites = ref([]);

let describe = {
  withLifecycle: (name, lc, body) => {
    let children = ref([]);
    body({it: (name, body) => children.contents = [(name, body), ...children.contents]});
    suites.contents = [Suite(name, lc, children.contents)]
  }
}

describe.withLifecycle("A", {
  beforeAll: () => 10,
  beforeEach: string_of_int,
  afterEach: (s: string) => (),
  afterAll: (x: int) => (),
}, ({it}) => {
  it("x", ({expect, env}) => {
    expect.string(env).toEqual("10")
  })
});

suites.contents->Belt.List.forEach((Suite(name, lc, tests)) => {
  print_endline(name);
  let allEnv = lc.beforeAll(());
  tests->Belt.List.forEach(((name, body)) => {
    switch {
      lc.beforeEach(allEnv)
    } {
      | exception Expect(err) => print_endline("Expectation error: " ++ err)
      | exception Failure(err) => print_endline("Failure: " ++ err)
      | exception exn => print_endline("Other error: " ++ Printexc.to_string(exn))
      | before =>
        print_endline(name);
        switch {
          body({env: before, expect})
        } {
          | exception Expect(err) => print_endline("Expectation error: " ++ err)
          | exception Failure(err) => print_endline("Failure: " ++ err)
          | exception exn => print_endline("Other error: " ++ Printexc.to_string(exn))
          | () =>
            switch {
              lc.afterEach(before)
            } {
              | exception Expect(err) => print_endline("Expectation error: " ++ err)
              | exception Failure(err) => print_endline("Failure: " ++ err)
              | exception exn => print_endline("Other error: " ++ Printexc.to_string(exn))
              | () => print_endline("Done")
            }
        }
    };
  })
})






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