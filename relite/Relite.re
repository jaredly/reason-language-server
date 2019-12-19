Printexc.record_backtrace(true);

type stringExpect = {toEqual: (~message: string=?, string) => unit};

type intExpect = {toEqual: (~message: string=?, int) => unit};

type expect = {
  string: string => stringExpect,
  int: int => intExpect,
};

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