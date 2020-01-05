exception Expect(string);

type lifecycle('parentEnv, 'allEnv, 'eachEnv) = {
  beforeAll: 'parentEnv => 'allEnv,
  beforeEach: 'allEnv => 'eachEnv,
  afterEach: 'eachEnv => unit,
  afterAll: 'allEnv => unit,
};

type stringExpect = {toEqual: (~message: string=?, string) => unit};
type intExpect = {toEqual: (~message: string=?, int) => unit};
type expect = {
  string: string => stringExpect,
  int: int => intExpect,
};

type describeArgs('parentEnv, 'allEnv, 'eachEnv) = {
  it: (string, testArgs('eachEnv) => unit) => unit,
  describe: describeWithOptions('allEnv),
}
and describe('parentEnv) = {
  plain: (string, describeArgs('parentEnv, 'parentEnv, 'parentEnv) => unit) => unit,
  withLifecycle:
    'allEnv 'eachEnv.
    (
      string,
      lifecycle('parentEnv, 'allEnv, 'eachEnv),
      describeArgs('parentEnv, 'allEnv, 'eachEnv) => unit
    ) =>
    unit,

}
and describeWithOptions('parentEnv) = {
  plain: (string, describeArgs('parentEnv, 'parentEnv, 'parentEnv) => unit) => unit,
  withLifecycle:
    'allEnv 'eachEnv.
    (
      string,
      lifecycle('parentEnv, 'allEnv, 'eachEnv),
      describeArgs('parentEnv, 'allEnv, 'eachEnv) => unit
    ) =>
    unit,

  skip: describe('parentEnv),
  skipIf: ('parentEnv => bool) => describe('parentEnv),
}
and testArgs('env) = {
  expect,
  ctx: 'env,
}
and child('allEnv, 'eachEnv) =
  | Test(string, testArgs('eachEnv) => unit)
  | Suite(lockedSuite('allEnv))
and lockedSuite('parentEnv) =
  | LockedSuite(suite('parentEnv, 'allEnv, 'eachEnv)): lockedSuite('parentEnv)
and suite('parentEnv, 'allEnv, 'eachEnv) = {
  name: string,
  skip: option('parentEnv => bool),
  lc: lifecycle('parentEnv, 'allEnv, 'eachEnv),
  mutable children: list(child('allEnv, 'eachEnv)),
};
