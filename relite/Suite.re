
open Types;

type maker = {
  make: 'parentEnv 'allEnv 'eachEnv . (maker, suite('parentEnv, 'allEnv, 'eachEnv)) => describeWithOptions('allEnv),
}

let describeArgs = (inner, maker) => {
  it: (name, body) => inner.children = [Test(name, body), ...inner.children],
  describe: maker.make(maker, inner),
};
let plainLc = {beforeAll: x => x, beforeEach: x => x, afterEach: _ => (), afterAll: _ => ()};
let suite = suite => Suite(LockedSuite(suite));

let makeDescribe = (maker, parent) => {
  let addSuite = (name, lc, skip, body) => {
    let inner = {name, lc, skip, children: []};
    body(describeArgs(inner, maker));
    parent.children = [
      suite(inner),
      ...parent.children,
    ];
  };
  let describeWithOptions = {
    plain: (name, body) => addSuite(name, plainLc, None, body),
    withLifecycle: (name, lc, body) => addSuite(name, lc, None, body),
    skipIf: fn => {
      plain: (name, body) => addSuite(name, plainLc, Some(fn), body),
      withLifecycle: (name, lc, body) => addSuite(name, lc, Some(fn), body),
    },
    skip: {
      plain: (name, body) => addSuite(name, plainLc, Some(_ => true), body),
      withLifecycle: (name, lc, body) => addSuite(name, lc, Some(_ => true), body),
    },
  };
  describeWithOptions;
};
let maker = {make: makeDescribe};

let rootLc = {beforeAll: () => (), beforeEach: () => (), afterEach: () => (), afterAll: () => ()};
let root = () => {name: "Root", lc: rootLc, skip: None, children: []};
let describe = suite => makeDescribe(maker, suite);

let beforeAll = beforeAll => {
  beforeAll,
  beforeEach: x => x,
  afterEach: _ => (),
  afterAll: _ => (),
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