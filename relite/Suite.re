
open Types;

type maker = {
  make: 'parentEnv 'allEnv 'eachEnv . (maker, suite('parentEnv, 'allEnv, 'eachEnv)) => describeWithOptions('allEnv),
}


let describeArgs = (inner, describe) => {
  it: (name, body) => inner.children = [Test(name, body), ...inner.children],
  describe,
};
let plainLc = {beforeAll: x => x, beforeEach: x => x, afterEach: _ => (), afterAll: _ => ()};
let suite = suite => Suite(LockedSuite(suite));

let makeDescribe = (maker, parent) => {
  let describeWithOptions = {
    plain: (name, body) => {
      let inner = {name, lc: plainLc, skipped: false, children: []};
      body(describeArgs(inner, maker.make(maker, inner)));
      parent.children = [
        suite(inner),
        ...parent.children,
      ];
    },
    withLifecycle: (name, lc, body) => {
      let inner = {name, lc, skipped: false, children: []}
      body(describeArgs(inner, maker.make(maker, inner)));
      parent.children = [
        suite(inner),
        ...parent.children,
      ];
    },
    skip: {
      plain: (name, body) => {
        let inner = {name, lc: plainLc, skipped: true, children: []};
        body(describeArgs(inner, maker.make(maker, inner)));
        parent.children = [
          suite(inner),
          ...parent.children,
        ];
      },
      withLifecycle: (name, lc, body) => {
        let children = ref([]);
        let inner = {name, lc, skipped: true, children: []};
        body(describeArgs(inner, maker.make(maker, inner)));
        parent.children = [
          suite(inner),
          ...parent.children,
        ];
      },
    },
  };
  describeWithOptions;
};
let maker = {make: makeDescribe};
// let describe = makeDescribe(maker, rootSuite);

let rootLc = {beforeAll: () => (), beforeEach: () => (), afterEach: () => (), afterAll: () => ()};
let root = () => {name: "Root", lc: rootLc, skipped: false, children: []};
let describe = suite => makeDescribe(maker, suite);

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