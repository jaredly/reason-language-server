// let root = Suite.root();
// let describe = Suite.describe(root);
let {Relite.describe, run} = Relite.init();

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

print_endline("Relite example:")
// Runner.run(~report=Reporter.report, expect, root)
run();