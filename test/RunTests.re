include ExamplesTests;
include AnalyzeFixtureTests;
include UtilTests;
TestFramework.cli()

// Things I wish rely would do:
// - watch mode would be awesome
// - allow you to select a subset of the tests to run, from cli args
// - allow you to nest describes with beforeEach n stuff
// - display the sub-items of a describe as they're being run (at least sub-describes)
//   - it would be fine to use readline to replace them one after another, but I just want
//     feedback that things are happening.