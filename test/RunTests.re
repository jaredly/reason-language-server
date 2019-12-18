print_endline("Start of RunTests.re file");
include ExamplesTests;
print_endline("After including ExamplesTests");
include AnalyzeFixtureTests;
print_endline("After including Fixture Tests");
include UtilTests;
print_endline("After utilTests, ready for cli");
TestFramework.cli()
print_endline("Done");

// Things I wish rely would do:
// - watch mode would be awesome
// - allow you to select a subset of the tests to run, from cli args
// - allow you to nest describes with beforeEach n stuff
// - display the sub-items of a describe as they're being run (at least sub-describes)
//   - it would be fine to use readline to replace them one after another, but I just want
//     feedback that things are happening.