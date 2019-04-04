Printexc.record_backtrace(true);

switch (Sys.argv) {
  | [|_, merlinFile|] =>
    let modules = Util.MerlinFile.getModulesFromMerlin(Filename.dirname(merlinFile), Util.Files.readFileExn(merlinFile))
    print_endline("Ok")
  | _ => failwith("bad args")
}