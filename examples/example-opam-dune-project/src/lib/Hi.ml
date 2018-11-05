let hullo = "hi world"

let f1 = 1

(* let%test _ = f1 = 2 *)

(* the following should fail `dune runtest` *)
let%expect_test "ppx_expect test" =
  print_endline "actual";
  [%expect {|
    expected
  |}]
