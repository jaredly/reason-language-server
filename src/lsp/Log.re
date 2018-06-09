
let out = ref(None);
out := Some(open_out("lsp.log"));
/*open_out("/tmp/lsp.log"));*/

let setLocation = location => {
  switch (out^) {
    | None => ()
    | Some(out) =>
  close_out(out);
  };
  output_string(stderr, "Setting log location: " ++ location ++ "\n");
  out := Some(open_out(location));
};

let log = msg => {
  output_string(stderr, msg ++ "\n");
  switch (out^) {
    | None => ()
    | Some(out) => {
  output_string(out, msg ++ "\n");
  flush(out);

    }
  }
};
