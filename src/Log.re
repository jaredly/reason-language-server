let out = open_out("/Users/jared/ls.log");

let log = msg => {
  output_string(stderr, msg ++ "\n");
  output_string(out, msg ++ "\n");
  flush(out);
};
