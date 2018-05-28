let out = ref(open_out("/Users/jared/ls.log"));

let setLocation = location => {
  close_out(out^);
  output_string(stderr, "Setting log location: " ++ location ++ "\n");
  out := open_out(location);
};

let log = msg => {
  output_string(stderr, msg ++ "\n");
  output_string(out^, msg ++ "\n");
  flush(out^);
};
