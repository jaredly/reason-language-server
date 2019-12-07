open Emotion;;

begin
  global "html" [
    minHeight (`pct 100.);
    fontFamily "Tahoma, sans-serif";
  ];

  global "html, body, #app" [
    display `grid;
    gridTemplateRows (`list [`fr 1.;]);
    gridTemplateColumns (`list [`fr 1.;]);
    gridGap `zero;
    margin `zero;
    padding `zero;
  ];

  fontFace [
    fontFamily "MyFont";
    fontWeight 400;
    fontStyle `normal;
    src [
      (`url "my-font.woff", Some `woff);
      (`url "my-font.woff2", Some `woff2);
    ];
  ];
end
