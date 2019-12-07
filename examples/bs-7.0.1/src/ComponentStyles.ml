open Emotion

let container = [%css [
  display `flex;
  flexFlow `column `nowrap;
  alignItems `center;
]]

let shape = [%css [
  display `flex;
  flexFlow `row `nowrap;
  alignItems `center;
  justifyContent `center;
  transitionProperty "border-radius";
  transitionDuration (`ms 100);
  transitionTimingFunction `easeInOut;
  width (`px 200);
  height (`px 200);
  borderRadius (`px 6);
  backgroundColor (`hex "29d");

  (* :hover selector, same as `select ":hover" [ ... ]` *)
  hover [
    borderRadius (`pct 50.);
    important (cursor `grab);
  ];
]]

(* Dynamic styling *)
(* NOTE: ppx supports functions with max 2 arguments *)
let text ~size = [%css [
  color (`hex "fff");
  fontSize (`px size);
  fontWeight 700;

  (* Transition takes property, duration, timing-function & delay *)
  transition "font-size" (`ms 100) `easeInOut `zero;

  (* You can define multiple transitions by packing them into list of tuples *)
  transitions [
    ("font-size", `ms 100, `easeInOut, `ms 0);
  ];

  (* Complex selector that uses .container class defined above *)
  (* Rendered as: `.container:hover .text {...}` *)
  select {j|.$container:hover &|j} [
    fontSize Calc.(((`px size) + (`pct 150.)) * (`n 1.5));
  ];

  (* @media quiery with nested selectors *)
  media "(max-width: 900px)" [
    color (`hex "ff69b4");

    select ":hover" [
      color (`hex "fff");
    ];
  ];
]]

(* Define keyframes *)
let bounce = keyframes [
  (0,   [ transform (`translateY `zero); ]);
  (50,  [ transform (`translateY (`px (-20))); ]);
  (100, [ transform (`translateY `zero); ]);
]

let animated = [%css [
  (* Use generated animation name *)
  animationName bounce;
  animationDuration (`ms 300);
  animationIterationCount (`i 7);
]]

(* Compose things *)
let smallText = [%css [
  fontSize (`em 0.8);
]]

let note = css ~extend: smallText [
  label "note";

  margin2 (`px 20) `zero;
]

(* Grid *)
let grid = [%css [
  display `grid;
  gridTemplateColumns (`list [`repeat (`n 3, [`px 100;]);]);
  gridAutoRows (`px 100);
  gridGap (`px 10);
  gridTemplateAreas (`areas ["a a a"; ". . ."; ". . .";]);
]]

let gridItem = [%css [
  padding (`px 10);
  color (`hex "fff");
  backgroundColor (`hex "29d");
]]

let gridItem1 = [%css [
  gridArea "a";
]]
