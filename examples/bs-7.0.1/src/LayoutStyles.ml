open Emotion

let container = [%css [
  display `grid;
  gridTemplateRows (`list [`fr 1.; `maxContent;]);
  gridTemplateColumns (`list [`fr 1.;]);
]]

let main = [%css [
  display `grid;
  alignItems `center;
  justifyItems `center;
]]

let footer = [%css [
  display `grid;
  alignItems `center;
  justifyItems `center;
  padding (`px 20);
  backgroundColor (`hex "f7f7f7");
]]
