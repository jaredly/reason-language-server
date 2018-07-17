let component = ReasonReact.statelessComponent("Box");
let make = _children => {
  ...component,
  render: _self =>
    <div>
      (ReasonReact.string("Whats in the box?.... it's: " ++ _children))
    </div>,
};