module Css = LayoutStyles;

let component = ReasonReact.statelessComponent(__MODULE__);

let make = children => {
  ...component,
  render: _ =>
    <div className=Css.container>
      <main className=Css.main> ...children </main>
      <footer className=Css.footer />
    </div>,
};
