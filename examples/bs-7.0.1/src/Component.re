module Css = ComponentStyles;

let component = ReasonReact.statelessComponent(__MODULE__);

let make = (~size, ~animate, _) => {
  ...component,
  render: _ =>
    <Layout>
      <div className=Css.container>
        <div className=Css.shape>
          <div
            className={
              Cn.make([Css.text(~size), Css.animated->Cn.ifTrue(animate)])
            }>
            "Hi!"->ReasonReact.string
          </div>
        </div>
        <div className=Css.note>
          {j|↑ Hover it ↑|j}->ReasonReact.string
        </div>
        <h2> "Grid"->ReasonReact.string </h2>
        <div className=Css.grid>
          <div className={Cn.make([Css.gridItem, Css.gridItem1])}>
            "1"->ReasonReact.string
          </div>
          <div className=Css.gridItem> "2"->ReasonReact.string </div>
          <div className=Css.gridItem> "3"->ReasonReact.string </div>
          <div className=Css.gridItem> "4"->ReasonReact.string </div>
          <div className=Css.gridItem> "5"->ReasonReact.string </div>
          <div className=Css.gridItem> "6"->ReasonReact.string </div>
          <div className=Css.gridItem> "7"->ReasonReact.string </div>
        </div>
      </div>
    </Layout>,
};
