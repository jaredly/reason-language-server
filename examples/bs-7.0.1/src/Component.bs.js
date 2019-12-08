// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var Cn = require("re-classnames/src/Cn.bs.js");
var React = require("react");
var Layout = require("./Layout.bs.js");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var ComponentStyles = require("./ComponentStyles.bs.js");

var component = ReasonReact.statelessComponent("Component");

function make(size, animate, param) {
  return {
          debugName: component.debugName,
          reactClassInternal: component.reactClassInternal,
          handedOffState: component.handedOffState,
          willReceiveProps: component.willReceiveProps,
          didMount: component.didMount,
          didUpdate: component.didUpdate,
          willUnmount: component.willUnmount,
          willUpdate: component.willUpdate,
          shouldUpdate: component.shouldUpdate,
          render: (function (param) {
              return ReasonReact.element(undefined, undefined, Layout.make(/* array */[React.createElement("div", {
                                    className: ComponentStyles.container
                                  }, React.createElement("div", {
                                        className: ComponentStyles.shape
                                      }, React.createElement("div", {
                                            className: Cn.make(/* :: */[
                                                  ComponentStyles.text(size),
                                                  /* :: */[
                                                    Cn.ifTrue(ComponentStyles.animated, animate),
                                                    /* [] */0
                                                  ]
                                                ])
                                          }, "Hi!")), React.createElement("div", {
                                        className: ComponentStyles.note
                                      }, "↑ Hover it ↑"), React.createElement("h2", undefined, "Grid"), React.createElement("div", {
                                        className: ComponentStyles.grid
                                      }, React.createElement("div", {
                                            className: Cn.make(/* :: */[
                                                  ComponentStyles.gridItem,
                                                  /* :: */[
                                                    ComponentStyles.gridItem1,
                                                    /* [] */0
                                                  ]
                                                ])
                                          }, "1"), React.createElement("div", {
                                            className: ComponentStyles.gridItem
                                          }, "2"), React.createElement("div", {
                                            className: ComponentStyles.gridItem
                                          }, "3"), React.createElement("div", {
                                            className: ComponentStyles.gridItem
                                          }, "4"), React.createElement("div", {
                                            className: ComponentStyles.gridItem
                                          }, "5"), React.createElement("div", {
                                            className: ComponentStyles.gridItem
                                          }, "6"), React.createElement("div", {
                                            className: ComponentStyles.gridItem
                                          }, "7")))]));
            }),
          initialState: component.initialState,
          retainedProps: component.retainedProps,
          reducer: component.reducer,
          jsElementWrapped: component.jsElementWrapped
        };
}

var Css = /* alias */0;

exports.Css = Css;
exports.component = component;
exports.make = make;
/* component Not a pure module */