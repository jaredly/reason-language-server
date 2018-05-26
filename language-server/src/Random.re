
/* Couldn't manage to trigger the showReferences pane, unfortunately.
 * Might be able to do it from javascript tho.
 * see https://github.com/Microsoft/vscode/blob/8835c06e1aaa6a9abbb29e3ee6ea4d9e376ecb0a/src/vs/workbench/parts/codeEditor/electron-browser/workbenchReferenceSearch.ts
 */
      /* o([
        ("range", range(~start=pos(~line=3, ~character=0), ~end_=pos(~line=3, ~character=0))),
        ("command", o([
          ("title", s("3 references")),
          ("command", s("editor.action.showReferences")),
          ("arguments", l([
            /* o([
              ("uri", s("file://" ++ state.rootPath)),
            ]), */
            o([
              ("scheme", s("file")),
              ("authority", s("")),
              ("path", s(state.rootPath)),
              ("query", s("")),
              ("fragment", s(""))
            ]),
            pos(~line=0, ~character=0),
            l([
              pos(~line=4, ~character=0)
            ])
          ]))
        ]))
      ]) */
