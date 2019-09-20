let debug: ref(bool);
let fixPpx: (string, string) => string;
let parseMerlin:
  (~isNative: bool, string, string) => (list(string), list(string), list(string));
let isRelativePath: string => bool;
let isBuildFile: string => bool;
let isSourceFile: string => bool;
let tryReduce: (list('a), 'b, ('b, 'a) => result('b, 'e)) => result('b, 'e);
let maybeHash: (Hashtbl.t('a, 'b), 'a) => option('b);
let (|??): (option('a), option('a)) => option('a);
let orLog: result('a, string) => option('a);
type files = {
  src: option(string),
  srci: option(string),
  cmi: option(string),
  cmt: option(string),
  cmti: option(string),
};
let orBlank: option(string) => string;
let showFiles: files => string;
let calcPaths:
  (string, files) =>
  result(
    [>
      | `Impl(string, option(string))
      | `Intf(string, option(string))
      | `IntfAndImpl(string, option(string), string, option(string))
    ],
    string,
  );
let getModulesFromMerlin:
  (~stdlibs: list(string), ~isNative: bool, string, string) =>
  (
    Hashtbl.t(
      string,
      [
        | `Impl(string, option(string))
        | `Intf(string, option(string))
        | `IntfAndImpl(string, option(string), string, option(string))
      ],
    ),
    Hashtbl.t(string, string),
    list(string),
    list(string),
    list(string),
  );
let getFlags: (~isNative: bool, string) => RResult.result(list(string), string);
let getBackend: string => RResult.result(string, string);