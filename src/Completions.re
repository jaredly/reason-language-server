
open State;

let isCapitalized = name => name.[0] >= 'A' && name.[0] <= 'Z';

let get = (scope, name, state) => {
  if (scope == [] && isCapitalized(name)) {
    let results = List.fold_left((results, (k, _)) => Utils.startsWith(k, name) ? [k, ...results] : results, [], state.localModules);
    let results = List.fold_left((results, (k, _)) => switch k {
    | FindFiles.Plain(k) => Utils.startsWith(k, name) ? [k, ...results] : results
    | _ => results
    }, results, state.dependencyModules);
    results
  } else {
    []
  }
};
