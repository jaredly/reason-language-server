
type result('ok, 'error) = Belt.Result.t('ok, 'error) = Ok('ok) | Error('error);

let resultOfOption = (err, v) => switch v { | Some(v) => Ok(v) | None => Error(err)};
let orError = resultOfOption;

let toOptionAndLog = err => switch err {
  | Error(e) => Log.log(e); None
  | Ok(v) => Some(v)
};

module InfixResult = {
  let (|?>) = (a, fn) => switch a { | Ok(a) => fn(a) | Error(e) => Error(e) };
  let (|.>) = (fa, fb, value) => fb(fa(value));
  let (|?>>) = (a, fn) => switch a { | Ok(a) => Ok(fn(a)) | Error(e) => Error(e) };
  let (|?) = (a, default) => switch a { | Ok(a) => a | Error(_) => default };
  let (|!) = (a, message) => switch a { | Ok(a) => a | Error(_) => failwith(message) };
};
open InfixResult;
let withDefault = (d, v) => v |? d;
let map = (fn, r) => r |?>> fn;
let bind = (fn, r) => r |?> fn;