
type result('ok, 'error) = Ok('ok) | Error('error);

let resultOfOption = (err, v) => switch v { | Some(v) => Ok(v) | None => Error(err)};
let orError = resultOfOption;

module InfixResult = {
  let (|?>) = (a, fn) => switch a { | Ok(a) => fn(a) | Error(e) => Error(e) };
  let (|.>) = (fa, fb, value) => fb(fa(value));
  let (|?>>) = (a, fn) => switch a { | Ok(a) => Ok(fn(a)) | Error(e) => Error(e) };
  let (|?) = (a, default) => switch a { | Ok(a) => a | Error(e) => default };
  let (|!) = (a, message) => switch a { | Ok(a) => a | Error(e) => failwith(message) };
};
open InfixResult;
let withDefault = (d, v) => v |? d;