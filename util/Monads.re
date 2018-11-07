type either('a, 'b) =
  | Left('a)
  | Right('b);

let leftForce = a => switch a { | Left(a) => a | Right(_) => failwith("Expected a left")};
let rightForce = a => switch a { | Right(a) => a | Left(_) => failwith("Expected a right")};

module type MonadThing = {
  type t('a);
  let return: 'a => t('a);
  let map: (t('a), ~f: 'a => 'b) => t('b);
  let bind: (t('a), ~f: 'a => t('b)) => t('b);
  let consume: (t('a), ~f: 'a => unit) => unit;
  let join2: (t('a), t('b)) => t(('a, 'b));
  /*let mapl: list (t 'a) => f::('a => 'b) => t (list 'b);*/
  /*let bindl: list (t 'a) => f::('a => t 'b) => t (list 'b);*/
  /* let first: t 'a => t 'b => t (either 'a 'b); */
};

/* module Promise = {
  type t('a) = Js.Promise.t('a);
  let return = Js.Promise.resolve;
  let map = (value, ~f) => value |> Js.Promise.then_(res => Js.Promise.resolve(f(res)));
  let bind = (value, ~f) => value |> Js.Promise.then_(f);
  let consume = (value, ~f) => value |> Js.Promise.then_(res => {f(res); Js.Promise.resolve(0)}) |> ignore;
  let join2 = (a, b) => Js.Promise.all([|map(a, ~f=(r => Left(r))), map(b, ~f=(r => Right(r)))|]) |> Js.Promise.then_(items => Js.Promise.resolve((leftForce(items[0]), rightForce(items[1]))));
}; */

/* module P: MonadThing = Promise; */

module Continuation = {
  type t('a) = ('a => unit) => unit;
  let return = (x, fin) => fin(x);
  let map = (work, ~f as use, fin) => work((result) => fin(use(result)));
  let bind = (work, ~f as use, fin) => work((result) => (use(result))(fin));
  let consume = (work, ~f as use) => work(use);
  type side('a, 'b) =
    | One('a)
    | Two('b)
    | Neither
    | Done;
  let join2 = (one, two, fin) => {
    let side = ref(Neither);
    one(
      (one) =>
        switch side^ {
        | Neither => side := One(one)
        | Two(two) =>
          side := Done;
          fin((one, two))
        /* not allowed to call multiple times */
        | One(_)
        | Done => ()
        }
    );
    two(
      (two) =>
        switch side^ {
        | Neither => side := Two(two)
        | One(one) =>
          side := Done;
          fin((one, two))
        /* not allowed to call multiple times */
        | Two(_)
        | Done => ()
        }
    )
  };
  let first = (one, two, fin) => {
    let finished = ref(false);
    one(
      (one) =>
        if (! finished^) {
          finished := true;
          fin(Left(one))
        }
    );
    two(
      (two) =>
        if (! finished^) {
          finished := true;
          fin(Right(two))
        }
    )
  };
};

let module C: MonadThing = Continuation;

module NodeContinuation = {
  open RResult;
  type t('a, 'b) = (RResult.result('a, 'b) => unit) => unit;
  let return = (x, fin) => fin(Ok(x));
  let map = (work, ~f as use, fin) => work((result) => fin(Ok(use(result))));
  let bind = (work, ~f as use, fin) => work((result) => (use(result))(fin));
  let consume = (work, ~f as use) => work(use);
  type side('a, 'b) =
    | One('a)
    | Two('b)
    | Neither
    | Done;
  let join2 = (one, two, fin) => {
    let side = ref(Neither);
    one(
      (oneRes) =>
        switch side^ {
        | Neither =>
          switch oneRes {
          | Ok(oneVal) => side := One(oneVal)
          | Error(err) =>
            side := Done;
            fin(Error(err))
          }
        | Two(twoVal) =>
          switch oneRes {
          | Ok(oneVal) =>
            side := Done;
            fin(Ok((oneVal, twoVal)))
          | Error(err) => fin(Error(err))
          }
        /* not allowed to call multiple times */
        | One(_)
        | Done => ()
        }
    );
    two(
      (two) =>
        switch side^ {
        | Neither =>
          switch two {
          | Ok(two) => side := Two(two)
          | Error(err) =>
            side := Done;
            fin(Error(err))
          }
        | One(one) =>
          switch two {
          | Ok(two) =>
            side := Done;
            fin(Ok((one, two)))
          | Error(err) => fin(Error(err))
          }
        /* not allowed to call multiple times */
        | Two(_)
        | Done => ()
        }
    )
  };
};

/* let module N: MonadThing = NodeContinuation; */

module Option = {
  type t('a) = option('a);
  let return = (x) => Some(x);
  let map = (value, ~f as use) =>
    switch value {
    | Some(x) => Some(use(x))
    | None => None
    };
  let bind = (value, ~f as use) =>
    switch value {
    | Some(x) => use(x)
    | None => None
    };
  let consume = (value, ~f as use) =>
    switch value {
    | Some(x) => use(x)
    | None => ()
    };
  let force = (value, ~f as use) =>
    switch value {
    | Some(x) => use(x)
    | None => failwith("Tried to unwrap an empty optional")
    };
  let join2 = (one, two) =>
    switch one {
    | None => None
    | Some(one) =>
      switch two {
      | None => None
      | Some(two) => Some((one, two))
      }
    };
  let first = (one, two) =>
    switch one {
    | Some(one) => Some(Left(one))
    | None =>
      switch two {
      | Some(two) => Some(Right(two))
      | None => None
      }
    };
};

module O: MonadThing = Option;

module Result = {
  open RResult;
  let return = (x) => Ok(x);
  let map /*: t 'a 'b => f::('a => 'c) => t 'c 'b*/ = (value, ~f as use) =>
    switch value {
    | Ok(x) => Ok(use(x))
    | Error(e) => Error(e)
    };

  let bind: (result('a, 'b), ~f: 'a => result('c, 'b)) => result('c, 'b) =
    (value, ~f as use) =>
      switch value {
      | Ok(x) => use(x)
      | Error(e) => Error(e)
      };
  let consume: (result('a, 'b), ~f: 'a => unit) => unit =
    (value, ~f as use) =>
      switch value {
      | Ok(x) => use(x)
      | Error(_) => ()
      };
  let force: (result('a, 'b), ~f: 'a => 'c) => 'c =
    (value, ~f as use) =>
      switch value {
      | Ok(x) => use(x)
      | Error(error) => failwith(error)
      };
  let join2 = (one, two) =>
    switch one {
    | Error(e) => Error(e)
    | Ok(v1) =>
      switch two {
      | Error(e) => Error(e)
      | Ok(v2) => Ok((v1, v2))
      }
    };
  let first = (one, two) =>
    switch one {
    | Ok(x) => Ok(Left(x))
    | Error(_e) =>
      switch two {
      | Ok(x) => Ok(Right(x))
      | Error(e) => Error(e) /* maybe have the error include both? */
      }
    };
  let unwrap = (value, map, other_return) =>
    switch value {
    | Ok(x) => map(x, ~f=return)
    | Error(err) => other_return(Error(err))
    };
};
