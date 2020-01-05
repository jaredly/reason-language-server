type t;

let none: t;

let parse: string => t;

let major: t => string;
let minor: t => string;
let patch: t => string;
let metadata: t => string;