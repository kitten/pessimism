type k = string;

type boxT('v) = {
  key: k,
  mutable id: int,
  mutable value: 'v,
  mutable prev: option(boxT('v)),
};

type valueT('v) =
  | Raw({
      key: k,
      mutable value: 'v,
    })
  | Chain(boxT('v))
  | Collision(array(boxT('v)));

type mapT('v);
type t('v) = mapT(valueT('v));

let make: unit => t('v);
let get: (t('v), k) => option('v);
let getUndefined: (t('v), k) => Js.Undefined.t('v);
let remove: (t('v), k) => t('v);
let set: (t('v), k, 'v) => t('v);
let setOptimistic: (t('v), k, 'v, int) => t('v);
let clearOptimistic: (t('v), int) => t('v);
