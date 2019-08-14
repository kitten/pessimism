type k = string;

type boxT('v) = {
  key: k,
  value: 'v,
  id: int,
  prev: option(boxT('v)),
};

type t('v) =
  | Index(int, array(t('v)))
  | Collision(array(boxT('v)), int)
  | Leaf(boxT('v), int)
  | Empty;

let make: unit => t('v);
let get: (t('v), k) => option('v);
let remove: (t('v), k) => t('v);
let set: (t('v), k, 'v) => t('v);
let setOptimistic: (t('v), k, 'v, int) => t('v);
let clearOptimistic: (t('v), int) => t('v);
