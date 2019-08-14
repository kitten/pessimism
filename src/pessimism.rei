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

let get: (t('v), string) => option('v);
let set: (t('v), string, 'v) => t('v);
let setOptimistic: (t('v), string, 'v, int) => t('v);
let clearOptimistic: (t('v), int) => t('v);
