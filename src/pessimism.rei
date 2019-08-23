type k = string;

type ownerT = ref(unit);

type boxT('v) = {
  key: k,
  id: int,
  value: 'v,
  prev: option(boxT('v)),
};

type t('v) =
  | Index({
      mutable bitmap: int,
      mutable contents: array(t('v)),
      owner: ownerT,
    })
  | Collision(array(boxT('v)), int)
  | Leaf(boxT('v), int)
  | RawLeaf(k, 'v, int)
  | Empty;

let make: unit => t('v);
let get: (t('v), k) => option('v);
let getUndefined: (t('v), k) => Js.Undefined.t('v);
let remove: (t('v), k) => t('v);
let set: (t('v), k, 'v) => t('v);
let setOptimistic: (t('v), k, 'v, int) => t('v);
let clearOptimistic: (t('v), int) => t('v);
