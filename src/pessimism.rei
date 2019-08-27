type keyT = string;
type hashT = int;
type ownerT = ref(unit);

type valueT('v) = {
  key: keyT,
  value: 'v,
  id: int,
  mutable prev: option(valueT('v)),
};

type nodeT('v) =
  | Index(t('v))
  | Value(keyT, 'v, hashT)
  | Values(valueT('v), hashT)
  | Collision(array(valueT('v)), hashT)
  | Empty(hashT)
and t('v) = {
  mutable bitmap: int,
  mutable contents: array(nodeT('v)),
  mutable owner: ownerT,
};

let make: unit => t('v);
let asMutable: t('v) => t('v);
let asImmutable: t('v) => t('v);
let get: (t('v), keyT) => option('v);
let getUndefined: (t('v), keyT) => Js.Undefined.t('v);
let remove: (t('v), keyT) => t('v);
let set: (t('v), keyT, 'v) => t('v);
let setOptimistic: (t('v), keyT, 'v, int) => t('v);
let clearOptimistic: (t('v), int) => t('v);
