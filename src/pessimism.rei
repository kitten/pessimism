type k = string;

type boxT('v) = {
  key: k,
  value: 'v,
  id: int,
  prev: option(boxT('v)),
};

type t('v) =
  | Index(int, array(t('v)))
  | Value(k, 'v, int)
  | ValueChain(k, boxT('v), int)
  | Collision(array(boxT('v)), int);

let get: (t('v), string) => option('v);
let set: (t('v), string, 'v) => t('v);
