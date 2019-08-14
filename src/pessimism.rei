type k = string;

type boxT('v) =
  | Permanent('v)
  | Optimistic('v, int, option(boxT('v)));

type t('v) =
  | Index(int, array(t('v)))
  | Value(k, 'v, int)
  | ValueChain(k, boxT('v), int)
  | Collision(list((k, boxT('v))), int);

let get: (t('v), string) => option('v);
let set: (t('v), string, 'v) => t('v);
