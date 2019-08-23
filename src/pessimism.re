type k = string;

type ownerT = ref(unit);

type boxT('v) = {
  key: k,
  id: int,
  value: 'v,
  prev: option(boxT('v)),
};

type nodeT('v) =
  | Index({
      mutable bitmap: int,
      mutable contents: array(nodeT('v)),
      owner: ownerT,
    })
  | Collision(array(boxT('v)), int)
  | Leaf(boxT('v), int)
  | RawLeaf(k, 'v, int)
  | Empty;

type t('v) = {
  mutable root: nodeT('v),
  mutable owner: ownerT,
};

/*-- Helpers -------------------------------------*/

let owner = () => ref();
let anon = owner();

let isOwner = (a: ownerT, b: ownerT) => a !== anon && a === b;

let mask = (x: int, pos: int) => 1 lsl (x lsr (pos * 5) land 31);

let smi = (x: int) => x lsr 1 land 0x40000000 lor (x land 0xbfffffff);

let hash = (x: string) => {
  let length = String.length(x);
  let rec explode = (h, i) =>
    if (i < length) {
      let h2 = h lsl 5 + h + int_of_char(String.unsafe_get(x, i));
      explode(h2, i + 1);
    } else {
      h;
    };
  smi(explode(5381, 0));
};

let hammingWeight = (x: int) => {
  let x = x - x asr 1 land 0x55555555;
  let x = x land 0x33333333 + x asr 2 land 0x33333333;
  let x = (x + x asr 4) land 0x0f0f0f0f;
  let x = x + x asr 8;
  let x = x + x asr 16;
  x land 0x7f;
};

let indexBit = (x: int, pos: int) => hammingWeight(x land (pos - 1));

let arraySet = Array.unsafe_set;

let arrayRemove = (arr, index) =>
  Js.Array.removeCountInPlace(~pos=index, ~count=1, arr) |> ignore;

let arrayAdd = (arr, index, value) =>
  Js.Array.spliceInPlace(~pos=index, ~remove=0, ~add=[|value|], arr)
  |> ignore;

/*-- Main methods -------------------------------------*/

let makeIndex = owner => Index({bitmap: 0, contents: [||], owner});

let update = (map, root) =>
  if (map.owner !== anon) {
    map.root = root;
    map;
  } else {
    {owner: map.owner, root};
  };

let make = () => {root: makeIndex(anon), owner: anon};

let asMutable = (map: t('v)) =>
  if (map.owner === anon) {
    {root: map.root, owner: owner()};
  } else {
    map;
  };

let asImmutable = (map: t('v)) => {
  map.owner = anon;
  map;
};

let getUndefined = (map: t('v), k: k): Js.Undefined.t('v) => {
  let code = hash(k);

  let rec traverse = (node, depth) =>
    switch (node) {
    | Index({bitmap, contents}) =>
      let pos = mask(code, depth);
      let has = bitmap land pos;
      if (has !== 0) {
        let index = indexBit(bitmap, pos);
        let child = Js.Array.unsafe_get(contents, index);
        traverse(child, depth + 1);
      } else {
        Js.Undefined.empty;
      };

    | Collision(bucket, _) =>
      switch (Js.Array.find(({key}) => key === k, bucket)) {
      | Some({value}) => Js.Undefined.return(value)
      | None => Js.Undefined.empty
      }

    | Leaf({key, value}, _)
    | RawLeaf(key, value, _) when key === k => Js.Undefined.return(value)

    | Empty
    | Leaf(_)
    | RawLeaf(_) => Js.Undefined.empty
    };

  traverse(map.root, 0);
};

let get = (map, k) => Js.Undefined.toOption(getUndefined(map, k));

let rec make_index = (code_a, code_b, a, b, depth, owner) => {
  let pos_a = mask(code_a, depth);
  let pos_b = mask(code_b, depth);
  let bitmap = pos_a lor pos_b;
  let contents =
    if (pos_a === pos_b) {
      [|make_index(code_a, code_b, a, b, depth + 1, owner)|];
    } else {
      indexBit(bitmap, pos_a) !== 0 ? [|b, a|] : [|a, b|];
    };

  Index({bitmap, contents, owner});
};

let setOptimistic = (map: t('v), k: k, v: 'v, id: int): t('v) => {
  let mapOwner = map.owner;
  let code = hash(k);
  let optimistic = id !== 0;
  let vbox = {key: k, value: v, id, prev: None};

  let rec traverse = (node, depth) =>
    switch (node) {
    | Index({bitmap, contents, owner} as record) =>
      let pos = mask(code, depth);
      let has = bitmap land pos;
      let bitmap = bitmap lor pos;
      let index = indexBit(bitmap, pos);
      let mutate = isOwner(mapOwner, owner);

      if (has !== 0) {
        let n = traverse(Js.Array.unsafe_get(contents, index), depth + 1);
        if (mutate) {
          arraySet(contents, index, n);
          record.bitmap = bitmap;
          node;
        } else {
          let contents = Js.Array.copy(contents);
          arraySet(contents, index, n);
          Index({bitmap, contents, owner: mapOwner});
        };
      } else {
        let n = optimistic ? Leaf(vbox, code) : RawLeaf(k, v, code);
        if (mutate) {
          arrayAdd(contents, index, n);
          record.bitmap = bitmap;
          node;
        } else {
          let contents = Js.Array.copy(contents);
          arrayAdd(contents, index, n);
          Index({bitmap, contents, owner: mapOwner});
        };
      };

    | Leaf({key} as box, _) when key === k && optimistic =>
      Leaf({...vbox, prev: Some(box)}, code)
    | RawLeaf(key, value, _) when key === k && optimistic =>
      let prev = {key, value, id: 0, prev: None};
      Leaf({...vbox, prev: Some(prev)}, code);

    | Leaf({key}, _) when key === k => Leaf(vbox, code)
    | RawLeaf(key, _, _) when key === k => RawLeaf(k, v, code)

    | Leaf(box, c) when c === code => Collision([|vbox, box|], code)
    | RawLeaf(key, value, c) when c === code =>
      Collision([|vbox, {key, value, id: 0, prev: None}|], code)

    | Collision(bucket, c) when c === code =>
      let index = Js.Array.findIndex(({key}) => key === k, bucket);
      if (index > (-1)) {
        let prev = Js.Array.unsafe_get(bucket, index);
        let bucket = Js.Array.copy(bucket);
        Js.Array.unsafe_set(
          bucket,
          index,
          optimistic ? {...vbox, prev: Some(prev)} : vbox,
        );
        Collision(bucket, code);
      } else {
        Collision(Js.Array.concat(bucket, [|vbox|]), code);
      };

    | Leaf(_, c)
    | RawLeaf(_, _, c)
    | Collision(_, c) =>
      let n = optimistic ? Leaf(vbox, code) : RawLeaf(k, v, code);
      make_index(c, code, node, n, depth, mapOwner);

    | Empty => RawLeaf(k, v, code)
    };

  update(map, traverse(map.root, 0));
};

let set = (map, k, v) => setOptimistic(map, k, v, 0);

let remove = (map: t('v), k: k): t('v) => {
  let mapOwner = map.owner;
  let code = hash(k);

  let rec traverse = (node, depth) =>
    switch (node) {
    | Index({bitmap, contents, owner} as record) =>
      let pos = mask(code, depth);
      let has = bitmap land pos;
      let index = indexBit(bitmap, pos);
      let mutate = isOwner(mapOwner, owner);

      if (has !== 0) {
        let n = traverse(Js.Array.unsafe_get(contents, index), depth + 1);
        if (n === Empty) {
          let bitmap = bitmap lxor pos;
          if (bitmap === 0) {
            depth === 0 ? makeIndex(mapOwner) : Empty;
          } else if (mutate) {
            arrayRemove(contents, index);
            record.bitmap = bitmap;
            node;
          } else {
            let contents = Js.Array.copy(contents);
            arrayRemove(contents, index);
            Index({bitmap, contents, owner: mapOwner});
          };
        } else if (mutate) {
          Js.Array.unsafe_set(contents, index, n);
          node;
        } else {
          let contents = Js.Array.copy(contents);
          Js.Array.unsafe_set(contents, index, n);
          Index({bitmap, contents, owner: mapOwner});
        };
      } else {
        node;
      };

    | RawLeaf(key, _, _) when key === k => Empty
    | Leaf({key}, _) when key === k => Empty

    | RawLeaf(_) => node
    | Leaf(_) => node

    | Collision(bucket, c) when c === code =>
      let bucket = Js.Array.filter(({key}) => key === k, bucket);
      switch (bucket) {
      | [||] => Empty
      | [|box|] => RawLeaf(box.key, box.value, code)
      | _ => Collision(bucket, code)
      };
    | Collision(_) => node

    | Empty => Empty
    };

  update(map, traverse(map.root, 0));
};

let clear_box = (box: boxT('a), optid: int) => {
  let rec filter = (x: option(boxT('a))) =>
    switch (x) {
    | Some({id, prev}) when id === optid => filter(prev)
    | Some(b) => Some({...b, prev: filter(b.prev)})
    | None => None
    };
  filter(Some(box));
};

let clearOptimistic = (map: t('v), optid: int): t('v) => {
  let mapOwner = map.owner;

  let rec traverse = (node, depth) =>
    switch (node) {
    | Leaf({id} as box, code) when id !== 0 =>
      switch (clear_box(box, optid)) {
      | Some({key, value, id: 0}) => RawLeaf(key, value, code)
      | Some(box) => Leaf(box, code)
      | None => Empty
      }

    | Index({bitmap, contents, owner} as record) =>
      let hasContent = ref(false);
      let contents =
        Js.Array.map(
          node => {
            let node = traverse(node, depth + 1);
            if (node !== Empty) {
              hasContent := true;
            };
            node;
          },
          contents,
        );

      if (hasContent^ && isOwner(mapOwner, owner)) {
        record.contents = contents;
        node;
      } else if (hasContent^) {
        Index({bitmap, contents, owner: mapOwner});
      } else {
        depth === 0 ? makeIndex(mapOwner) : Empty;
      };

    | Collision(bucket, code) =>
      let bucket =
        Js.Array.reduce(
          (acc, box) =>
            if (box.id !== 0) {
              switch (clear_box(box, optid)) {
              | Some(box) =>
                ignore(Js.Array.push(box, acc));
                acc;
              | None => acc
              };
            } else {
              ignore(Js.Array.push(box, acc));
              acc;
            },
          [||],
          bucket,
        );
      switch (bucket) {
      | [||] => Empty
      | [|{key, value, id: 0}|] => RawLeaf(key, value, code)
      | [|box|] => Leaf(box, code)
      | _ => Collision(bucket, code)
      };

    | Leaf(_)
    | RawLeaf(_) => node

    | Empty => Empty
    };

  update(map, traverse(map.root, 0));
};
