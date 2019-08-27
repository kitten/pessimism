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

/*-- Helpers -------------------------------------*/

let owner = () => ref();
let anon = owner();
let isOwner = (a: ownerT, b: ownerT) => a !== anon && a === b;

let mask = (x: int, pos: int) => 1 lsl (x lsr (pos * 5) land 31);

let smi = (x: int) => x lsr 1 land 0x40000000 lor (x land 0xbfffffff);

let hash = (x: string): hashT => {
  let until = String.length(x) - 1;
  let h = ref(5381);
  for (i in 0 to until) {
    h := h^ lsl 5 + h^ + int_of_char(String.unsafe_get(x, i));
  };
  smi(h^);
};

let hammingWeight = (input: int) => {
  let x = ref(input);
  x := x^ - x^ asr 1 land 0x55555555;
  x := x^ land 0x33333333 + x^ asr 2 land 0x33333333;
  x := (x^ + x^ asr 4) land 0x0f0f0f0f;
  x := x^ + x^ asr 8;
  x := x^ + x^ asr 16;
  x^ land 0x7f;
};

let indexBit = (x: int, pos: int) => hammingWeight(x land (pos - 1));

/*-- Array helpers -------------------------------------*/

[@bs.get_index] external arrayGet: (array('a), int) => 'a = "";
[@bs.set_index] external arraySet: (array('a), int, 'a) => unit = "";
[@bs.send] external arrayCopy: array('a) => array('a) = "slice";
[@bs.send]
external arrayRemove: (array('a), int, [@bs.as 1] _) => unit = "splice";
[@bs.send]
external arrayAdd: (array('a), int, [@bs.as 0] _, 'a) => unit = "splice";
[@bs.send] external arrayPush: (array('a), 'a) => unit = "push";
[@bs.get] external arraySize: array('a) => int = "length";

/*-- Index helpers -------------------------------------*/

let copyIndex = (index: t('v), owner: ownerT) =>
  !isOwner(owner, index.owner)
    ? {...index, contents: arrayCopy(index.contents), owner} : index;

let traverseCopy = (map, code, owner) => {
  let rec traverse = (index, depth) => {
    let {bitmap, contents} = index;
    let pos = mask(code, depth);
    if (bitmap lor pos !== bitmap) {
      (depth, index);
    } else {
      let i = indexBit(bitmap, pos);
      let child = arrayGet(contents, i);
      switch (child) {
      | Index(childIndex) =>
        let newChildIndex = copyIndex(childIndex, owner);
        arraySet(contents, i, Index(newChildIndex));
        traverse(newChildIndex, depth + 1);
      | _ => (depth, index)
      };
    };
  };

  traverse(map, 0);
};

let rec resolveConflict = (codeA, codeB, nodeA, nodeB, depth, owner) => {
  let posA = mask(codeA, depth);
  let posB = mask(codeB, depth);
  let bitmap = posA lor posB;
  let contents =
    posA === posB
      ? [|resolveConflict(codeA, codeB, nodeA, nodeB, depth + 1, owner)|]
      : indexBit(bitmap, posA) !== 0 ? [|nodeB, nodeA|] : [|nodeA, nodeB|];
  Index({bitmap, contents, owner});
};

let removeFromIndex = (index: t('v), pos: int, owner: ownerT) => {
  let {bitmap} = index;
  let newBitmap = bitmap lxor pos;
  if (newBitmap !== bitmap) {
    let index = copyIndex(index, owner);
    arrayRemove(index.contents, indexBit(bitmap, pos));
    index.bitmap = newBitmap;
    index;
  } else {
    index;
  };
};

let rec clearBox = (box: valueT('a), optid: int) =>
  switch (box, box.prev) {
  | ({id}, Some(prev)) when id === optid => clearBox(prev, optid)
  | ({id}, None) when id === optid => None
  | (_, Some(prev)) => Some({...box, prev: clearBox(prev, optid)})
  | _ => Some(box)
  };

let clearOptimisticNode = (node: nodeT('v), optid: int) =>
  switch (node) {
  | Empty(_)
  | Index(_)
  | Value(_) => node

  | Values(box, _) when box.id === 0 => node

  | Values(box, code) =>
    switch (clearBox(box, optid)) {
    | Some(box) => Values(box, code)
    | None => Empty(code)
    }

  | Collision(bucket, code) =>
    let bucket =
      Js.Array.reduce(
        (bucket, box) =>
          if (box.id === 0) {
            arrayPush(bucket, box);
            bucket;
          } else {
            switch (clearBox(box, optid)) {
            | Some(box) =>
              arrayPush(bucket, box);
              bucket;
            | None => bucket
            };
          },
        [||],
        bucket,
      );

    arraySize(bucket) > 0 ? Collision(bucket, code) : Empty(code);
  };

let addToBucket = (bucket: array(valueT('v)), box: valueT('v)) => {
  let bucket = arrayCopy(bucket);
  let optimistic = box.id !== 0;
  let i = Js.Array.findIndex(x => x.key === box.key, bucket);
  if (i > (-1) && optimistic) {
    let prev = arrayGet(bucket, i);
    if (optimistic) {
      box.prev = Some(prev);
    };
    arraySet(bucket, i, box);
  } else {
    arrayPush(bucket, box);
  };

  bucket;
};

let rec rebuildWithStack = (stack, depth, innerIndex, code, owner) => {
  let pos = mask(code, depth);
  switch (stack) {
  | [index, ...rest] when innerIndex.bitmap === 0 =>
    let index = removeFromIndex(index, pos, owner);
    rebuildWithStack(rest, depth - 1, index, code, owner);
  | [index, ...rest] =>
    let index = copyIndex(index, owner);
    arraySet(
      index.contents,
      indexBit(index.bitmap, pos),
      Index(innerIndex),
    );
    rebuildWithStack(rest, depth - 1, index, code, owner);
  | [] => innerIndex
  };
};

/*-- Main methods -------------------------------------*/

let make = () => {bitmap: 0, contents: [||], owner: anon};

let asMutable = (index: t('v)) =>
  index.owner === anon ? {...index, owner: owner()} : index;

let asImmutable = (index: t('v)) => {
  index.owner = anon;
  index;
};

let getUndefined = (map: t('v), key: keyT): Js.Undefined.t('v) => {
  let code = hash(key);

  let rec traverse = (index, depth) => {
    let {bitmap, contents} = index;
    let pos = mask(code, depth);
    if (bitmap lor pos !== bitmap) {
      Js.Undefined.empty;
    } else {
      let child = arrayGet(contents, indexBit(bitmap, pos));
      switch (child) {
      | Index(index) => traverse(index, depth + 1)

      | Collision(bucket, _) =>
        switch (Js.Array.find(box => box.key === key, bucket)) {
        | Some({value}) => Js.Undefined.return(value)
        | None => Js.Undefined.empty
        }

      | Value(k, v, _) when k === key => Js.Undefined.return(v)
      | Values(box, _) when box.key === key => Js.Undefined.return(box.value)

      | Value(_)
      | Values(_)
      | Empty(_) => Js.Undefined.empty
      };
    };
  };

  traverse(map, 0);
};

let get = (map, k) => Js.Undefined.toOption(getUndefined(map, k));

let setOptimistic = (map: t('v), key: keyT, value: 'v, id: int): t('v) => {
  let {owner} = map;
  let map = copyIndex(map, owner);

  let code = hash(key);
  let (depth, index) = traverseCopy(map, code, owner);

  let optimistic = id !== 0;
  let pos = mask(code, depth);
  let newBitmap = index.bitmap lor pos;
  let i = indexBit(newBitmap, pos);
  if (newBitmap !== index.bitmap) {
    arrayAdd(index.contents, i, Value(key, value, code));
    index.bitmap = newBitmap;
  } else {
    let node =
      switch (arrayGet(index.contents, i)) {
      | Value(k, v, _) when k === key && optimistic =>
        let prev = {key: k, value: v, id: 0, prev: None};
        let next = {key, value, id, prev: Some(prev)};
        Values(next, code);

      | Values(box, _) when box.key === key && optimistic =>
        Values({key, value, id, prev: Some(box)}, code)

      | Value(k, _, _) when k === key => Value(key, value, code)
      | Values(box, _) when box.key === key =>
        Values({key, value, id: 0, prev: None}, code)

      | Value(k, v, c) when c === code =>
        let prev = {key: k, value: v, id: 0, prev: None};
        let next = {key, value, id: 0, prev: None};
        Collision([|prev, next|], code);

      | Values(prev, c) when c === code =>
        let next = {key, value, id: 0, prev: None};
        Collision([|prev, next|], code);

      | Collision(bucket, c) when c === code =>
        let box = {key, value, id, prev: None};
        Collision(addToBucket(bucket, box), code);

      | Empty(_) when optimistic =>
        Values({key, value, id, prev: None}, code)
      | Empty(_) => Value(key, value, code)

      | Value(_, _, prevCode) as prev
      | Values(_, prevCode) as prev
      | Collision(_, prevCode) as prev =>
        let next =
          optimistic
            ? Values({key, value, id, prev: None}, code)
            : Value(key, value, code);
        resolveConflict(prevCode, code, prev, next, depth + 1, owner);

      | Index(_) as prev => prev /* this should never happen */
      };

    arraySet(index.contents, i, node);
  };

  map;
};

let set = (map, k, v) => setOptimistic(map, k, v, 0);

let remove = (map: t('v), key: keyT) => {
  let {owner} = map;
  let code = hash(key);

  let rec traverse = (stack, index, depth) => {
    let {bitmap, contents} = index;
    let pos = mask(code, depth);
    if (bitmap lor pos !== bitmap) {
      map;
    } else {
      let i = indexBit(bitmap, pos);
      let child = arrayGet(contents, i);
      switch (child) {
      | Index(index) => traverse([index, ...stack], index, depth + 1)

      | Value(k, _, _)
      | Values({key: k}, _) when k === key =>
        let index = removeFromIndex(index, pos, owner);
        rebuildWithStack(stack, depth - 1, index, code, owner);

      | Collision(bucket, c) when c === code =>
        let bucket = Js.Array.filter(x => x.key !== key, bucket);
        if (arraySize(bucket) === 0) {
          let index = removeFromIndex(index, pos, owner);
          rebuildWithStack(stack, depth - 1, index, code, owner);
        } else {
          let index = copyIndex(index, owner);
          arraySet(index.contents, i, Collision(bucket, code));
          rebuildWithStack(stack, depth - 1, index, code, owner);
        };

      | _ => map
      };
    };
  };

  traverse([], map, 0);
};

let rec clearOptimistic = (map: t('v), optid: int): t('v) => {
  let {owner} = map;
  let index = copyIndex(map, owner);

  for (x in 31 downto 0) {
    let pos = 1 lsl x;
    if (pos land index.bitmap !== 0) {
      let i = indexBit(index.bitmap, pos);
      switch (clearOptimisticNode(arrayGet(index.contents, i), optid)) {
      | Index(index) =>
        let innerIndex = clearOptimistic(index, optid);
        if (innerIndex.bitmap === 0) {
          arrayRemove(index.contents, i);
          index.bitmap = index.bitmap lxor pos;
        } else {
          arraySet(index.contents, i, Index(innerIndex));
        };

      | Empty(_) =>
        arrayRemove(index.contents, i);
        index.bitmap = index.bitmap lxor pos;
      | node => arraySet(index.contents, i, node)
      };
    };
  };

  index;
};
