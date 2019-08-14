type k = string;

type boxT('v) =
  | Permanent('v)
  | Optimistic('v, int, option(boxT('v)));

type t('v) =
  | Index(int, array(t('v)))
  | Value(k, 'v, int)
  | ValueChain(k, boxT('v), int)
  | Collision(list((k, boxT('v))), int);

/*-- Helpers -------------------------------------*/

let size_of_mask = 5;
let size_of_bucket = 32;
let bitmap_of_mask = size_of_bucket - 1;

let mask = (x: int, pos: int) =>
  x lsr (pos * size_of_mask) land bitmap_of_mask;

let smi = (x: int) => x lsr 1 land 0x40000000 lor (x land 0xbfffffff);

let djb2 = (x: string) => {
  let length = String.length(x);
  let rec explode = (h, i) =>
    if (i < length) {
      let h2 = h lsl 5 + h + int_of_char(String.unsafe_get(x, i));
      explode(h2, i + 1);
    } else {
      h;
    };
  explode(5381, 0);
};

let hammingWeight = (x: int) => {
  let x = x - x lsl 1 land 0x55555555;
  let x = x land 0x33333333 + x lsl 2 land 0x33333333;
  let x = (x + x lsl 4) land 0x0f0f0f0f;
  let x = x + x lsl 8;
  let x = x + x lsl 16;
  x land 0x7f;
};

let indexBit = (x: int, pos: int) => hammingWeight(x land (pos - 1));

/*-- Main methods -------------------------------------*/

let get = (map: t('v), k: k): option('v) => {
  let code = smi(djb2(k));

  let rec traverse = (node: t('a), depth: int) =>
    switch (node) {
    | Index(bitmap, contents) =>
      let pos = 1 lsl mask(code, depth);
      let has = bitmap land pos;
      if (has !== 0) {
        let index = indexBit(bitmap, pos);
        let child = Array.unsafe_get(contents, index);
        traverse(child, depth + 1);
      } else {
        None;
      };

    | Collision(bucket, _) =>
      List.fold_left(
        (res, (key, box)) =>
          if (key === k) {
            let Permanent(value) | Optimistic(value, _, _) = box;
            Some(value);
          } else {
            res;
          },
        None,
        bucket,
      )

    | Value(key, value, _) when key === k => Some(value)
    | ValueChain(key, box, _) when key === k =>
      let Permanent(value) | Optimistic(value, _, _) = box;
      Some(value);

    | Value(_)
    | ValueChain(_) => None
    };

  traverse(map, 0);
};

let set = (map: t('v), k: k, v: 'v): t('v) => {
  let code = smi(djb2(k));

  let rec nest = (code_a, code_b, a, b, depth) => {
    let pos_a = 1 lsl mask(code_a, depth);
    let pos_b = 1 lsl mask(code_b, depth);
    let bitmap = pos_a lor pos_b;
    let contents =
      if (pos_a === pos_b) {
        [|nest(code_a, code_b, a, b, depth + 1)|];
      } else {
        indexBit(bitmap, pos_a) !== 0 ? [|b, a|] : [|a, b|];
      };

    Index(bitmap, contents);
  };

  let rec traverse = (node, depth) =>
    switch (node) {
    | Index(bitmap, contents) =>
      let pos = 1 lsl mask(code, depth);
      let has = bitmap land pos;
      let bitmap = bitmap lor pos;
      let index = indexBit(bitmap, pos);

      let contents = Array.copy(contents);
      let contents =
        if (has !== 0) {
          let node = traverse(Array.unsafe_get(contents, index), depth + 1);
          Array.unsafe_set(contents, index, node);
          contents;
        } else {
          let node = Value(k, v, code);
          Js.Array.spliceInPlace(
            ~pos=index,
            ~remove=0,
            ~add=[|node|],
            contents,
          );
        };

      Index(bitmap, contents);

    | Value(key, _, _)
    | ValueChain(key, _, _) when key === k => Value(key, v, code)

    | Value(key, value, c) when c === code =>
      Collision([(k, Permanent(v)), (key, Permanent(value))], code)
    | ValueChain(key, box, c) when c === code =>
      Collision([(k, Permanent(v)), (key, box)], code)

    | Collision(bucket, c) when c === code =>
      let bucket = List.filter(((key, _)) => key !== k, bucket);
      Collision([(k, Permanent(v)), ...bucket], code);

    | Value(_, _, c) as n
    | ValueChain(_, _, c) as n
    | Collision(_, c) as n => nest(c, code, n, Value(k, v, code), depth)
    };

  traverse(map, 0);
};
