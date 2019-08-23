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

/*-- Helpers -------------------------------------*/

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

let arraySet = Js.Array.unsafe_set;
let arrayGet = Js.Array.unsafe_get;

let arrayRemove = (arr, index) =>
  Js.Array.removeCountInPlace(~pos=index, ~count=1, arr) |> ignore;

[@bs.new] external makeMap: unit => mapT('v) = "Map";
[@bs.send] external mapGet: (mapT('v), int) => Js.Undefined.t('v) = "get";
[@bs.send] external mapSet: (mapT('v), int, 'v) => unit = "set";
[@bs.send] external mapRem: (mapT('v), int) => unit = "delete";
[@bs.send]
external mapIter: (mapT('v), ('v, int) => unit) => unit = "forEach";

/*-- Main methods -------------------------------------*/

let make = (): t('v) => makeMap();

let getUndefined = (map: t('v), k: k): Js.Undefined.t('v) => {
  let item = mapGet(map, hash(k));

  if (Js.Undefined.testAny(item)) {
    Js.Undefined.empty;
  } else {
    switch (Js.Undefined.getUnsafe(item)) {
    | Collision(bucket) =>
      switch (Js.Array.find(({key}) => key === k, bucket)) {
      | Some({value}) => Js.Undefined.return(value)
      | None => Js.Undefined.empty
      }

    | Raw({key, value}) when key === k => Js.Undefined.return(value)
    | Raw(_) => Js.Undefined.empty

    | Chain({key, value}) when key === k => Js.Undefined.return(value)
    | Chain(_) => Js.Undefined.empty
    };
  };
};

let get = (map, k) => Js.Undefined.toOption(getUndefined(map, k));

let setOptimistic = (map: t('v), k: k, v: 'v, id: int) => {
  let code = hash(k);
  let optimistic = id !== 0;
  let item = mapGet(map, code);

  let newItem =
    if (Js.Undefined.testAny(item)) {
      optimistic
        ? Chain({key: k, id, value: v, prev: None}) : Raw({key: k, value: v});
    } else {
      let item = Js.Undefined.getUnsafe(item);

      switch (item) {
      | Collision(bucket) =>
        let index = Js.Array.findIndex(({key}) => key === k, bucket);
        if (index > (-1)) {
          let prev = arrayGet(bucket, index);
          let box = {
            key: k,
            value: v,
            id,
            prev: optimistic ? Some(prev) : None,
          };
          arraySet(bucket, index, box);
        } else {
          let box = {key: k, value: v, id, prev: None};
          Js.Array.push(box, bucket) |> ignore;
        };

        item;

      | Chain({key}) when key === k && !optimistic => Raw({key, value: v})

      | Chain({key} as prev) when key === k =>
        let box = {key, value: v, id, prev: Some(prev)};
        Chain(box);

      | Raw({key} as record) when key === k && !optimistic =>
        record.value = v;
        item;

      | Raw({key, value}) when key === k =>
        let prev = {key, value, id: 0, prev: None};
        let box = {key, value: v, id, prev: Some(prev)};
        Chain(box);

      | Chain(other) =>
        let box = {key: k, value: v, id, prev: None};
        Collision([|box, other|]);

      | Raw({key, value}) =>
        let box = {key: k, value: v, id, prev: None};
        let other = {key, value, id: 0, prev: None};
        Collision([|box, other|]);
      };
    };

  mapSet(map, code, newItem);
  map;
};

let set = (map, k, v) => setOptimistic(map, k, v, 0);

let remove = (map: t('v), k: k): t('v) => {
  let code = hash(k);
  let item = mapGet(map, code);
  if (!Js.Undefined.testAny(item)) {
    switch (Js.Undefined.getUnsafe(item)) {
    | Collision(bucket) =>
      let index = Js.Array.findIndex(({key}) => key === k, bucket);
      if (index > (-1)) {
        arrayRemove(bucket, index);
      };
    | Chain({key}) when key === k => mapRem(map, code)
    | Raw({key}) when key === k => mapRem(map, code)
    | Chain(_)
    | Raw(_) => ()
    };
  };

  map;
};

let clearBoxOptimistic = (box: boxT('a), optid: int) => {
  let rec filter = (x: option(boxT('a))) =>
    switch (x) {
    | Some(b) when b.id !== optid =>
      b.prev = filter(b.prev);
      x;
    | Some({prev}) => filter(prev)
    | None => None
    };
  filter(Some(box));
};

let clearOptimistic = (map: t('v), optid: int): t('v) => {
  mapIter(map, (node, code) =>
    switch (node) {
    | Collision(bucket) =>
      let bucket =
        Js.Array.reduce(
          (acc, box) =>
            if (box.id !== 0) {
              switch (clearBoxOptimistic(box, optid)) {
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
      | [||] => mapRem(map, code)
      | [|{key, value, id: 0}|] => mapSet(map, code, Raw({key, value}))
      | [|box|] => mapSet(map, code, Chain(box))
      | _ => mapSet(map, code, Collision(bucket))
      };

    | Chain(box) when box.id !== 0 =>
      switch (clearBoxOptimistic(box, optid)) {
      | None => mapRem(map, code)
      | Some(box) => mapSet(map, code, Chain(box))
      }

    | Chain(_)
    | Raw(_) => ()
    }
  );

  map;
};
