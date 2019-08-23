open Jest;
open Pessimism;

let it = test;

it("works for values on a single Index node", () => {
  open Expect;
  open! Expect.Operators;
  let words = [|"hello", "world", "test"|];
  let map =
    make()
    ->set(words[0], words[0])
    ->set(words[1], words[1])
    ->set(words[2], words[2]);
  let words_out = Array.map(x => map->get(x), words);
  expect(words_out) == Array.map(x => Some(x), words);
});

it("works for values that are colliding", () => {
  open Expect;
  open! Expect.Operators;
  let words = [|"hetairas", "mentioner", "heliotropes", "neurospora"|];
  let map =
    make()
    ->set(words[0], words[0])
    ->set(words[1], words[1])
    ->set(words[2], words[2])
    ->set(words[3], words[3]);
  let words_out = Array.map(x => map->get(x), words);
  expect(words_out) == Array.map(x => Some(x), words);
});

it("works for transitive changes", () => {
  open Expect;
  open! Expect.Operators;
  let words = [|
    "hello",
    "world",
    "test",
    "hetairas",
    "mentioner",
    "heliotropes",
    "neurospora",
  |];
  let map = ref(make()->asMutable);
  Array.iter(word => map := set(map^, word, word), words);
  let map = asImmutable(map^);
  let words_out = Array.map(x => map->get(x), words);
  expect(words_out) == Array.map(x => Some(x), words);
});

it("deletes values correctly", () => {
  open Expect;
  open! Expect.Operators;
  let words = [|"hello", "world", "test"|];
  let map =
    make()
    ->set(words[0], words[0])
    ->set(words[1], words[1])
    ->set(words[2], words[2])
    ->remove(words[1]);
  let a = map->get(words[0]);
  let b = map->get(words[1]);
  expect([|a, b|]) == [|Some(words[0]), None|];
});

it("sets optimistic values and clears them", () => {
  open Expect;
  open! Expect.Operators;
  let map =
    make()->set("key", "permanent")->setOptimistic("key", "temporary", 1);
  let before = map->get("key");
  let map = map->setOptimistic("key", "temporary2", 2);
  let middle = map->get("key");
  let map = map->clearOptimistic(1);
  let after = map->get("key");
  let map = map->clearOptimistic(2);
  let after2 = map->get("key");
  expect([|before, middle, after, after2|])
  == [|
       Some("temporary"),
       Some("temporary2"),
       Some("temporary2"),
       Some("permanent"),
     |];
});

it("sets optimistic values and overrides them if needed", () => {
  open Expect;
  open! Expect.Operators;
  let map =
    make()->set("key", "permanent")->setOptimistic("key", "temporary", 1);
  let before = map->get("key");
  let map = map->set("key", "permanent2");
  let after = map->get("key");
  expect([|before, after|]) == [|Some("temporary"), Some("permanent2")|];
});

it("supports setting and retrieving undefined", () => {
  open Expect;
  open! Expect.Operators;
  let map =
    make()
    ->set("key", Js.Undefined.return("permanent"))
    ->setOptimistic("key", Js.Undefined.empty, 1);
  let before = map->getUndefined("key");
  let map = map->clearOptimistic(1);
  let after = map->getUndefined("key");
  expect([|before, after|]) == [%raw "[undefined, 'permanent']"];
});
