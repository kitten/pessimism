<h2 align="center">pessimism</h2>
<p align="center">
<strong>A fast and compact KV-Cache with optimistic entries</strong>
</p>

`pessimism` is a fast and compact KV-Cache primarily built for `@urql/exchange-graphcache`.
It's a minimal `Map` hashmap structure, which supports optimistic entries, which can be invalidated in a single go.

## Usage

This library works with both TypeScript and BuckleScript (Reason/OCaml).

```sh
yarn add pessimism
# or
npm install --save pessimism
```

The basic methods support making a map and setting, getting, and removing entries:

```js
import * as Map from 'pessimism';

let map = Map.set(Map.make(), "key", "value");
Map.get(map, "key"); // "value"

map = Map.remove(map, "key");
Map.get(map, "key"); // undefined
```

Optimistic entries can be set using `setOptimistic` and cleared using `clearOptimistic`:

```js
import * as Map from 'pessimism';

let map = Map.set(Map.make(), "key", "value");
// Set an optimistic entry with the ID 1
map = Map.setOptimistic("key", "temp", 1);

Map.get(map, "key"); // "temp" which is the optimistic value

// Clear all optimistic entries with ID 1
map = Map.clearOptimistic(map, 1);
Map.get(map, "key"); // "value" which was the original value
```
