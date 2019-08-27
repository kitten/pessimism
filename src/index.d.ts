export class Map<T> {}

export const make: <T>() => Map<T>;
export const asMutable: <T>(map: Map<T>) => Map<T>;
export const asImmutable: <T>(map: Map<T>) => Map<T>;
export const get: <T>(map: Map<T>, key: string) => undefined | T;
export const remove: <T>(map: Map<T>, key: string) => Map<T>;
export const set: <T>(map: Map<T>, key: string, value: T) => Map<T>;
export const setOptimistic: <T>(
  map: Map<T>,
  key: string,
  value: T,
  optimisticKey: number
) => Map<T>;
export const clearOptimistic: <T>(map: Map<T>, optimisticKey: number) => Map<T>;
