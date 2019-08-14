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
