const fs = require('node:fs');
const filename = process.argv[2];

const wasmBuffer = fs.readFileSync(filename);
WebAssembly.instantiate(wasmBuffer, {
  env: {
    print_str: console.log
  }
}).then(({ instance }) => {
  const decoder = new TextDecoder('utf-8');

  const { main, memory, concatenate_strings, test } = instance.exports;
  console.log(test());

  // const arr = new Uint8Array(memory.buffer);
  // let addr = concatenate_strings(1, 44);
  // let len = arr[addr];
  // console.log(decoder.decode(arr.slice(addr + 1, len)));
});
