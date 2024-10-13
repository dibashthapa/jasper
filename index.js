const fs = require('node:fs');
const filename = process.argv[2];

const wasmBuffer = fs.readFileSync(filename);
WebAssembly.instantiate(wasmBuffer).then(({ instance }) => {
  var memory = instance.exports.mem;

  const { main, test } = instance.exports;
  var bytes = new Uint8Array(memory.buffer, 5, 4);
  var string = new TextDecoder('utf8').decode(bytes);
  console.log(string);
});
