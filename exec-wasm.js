#!/usr/bin/env node

process.on("unhandledRejection", err => {
  throw err
})

const fs = require("fs")

const std = {
  math_floordiv: (a, b) => Math.floor(a / b),
  list_head: list => {
    console.log("list_head")
  },
  list_tail: list => {
    console.log("list_tail")
  },
  list_is_empty: list => {
    console.log("list_is_empty")
  },
  list_push: (list, new_element) => {
    console.log("list_push")
  }
}

const host = {
  memory: new WebAssembly.Memory({ initial: 3 }),
  print: console.log
}

WebAssembly.compile(fs.readFileSync(process.argv[2]))
  .then(wAsmModule => new WebAssembly.Instance(wAsmModule, { std, host }))
  .then(instance => {
    console.log(instance.exports.main())
  })
  .catch(err => {
    console.log("alloc pointers:", new Uint32Array(host.memory.buffer, 0, 2))
    console.log("globals:", new Uint32Array(host.memory.buffer, 8, 32))
    console.log("stack:", new Uint32Array(host.memory.buffer, 0x10000, 32))
    console.log("heap:", new Uint32Array(host.memory.buffer, 0x20000, 32))

    throw err
  })
