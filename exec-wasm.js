#!/usr/bin/env node

process.on("unhandledRejection", err => {
  throw err
})

const fs = require("fs")

const UNIVERSAL_ALIGN = 8
const PAGE_SIZE = 0x10000
const LIST_SIZE = UNIVERSAL_ALIGN * 2
const IS_LITTLE_ENDIAN = true // WAsm is always little endian
const U32_MAX = 0xffffffff
const I32_MAX = 0x7fffffff
const I32_MIN = -0x80000000
const NULL = U32_MAX

// First page is kept free for globals
// Second page is for the start of the stack
// Third page is for the start of the heap
let heapFreePointer = PAGE_SIZE * 2

/**
 * @param {number} n The number to round.
 * @param {number} multiple The multiple to round `n` to.
 */
const roundUp = (n, multiple) => {
  const mod = n % multiple
  if (mod === 0) {
    return n
  } else {
    return n + multiple - mod
  }
}

const host = {
  memory: new WebAssembly.Memory({ initial: 3 }),
  heap_alloc: size => {
    const bufferPointer = heapFreePointer
    heapFreePointer += roundUp(size, UNIVERSAL_ALIGN)
    return bufferPointer
  },
  print: console.log
}

const std = {
  math_floordiv: (a, b) => {
    // TODO: fix semantics and rename to int.div
    const quotient = Math.trunc(a / b)
    if (quotient > I32_MAX || quotient < I32_MIN) {
      throw new Error(
        "The result of the division is too big, just like your ego.\n"
      )
    }

    return quotient
  },
  list_head: list => {
    if (list === NULL) {
      throw new Error(
        "As usual, you can't get head.\nEspecially not from an empty list."
      )
    } else {
      const listDataView = new DataView(host.memory, list, LIST_SIZE)
      return listDataView.getBigUint64(0, IS_LITTLE_ENDIAN)
    }
  },
  list_tail: list => {
    if (list === NULL) {
      throw new Error(
        "You spent your whole life chasing your own tail.\n" +
          "You can't even find the tail of a list.\n" +
          "The list is empty, dumbass."
      )
    } else {
      const listDataView = new DataView(host.memory, list, LIST_SIZE)
      return listDataView.getUint32(UNIVERSAL_ALIGN, IS_LITTLE_ENDIAN)
    }
  },
  list_is_empty: list => {
    return list === NULL
  },
  list_push: (list, new_element) => {
    const newNodeSize = UNIVERSAL_ALIGN * 2
    const newNodePointer = host.heap_alloc(newNodeSize)
    const newNodeDataView = new DataView(
      host.memory,
      newNodePointer,
      newNodeSize
    )
    newNodeDataView.setBigUint64(0, new_element)
    newNodeDataView.setUint32(UNIVERSAL_ALIGN, list, IS_LITTLE_ENDIAN)

    return newNodePointer
  }
}

WebAssembly.compile(fs.readFileSync(process.argv[2]))
  .then(wAsmModule => new WebAssembly.Instance(wAsmModule, { std, host }))
  .then(instance => {
    console.log(instance.exports.main())
  })
  .catch(err => {
    console.log("globals:", new Uint32Array(host.memory.buffer, 8, 32))
    console.log("stack:", new Uint32Array(host.memory.buffer, 0x10000, 32))
    console.log("heap:", new Uint32Array(host.memory.buffer, 0x20000, 32))

    throw err
  })
