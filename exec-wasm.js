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

/**
 * @param {number} n
 */
const toUnsigned = n => (n < 0 ? n + U32_MAX + 1 : n)

/**
 * @param {ArrayBuffer} buffer
 * @param {number} destinationPointer
 * @param {number} sourcePointer
 * @param {number} length
 */
const memCopy = (buffer, destinationPointer, sourcePointer, length) => {
  const destDataView = new DataView(buffer, destinationPointer, length)
  const sourceDataView = new DataView(buffer, sourcePointer, length)
  for (let i = 0; i < length; i++) {
    destDataView.setUint8(i, sourceDataView.getUint8(i))
  }
}

const host = {
  memory: new WebAssembly.Memory({ initial: 3 }),
  print: console.log,

  /**
   * @param {number} size
   */
  heap_alloc: size => {
    const bufferPointer = heapFreePointer
    heapFreePointer += roundUp(size, UNIVERSAL_ALIGN)
    return bufferPointer
  }
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
    list = toUnsigned(list)
    if (list === NULL) {
      throw new Error(
        "As usual, you can't get head.\nEspecially not from an empty list."
      )
    } else {
      // Create a container for the i64
      const resultHolderSize = UNIVERSAL_ALIGN
      const resultHolderPointer = host.heap_alloc(resultHolderSize) // TODO: stack alloc
      memCopy(host.memory.buffer, resultHolderPointer, list, resultHolderSize)
      console.log(
        "head:",
        new Uint32Array(host.memory.buffer, resultHolderPointer, 2)
      )
      return resultHolderPointer
    }
  },
  list_tail: list => {
    list = toUnsigned(list)
    if (list === NULL) {
      throw new Error(
        "You spent your whole life chasing your own tail.\n" +
          "You can't even find the tail of a list.\n" +
          "The list is empty, dumbass."
      )
    } else {
      const listDataView = new DataView(host.memory.buffer, list, LIST_SIZE)
      const result = listDataView.getUint32(UNIVERSAL_ALIGN, IS_LITTLE_ENDIAN)
      console.log("tail:", result)
      return result
    }
  },
  list_is_empty: list => {
    list = toUnsigned(list)
    console.log("is_empty:", list === NULL)
    return list === NULL
  },
  list_push: (list, newElement) => {
    list = toUnsigned(list)
    const newNodeSize = UNIVERSAL_ALIGN * 2
    const newNodePointer = host.heap_alloc(newNodeSize)
    const newNodeDataView = new DataView(
      host.memory.buffer,
      newNodePointer,
      newNodeSize
    )
    memCopy(host.memory.buffer, newNodePointer, newElement, UNIVERSAL_ALIGN)
    newNodeDataView.setUint32(UNIVERSAL_ALIGN, list, IS_LITTLE_ENDIAN)

    console.log("push:", new Uint32Array(host.memory.buffer, newNodePointer, 4))

    return newNodePointer
  }
}

WebAssembly.compile(fs.readFileSync(process.argv[2]))
  .then(wAsmModule => new WebAssembly.Instance(wAsmModule, { std, host }))
  .then(instance => {
    // console.log(instance.exports.main())
    const listAddr = instance.exports.main()
    console.log(
      "list node 0:",
      new Uint32Array(host.memory.buffer, listAddr, 4)
    )
  })
  .catch(err => {
    console.log("globals:", new Uint32Array(host.memory.buffer, 8, 32))
    console.log("stack:", new Uint32Array(host.memory.buffer, 0x10000, 32))
    console.log("heap:", new Uint32Array(host.memory.buffer, 0x20000, 32))

    throw err
  })
