#!/usr/bin/env node

const fs = require("fs")

WebAssembly.compile(fs.readFileSync(process.argv[2]))
  .then(m => new WebAssembly.Instance(m, {}))
  .then(instance => {
    console.log(instance.exports.main())
  })
