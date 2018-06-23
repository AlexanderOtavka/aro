#!/bin/sh

std_path="target/arostd"
mkdir -p $std_path

aro_file="examples/$1.aro"
wat_file="target/$1.wat"
wasm_file="target/$1.wasm"

cargo run -- -t $aro_file > $wat_file &&
wat2wasm $wat_file -o $wasm_file &&
wasm-interp --run-all-exports --host-print $2 $wasm_file
