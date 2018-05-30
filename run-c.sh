#!/bin/sh

aro_file="examples/$1.aro"
c_file="target/$1.c"
exe_file="target/$1"

cargo run -- -c $aro_file | clang-format > $c_file &&
clang -o $exe_file $c_file &&
$exe_file
