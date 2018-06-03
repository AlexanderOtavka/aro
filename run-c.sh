#!/bin/sh

std_path="target/arostd"
mkdir -p $std_path

aro_file="examples/$1.aro"
c_file="target/$1.c"
exe_file="target/$1"

cargo run -- -c --build-std $std_path $aro_file | clang-format > $c_file &&
clang -o $exe_file -I$std_path $c_file "$std_path/arostd.c" "src/aroext.c" &&
$exe_file
