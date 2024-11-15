#!/usr/bin/env sh

export TEST262_DIR=./test262/
export JS_INTERPRETER=./target/debug/jswasm

ruby ./test262_runner.rb
