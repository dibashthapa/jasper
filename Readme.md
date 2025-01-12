# Experimental JS-to-WASM Compiler

This is an **experimental compiler** for translating JavaScript to WebAssembly (WASM). The project is currently a work in progress and achieves less than 5% compatibility with the [Test262](https://github.com/tc39/test262) test suite at this stage.

## Compilation Approaches
There are three potential approaches for compiling JavaScript to WASM:  
1. **Compile to core WASM instructions**  
2. **Compile to the WASM component model**  
3. **Compile to QuickJS bytecode and convert the bytecode to WASM**

Currently, the project focuses on the **first approach**, which involves direct compilation to core WASM instructions. 

## Challenges and Progress
Building this compiler has been a challenging yet rewarding experience. It required implementing a custom memory allocator and creating fundamental library functions from scratch.  

### Features Currently Supported:
1. **Loops**  
2. **Conditional Statements (if)**  
3. **Variable Assignments**

## Goals
1. Achieve compatibility with at least **50% of Test262 tests**  
2. **Integrate WASI** support for extended functionality  
3. Experiment with the **WASM component model** and **QuickJS bytecode**, comparing their performance against the current approach


# References
- https://radu-matei.com/blog/practical-guide-to-wasm-memory/
- https://blog.scottlogic.com/2019/05/17/webassembly-compiler.html
- https://blog.scottlogic.com/2018/04/26/webassembly-by-hand.html
- https://blog.ttulka.com/learning-webassembly-4-wasm-memory-and-working-with-strings/
- https://charlycst.github.io/posts/wasm-memory-allocator/
- https://burgers.io/complete-novice-wasm-allocator
- https://healeycodes.com/a-custom-webassembly-compiler
- https://skanehira.github.io/writing-a-wasm-runtime-in-rust/14_build_runtime_wasi.html
- https://cohost.org/eniko/post/171803-basic-memory-allocat
- https://rust-hosted-langs.github.io/book/introduction.html
- https://openhome.cc/eGossip/WebAssembly/String.html
- https://www.maybeuninit.com/2022/05/31/concat-wasm.html
- https://yangdanny97.github.io/blog/2022/10/11/chocopy-wasm-backend
- https://coderundebug.com/learn/wat/tables/#callbacks
- https://www.mirkosertic.de/blog/2018/01/object-oriented-webassembly/
- https://dmitripavlutin.com/what-every-javascript-developer-should-know-about-unicode/?_ga=2.234340766.1446238636.1731907720-479069397.1731907720
- https://www.vssut.ac.in/lecture_notes/lecture1422914957.pdf
- https://hal.science/hal-01937197v1/document
- https://wiki.mozilla.org/IonMonkey/MIR#Building_MIR
- https://mp2.dk/techblog/chowjs/
- https://rynco.me/posts/legacy/aot-compiling-javascript-pt-1/


