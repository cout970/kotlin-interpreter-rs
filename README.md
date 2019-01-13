# kotlin-interpreter-rs
Small Kotlin interpreter made in Rust

### Why?
Why a new interpreter if there is already a kotlin compiler and a kotlin REPL?
- The current version of the Kotlin REPL compiles the source code to JVM bytecode, so it takes a 
lot of time to compile plus it requieres some time to warmup the JVM
- Currently hotswaping in the JVM is quite slow, and affects the development speed significantly
- This interpreter can be embembed into other applications like a game engine or a config reader (using kotlin dsl builder)

### Primary goal
The main goal is to make a interpreter that can run any kotlin code (without corroutines or other more complex features) 
and requieres the minimun amount of time to hotswap piezes of code.  


Since the main purpose of this interpreter is to reduce time between code modification and execution, 
the runtime efficency get a second place in the priority list. 
If you really want to get runtime efficiency use the standart compiler for JVM or Native.

### Second goal
At some point I want to be able to use this project to make custom scripts for the Amethyst game engine. 
If we combine the eficiency and safety of Rust with the flexibily and conciseness of Kotlin we get a great 
union that allows to do quick development while maintaining great performance.
