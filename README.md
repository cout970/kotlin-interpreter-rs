# kotlin-interpreter-rs
Small Kotlin interpreter made in Rust

### Current state of the project: 
The project is inactive for now, maybe in the future a will make another attempt.
The main reason I stoped was because Kotlin has too many features and I don't know how to implement some of those. I been working on my other [intepreter](https://github.com/cout970/Elm-interpreter) and I learned a lot but not enough to solve all problems.

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


### Manual code check
- [x] Use of val/var in function parameters 
- [x] Valid function name and type for modifier 'operator'
- [ ] Valid modifiers in context
- [x] Modifiers not duplicated
- [x] Modifiers not incompatible to each other
- [ ] Supertype is not final
- [ ] Abstract methods are override in subclass
- [ ] Type name is valid
- [ ] Function call has valid parameters
- [ ] getter visibility must be the same as the property visibility
- [ ] local classes cannot be interfaces
- [ ] A bunch more...
