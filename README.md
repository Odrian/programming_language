# TODO
* add types (in progress)
* add pointers
* add arrays
* import another files
* add print/read function
* add structs
* add enums


* add vararg
* add default arguments
* add function overloading

# Programming language

File can contain any number of functions. Each function must return at any scenario.
```
foo :: (argument : i32) {
    return;
}

bar :: (argument : i32) -> i32 {
    return 0;
}
```

you can call functions with ```name(arg1, ...)```, redundant comma allowed.

You can define variable as follows. Type annotation is sugar, type always can be determinate by value type
```
name : type = value;
name := value;
```

Each variable and argument is mutable
```
name = value;
name += value;
name %= value;
```

if and while condition is bool expression:
```
if expression {
    ...
}
while expression {
    ...
}
```

Each statement in function must end with `;` except last in each scope, for example
```
foo :: () {
    a := 0;
    if a > 0 {
        b := 0;
        b := 0
    }
    return a
}
```

### Operators precedence:  
`=`, `:=`, `_=`(`_` may be `+-*/`)  
unary `-`, `!`  
`==`, `!=`, `>`, `>=`, `<`, `<=`  
`*`, `/`, `%`  
`+`, `-`  
`&`  
`|`  
`&&`  
`||`  

### types

`bool`, `f32`, `f64`,  
`i8`, `i16`, `i32`, `i64`, `i128`,  
`u8`, `u16`, `u32`, `u64`, `u128`,  

### literals

bool: true, false  
integer: `239i32`, `239u128`, `239` - using `i32` by default
float: `0.0f32`, `0.0f64`, `0.0` - using `f64` by default  

integer and float may contain any number of `_`  
(first char must be a digit)
