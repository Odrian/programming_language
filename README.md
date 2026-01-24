# TODO
* add structs (almost done)
* add export
* add #import
* not using cc to link objects?

# Programming language

### Function declaration
```
foo :: (argument : i32) -> void {
    return;
}

bar :: (argument : i32) -> i32 {
    return 0;
}
```
`-> void` is redundant, `return;` at the end of function is redundant

You can call functions with ```name(arg1, ...)```, redundant comma allowed.

You must declare main function with () -> i32 signature
```
main :: () -> i32 {
    return 0;
}
```

### Struct declaration

```
A :: struct {
    field1: u32,
    field2: *A,
}
```

### Variable declaration

You can define variable as follows.  
```
name : type;
name : type = value;
name := value;
```

Each variable and function argument is mutable
```
name = value;
name += value;
name %= value;
```

If and while condition must be a bool expression:
```
if expression {
    ...
}
while expression {
    ...
}
```

Names may contain only `A..Z`, `a..z`, `0..9` and `_`. Using ` you can use any chars you want
```
`if` := 0;
`a > b` := 0;
```

### Global variables

Global can't be defined with value

```
count: i32;

main :: () -> i32 {
    count += 10;
}

```

### Pointers
`&variable` returns pointer to variable  
`&value` will allocate memory in stack for value and return pointer to it  
`*pointer_value = value`

Remember that this will return dangling pointer
```
foo :: () -> *i32 {
    return &5;
}
```

### Reference

Type &T allow use *T as T.  
&T and *T are autocast to each other.  

```
x : i32 = 0;

y : &i32 = &x;
y = 1;
// now x == 1

y2 : *i32 = y; // or = &x;
*y2 = 2;
// now x == 2
```

### Code example
```
swap :: (a: *i32, b: *i32) {
    temp := *a;
    *a = *b;
    *b = temp;
}
```
```
swap :: (a: &i32, b: &i32) {
    temp : i32 = a;
    a = b;
    b = temp;
}
```
```
fibonachi :: (n: i32) -> i32 {
    f0 := 0;
    f1 := 1;
    while n > 0 {
        f0 += f1;
        swap(&f0, &f1);
        n -= 1;
    }
    return f0;
}
```


### Operators precedence:  
`=`, `:=`, `_=`(`_` may be `+-*/`)  
unary `-`, `!`, `*`, `&`  
`as`  
`==`, `!=`, `>`, `>=`, `<`, `<=`  
`*`, `/`, `%`  
`+`, `-`  
`&`  
`|`  
`&&`  
`||`  

### Types

`void`, `bool`, `char`,  
`i8`, `i16`, `i32`, `i64`, `i128`, `isize`,  
`u8`, `u16`, `u32`, `u64`, `u128`, `usize`,  
`f32`, `f64`,  
`*T`, where T is type

`isize`/`usize` is 32 bit at 32bit target, 64 bit at 64bit target

### Literals

bool: `true`, `false`  
char: `'c'` (only ascii chars allowed)  
integer: `239i32`, `239u128`, `239` - using `i32` by default  
float: `0.0f32`, `0.0f64`, `0.0` - using `f64` by default  

Integer and float may contain any number of `_` (first char must be a digit)  
`17_i128`, `50_000_000_000_i64`

### Allowed casts for operator `as`
* `integer` => `integer`
* `float` => `float`
* `bool` => `integer`
* `char` => `integer`
* `u8` => `char` (for `integer` => `char` use `as u8 as char`)
* `*T` => `*U`
* `*T` => `integer`
* `integer` => `*T`
