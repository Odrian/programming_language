## [To readme](../README.md)

# Extern attribute

`#extern` can be used to declare functions or variable from libc  
Only extern function can be Variadic.  

### Function
Same as regular function declaration, but without argument names and may be variadic.
```
#extern
abs :: (i32) -> i32;

#extern
fprintf :: (*FILE, *char, ...);
```

### Global variables
Same as regular global variable declaration.
```
FILE :: struct {}

#extern
stdout: *FILE;
```

### Multiline
Due to different declaration syntax `#extern` not working with `#expand`. Now it applicable to block with expanded.
```
#extern {
    abs :: (i32) -> i32;
    printf :: (*char, ...);
}
```

### example
Warning: stdout not declared on some platforms.
```
// we don't care about FILE layout and just use *FILE
FILE :: struct {}

#extern
stdout: *FILE;

#extern
abs :: (i32) -> i32;

#extern
fprintf :: (*FILE, *char, ...);

main :: () -> i32 {
    fprintf(stdout, "Hello user %d", abs(-765));
    return 0;
}
```
