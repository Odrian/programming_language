
## [To readme](../README.md)

# Modules

Module is directory with .pr files. Currently, you can compile only single module.

Inside module, you can `import` symbols (functions, variables, types) from another files from same module.

```
# src/main.pr
import super::number::get_exit_code;

main :: () -> i32 {
  return get_exit_code();
}

# src/number.pr

get_exit_code :: () -> i32 {
  return 10;
}
```

There is no restriction when splitting file into more files inside single module, because module compiled as single LLVM module, so it has
