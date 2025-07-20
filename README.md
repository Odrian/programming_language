# TODO
* parse "x = y" (done)
* parse if, while (done)
* parse function (in progress)
* add print, read function
* linking
* compile with int8 type only
* add types
* add arrays

# Programming language

Variable declaration
```
name : type = value
```

If statement
```
if expression {
    ...
}
```

Loop
```
while expression {
    ...
}
```

Function declaration
```
name :: (argument : type) -> result_type {
    ...
}

name(argument)
```

# Parsing algorithm

input file must contain any number of statements

## valid statements

* text = expression
* if expression { 'any number of statements' }

## valid expression

* text
* number
* (expression)
* expression + expression
