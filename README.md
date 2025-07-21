# TODO
* parse "x = y" (done)
* parse if, while (done)
* parse function (done)
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

* name = expression
* if expression { 'any number of statements' }
* name :: ('arguments') { 'any number of statements' }\
  'arguments' - any number of name seperated with comma

## valid expression

* name
* number
* (expression)
* expression + expression
