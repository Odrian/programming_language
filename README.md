# TODO
* parse "x = y" (done)
* parse if, while (done)
* parse function (done)
* linking (done)
* compile with int8 type only (in progress)
* add types
* add arrays

# Programming language

Variable declaration
```
name : type = value
name := value
```

Set variable
```
name = value
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
    return ...
}

name(argument)
```

# Parsing algorithm

input file must contain any number of statements

## valid statements

* 'any expression'
* name := expression
* name = expression
* if expression { 'any number of statements' }
* while expression { 'any number of statements' }
* name :: ('arguments') { 'any number of statements' }\
  'arguments' - any number of name seperated with comma
* return expression

## valid expression

* name
* number
* (expression)
* expression + expression
* name()
