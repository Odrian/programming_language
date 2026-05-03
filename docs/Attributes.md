## [To readme](../README.md)

# Attributes

Compiletime configuration for code
```
// sinle attributes
#attribute
statement

// multiple attribute, parsed left to right
#[attribute1, attribute2, ...]
statement

// same as
#attribute1
#[attribute2, ...]
statement
```

## Cfg attribute

if condition is false remove statement (after ast parsing, before linking)

```
#cfg(cond)
statement
```

### Cfg_if macro

Allows to get condition in code

```
if #cfg_if(cond) { .. }
let is_cond = #cfg_if(cond);
```

### Condition syntax
```
cond -> any(cond [, cond]*)
cond -> all(cond [, cond]*)
cond -> not(cond)

cond -> name
cond -> name = value
```
### Available names
```
linux, macos, windows

target_os = "macos"
target_arch = "aarch64"
pointer_width = "64"
```

## Expand attribute

```
#expand {
    statement
    ...
}
```
is equivalent to (don't create new local context, expanded before linking)
```
statement
...
```

### Multiline attributes

Expand propagate attributes to every statement, so it can be used to apply attribute to multiple lines of code

```
#[attribute] #expand {
    statement
    ...
    statement
}

// same as

#[attribute]
statement
...
#[attribute]
statement
```
