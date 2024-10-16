# Examples

```js
// "qwerty" -> "qwertz"
"qwerty" | map {'y' ? 'z' : x}
// or
"qwerty" | replace 'y' 'z'
```

```js
// counts 'x' chars in "xxxFnx"
"xxxFnx" | filter {'x'} | len 
```

```js
// "15 dogs" -> "dog the 15th"
// "7 cats" -> "cat the 7th"
"15 dogs"
	| split ' '                 // ["15", "dogs"]
	| nth_map 0 { x + "th" }    // ["15th", "dogs"]
	| nth_map 1 { drop -1 }     // ["15th", "dog"]
	| reverse                   // ["dog", "15th"]
	| join " the "              // "dog the 15th"
```

```js
// "AaaaaaaaBbbbbCccAaaaaaDd"
// Count all series of same letters which are longer than 3

"AaaaaaaaBbbbbCccAaaaaaDd"
	| map lower        // "aaaaaaaabbbbbcccaaaaaadd"
	| chunk_by eq      // ["aaaaaaaa", "bbbbb", "ccc", "aaaaaa", "dd"]
	| filter {len > 3} // ["aaaaaaaa", "bbbbb", "aaaaaa"]
	| map {get 0}      // ["a", "b", "a"]
	| collect          // "aba"
	| len              // 3
```

# Spec

### Basic types

| Type     | Literal                   |
| -------- | ------------------------- |
| `string` | `"qwerty"`                |
| `char`   | `'a'`                     |
| `int`    | `14`                      |
| `array`  | `["one", "two", "three"]` | 

### Lambdas

Lambdas are functions that look like:

```js
{ x + "th" }
{ 'y' ? 'z' : x }
{ get 0 }
```

Lambda takes data as an `x` and performs some expression. The result type of expression is a result type of a lambda itself. Lambda has no arguments, only `x`-data and it only can operate with it.

Sometimes when context does allow it, `x` can be omitted. Ex.:

```js
{ x = 'y' ? 'z' : x }
// equals to
{ 'y' ? 'z' : x }
```
```js
{ (x | len) > 3 }
// equals to
{ len > 3 }
```
```js
{ x | drop -1 }
// equals to
{ drop -1 }
```
### Operators

#### Special operators

- Pipe operator `|`. Chains functions one to another

#### Expression operators

- Concat operator `+`. Concats strings
- Compare operators ` =`, `!=`, `>`, `>=`, `<`, `<=` for strings and ints
- Logic operators `and`, `or`
- Arithmetic operators `+`, `-` for numbers
- Unary operator `-`
- Ternary operator `? :`

#### Precedence

| Operation      | Association | Chaining |
| -------------- | ----------- | -------- |
| \|             | left        | yes      |
| ? :            | right       | nesting  |
| and or         | left        | yes      |
| = != > >= < <= | left        | no       |
| + -            | left        | yes      |
| Unary -        | right       | yes      |


### Functions

Functions are called via pipe operator. Pipe operator implicitly forwards a data to the function. The data is accessible via keyword `x`. Function arguments are passed next to the function name separated by commas:

```js
"mean" | filter { x != 'e' }
// result is "man"
```

In this example we call `filter` function and pass to it a lambda-argument `{ x != 'e' }`. Data passed to the function is any character of the string `"mean"`. Under the hood the function will be called 4 times for each character: `'m'`, `'e'`, `'a'`, `'n'`

### Built-in functions

| Function   | Arguments         | What is it                                            |
| ---------- | ----------------- | ----------------------------------------------------- |
| `map`      | `{T1 -> T2}`      | transforms data from one form to another              |
| `nth_map`  | `int, {T1 -> T2}` | transforms one piece of data from one form to another |
| `filter`   | `{T1 -> bool}`    | decides if current element should stay within data    |
| `split`    | `T`               | splits data by separator                              |
| `join`     | `T`               | joins data by separator                               |
| `collect`  |                   | just joins data. without separator                    |
| `len`      |                   | returns data length as int                            |
| `sort`     |                   | sorts data                                            |
| `reverse`  |                   | reverses data                                         |
| `chunk_by` | `{T, T -> bool}`  | groups data on chunks by criteria                     |
| `replace`  | `T, T`            | replaces one piece of data to another                 |
| `get`      | `int`             | returns exact piece of data                           |
| `drop`     | `int`             | removes exact piece of data                           |
| `insert`   | `int, T`          | inserts new piece of data                             |

### Built-in named lambdas

| Lambda | Signature        | What is it |
| ------ | ---------------- | ---------- |
| `eq`   | `{T, T -> bool}` | =          |
| `neq`  | `{T, T -> bool}` | !=         |
| `lt`   | `{T, T -> bool}` | <          |
| `le`   | `{T, T -> bool}` | <=         |
| `gt`   | `{T, T -> bool}` | >          |
| `ge`   | `{T, T -> bool}` | >=         |
