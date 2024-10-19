## Grammar

```
program -> expression

expression -> primary_expression
expression -> unary_expression
expression -> arithmetic_expression
expression -> compare_expression
expression -> logic_expression
expression -> ternary_expression
expression -> functions_chain

primary_expression -> literal
primary_expression -> x_value
primary_expression -> '(' expression ')'

unary_expression -> '-' expression

x_value -> 'x'

literal -> <STRING>
literal -> <CHAR>
literal -> <NUMBER>
literal -> <BOOL>
literal -> array

array -> '[' ']'
array -> '[' literal_seq ']'

literal_seq -> literal ',' literal_seq
literal_seq -> literal

ternary_expression -> compare_expression '?' expression ':' expression

compare_expression -> expression compare_operation expression
compare_expression -> x_eq_expression

compare_operation -> '='
compare_operation -> '!='
compare_operation -> '>'
compare_operation -> '>='
compare_operation -> '<'
compare_operation -> '<='

x_eq_expression -> literal
x_eq_expression -> expression

logic_expression -> compare_expression logic_operation compare_expression

logic_operation -> 'and'
logic_operation -> 'or'

arithmetic_expression -> expression arithmetic_operation expression

arithmetic_operation -> '+'
arithmetic_operation -> '-'

functions_chain -> functions_chain_start functions_chain_rest

functions_chain_start -> literal
functions_chain_start -> x_value

functions_chain_rest -> '|' function_call functions_chain_rest
functions_chain_rest -> '|' function_call

function_call -> function_name function_arguments

function_name -> <ID>

function_arguments -> function_argument function_arguments
function_arguments -> function_argument

function_argument -> expression
function_argument -> lambda

lambda -> named_lambda
lambda -> anonymous_lambda

named_lambda -> <ID>

anonymous_lambda -> '{' lambda_expression '}'

lambda_expression -> expression
lambda_expression -> implicit_x_chain

implicit_x_chain -> function_call functions_chain_rest
implicit_x_chain -> function_call
```

## Expressions precedence

From the highest priority to the lowest:

- Negation
- Arithmetic Expression
- Compare Expression
- Logic Expression
- Ternary Operator
- Piping

So, the following code:

```
2 - 1 < 15 and "foo" != "42" + '4'
```

equals to

```
(2 - (1 < 15)) and ("foo" != ("42" + '4'))
```

and the following code:

```
2 - 1 < 15 and "foo" != ("42" | filter {'4'}) + '4'
```

has to have `()` around piping operation `|` since it has the lowest priority of all the expressions.