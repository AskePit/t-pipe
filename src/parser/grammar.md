## Grammar

- program:
    - expression

- expression:
    - left_expression right_expression

- left_expression:
    - literal
    - x_value
    - **(** expression **)**

- right_expression:
    - arithmetic_expression
    - logic_expression
    - compare_expression
    - ternary_operator
    - functions_chain
    - **\<EMPTY\>**

- x_value: **x**

- literal
    - string: **"qwerty"**
    - char: **'q'**
    - int: **12**
    - bool: **true** | **false**
    - array

-  array:
    - **\[**  literal_seq **\]**

- literal_seq:
    - literal literal_seq_tail

- literal_seq_tail:
    - **,** literal_seq
    - **\<EMPTY\>**

- ternary_operator:
    - **?** expression **:** expression

- compare_expression:
    - normal_compare_expression
    - x_eq_expression

- normal_compare_expression:
    - compare_operation expression

- x_eq_expression:
    - literal
    - arithmetic_expression

- compare_operation:
    - **= != > >= < <=**

- logic_expression:
    - logic_operation compare_expression

- logic_operation:
    - **and | or**

- arithmetic_expression:
    - arithmetic_operation expression

- arithmetic_operation:
    - **+ -**

- functions_chain:
    - functions_chain_start functions_chain_rest

- functions_chain_start:
    - literal
    - x_value

- functions_chain_rest:
    - **|** function_call functions_chain_rest_tail

- functions_chain_rest_tail:
    - functions_chain_rest
    - **\<EMPTY\>**

- function_call:
    - function_name function_arguments

- function_name: **\<ID\>**

- function_arguments:
    - function_argument function_arguments_tail

- function_arguments_tail:
    - **\<SPACE\>** function_argument
    - **\<EMPTY\>**

- function_argument:
    - expression
    - lambda

- lambda:
    - named_lambda
    - anonymous_lambda

- named_lambda: **\<ID\>**

- anonymous_lambda:
    - **{** lambda_expression **}**

- lambda_expression:
    - expression
    - implicit_x_chain

- implicit_x_chain:
    - function_call functions_chain_rest_tail

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