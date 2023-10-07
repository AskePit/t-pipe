## Grammar

- program:
    - expression

- expression:
  easy_to_parse_expression post_expression

- easy_to_parse_expression:
    - literal
    - x_value
    - functions_chain
    - **(** expression **)**

- post_expression:
    - arithmetic_expression
    - logic_expression
    - compare_expression
    - ternary_operator
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