## Negation expression reduce

```
Expression
    Negation
        Expression
            Literal(1)

->

Expression
    Literal(-1)
```

## Compile time expression calculation

### Int addition/subtraction

```
ArithmeticExpression
    Expression
        Literal(2)
    +
    Expression
        Literal(-2)
->

Expression
    Literal(0)
```

### Int comparison

```
CompareExpression
    Expression
        Literal(2)
    <
    Expression
        Literal(-2)
->

Expression
    Literal(false)
```

### Literal equality

```
CompareExpression
    Expression
        Literal("azaz")
    =
    Expression
        Literal("azaz")
->

Expression
    Literal(true)
```

## Implicit X-value desugaring

Here we make AST tree more complicated but more explicit since X-value omitting is a pure syntax sugar and can be
confusing while processing AST-tree as it is.

### Recover X-value EQ operation

Code:
```
{'y' ? 'z' : x}
->
{x = 'y' ? 'z' : x}
```

AST transformation:
```
Lambda
    Expression
        TernaryOperator
            Expression
                Literal('y')
            Expression
                Literal('z')
            Expression
                XValue
->
Lambda
    Expression
        TernaryOperator
            Expression
                CompareExpression
                    Expression
                        XValue
                    =
                    Expression
                        Literal('y')
            Expression
                Literal('z')
            Expression
                XValue
```

### Recover X-value functions chain start

Probably this transformation will not be necessary since it could be potentially managed at a parsing stage.

Code:
```
{ split '.' | collect }
->
{ x | split '.' | collect }
```

AST transformation:
```
?
->
Lambda
    Expression
        FunctionsChain
            FunctionData
                XValue
                FunctionCall
					"split"
					FunctionArgument
					    Expression
				            Literal('\'')
				FunctionCall
					"collect"
```