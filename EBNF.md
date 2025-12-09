# EBNF Grammar

## Identifier

```
Identifier = [a-zA-Z_][a-zA-Z0-9_]*
```

## Number

```
Number = [0-9]+(\.[0-9]+)?
```

## String

```
String = ('[^']*')|("[^"]*")
```


## Primary

```
Primary = Identifier
        | Number
        | String
        | "true"
        | "false"
        | "undefined"
        | "(" Expression ")"
```

## Unary

```
Unary = ("-" | "!") Unary | Primary
```

## Factor

```
Factor = Unary (("*" | "/") Unary)*
```

## Term

```
Term = Factor (("+" | "-") Factor)*
```

## Comparison

```
Comparison = Term (("<" | ">" | "<=" | ">=") Term)*
```

## Equality

```
Equality = Comparison (("==" | "!=") Comparison)*
```

## LogicalAnd

```
LogicalAnd = Equality ( "&&" Equality )*
```

## LogicalOr

```
LogicalOr = LogicalAnd ( "||" LogicalAnd )*
```

## Expression

```
Expression = LogicalOr
```
