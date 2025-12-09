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

## Boolean

```
Boolean = "true" | "false"
```

## Undefined

```
Undefined = "undefined"
```

## Field

```
Field = Identifier ":" Expression
```

## ObjectLiteral

```
ObjectLiteral = "{{" [ Field ("," Field)* ] "}}"
```

## Primary

```
Primary = Identifier
        | Number
        | String
        | Boolean
        | Undefined
        | ObjectLiteral
        | "(" Expression ")"
```

## FieldAccess

```
FieldAccess = Primary { "." Identifier }
```

## Unary

```
Unary = ("-" | "!") Unary | FieldAccess
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
