# EBNF Grammar

## Identifier

```
Identifier = [a-zA-Z_][a-zA-Z0-9_]*
```

## Number

```
Number = [0-9]+(\.[0-9]+)?
```

## Primary

```
Primary = Identifier | Number | "(" Expression ")"
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

## Expression

```
Expression = Equality
```
