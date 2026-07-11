
# Syntax

## Grammar

```
term =
  // Atom
  | id
  | int
  | float
  | str
  | char

  // Block
  | "(" term ")"
  | "[" term "]"
  | "{" term "}"

  // Field
  | term "." term

  // Sequence
  | term term+
  | term "," term*
  | term ";" term*

  // Syntax
  | "`" term
  | "$" term

  // Operators
  | op
  | op term
  | term op
  | term op term

prefix-symbol = "!" | "~" | "?" | "\" | "#"

infix-symbol =
  | "+"
  | "-"
  | "="
  | "*"
  | "/"
  | "%"
  | "<"
  | ">"
  | "|"
  | "&"
  | "@"
  | ":"
  | "$"
  | "^"
```


## Operator precedence

| Operator      | Associativity | Precedence |
|---------------|---------------|------------|
| `;`           | Left          | 10         |
| `,`           | Left          | 20         |
| `@`           | Left          | 25         |
| `=`           | Right         | 30         |
| `|`           | Left          | 40         |
| `:`           | Right         | 50         |
| `::`          | Left          | 60         |
| `:=`          | Right         | 60         |
| `<-`          | Right         | 60         |
| `&`           | Right         | 70         |
| `&&`          | Right         | 70         |
| `||`          | Right         | 70         |
| `**`          | Right         | 80         |
| juxtaposition | Left          | 200        |
| `.`           | Left          | 400        |
| `@...`        | Left          | 100        |
| `=...`        | Left          | 101        |
| `<...`        | Left          | 102        |
| `>...`        | Left          | 102        |
| `|...`        | Left          | 102        |
| `+...`        | Left          | 103        |
| `-...`        | Left          | 103        |
| `*...`        | Left          | 104        |
| `/...`        | Left          | 104        |
| other         | Left          | 100        |
