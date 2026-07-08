# flx design review

*July 2026. A review of the notation and its implementation, the normative
specification of the operator model adopted on branch
`fix-ops-parser-spacing`, and a roadmap of design suggestions.*

## 1. What flx is

flx is a **reader**: it turns text into a generic operator tree without knowing
the language being expressed. The tree algebra (`flx/expr.ml`) has atoms (ids,
ints, strings, chars), three delimiters (`()` `{}` `[]`), n-ary separators
(`,` `;` `.` `|`), juxtaposition sequences, prefix/infix/postfix operator
applications, quote/unquote, string templates, and attributes. A separate
layer (`fl/`) interprets these trees as the OCaml parsetree.

This two-layer architecture is the project's best idea and worth protecting:
the reader is small, language-agnostic, and usable on its own as a data
notation; all OCaml-specific meaning lives in the mapping. The closest prior
art is **Rhombus's shrubbery notation** and **Honu's enforestation** (Pratt
parsing over raw trees), and the printed form (`_+_`, `+_`, `_+`) follows the
Agda mixfix convention. **Elixir** is the strongest precedent for the
metaprogramming ambition: a fixed, tiny, uniform AST that macros traverse,
with quote/unquote as tree templating.

## 2. The operator model (normative)

The historic pain point was prefix/postfix operators, forced by OCaml idioms:
labeled args `f ~a ~b`, optional labels `~a?`, polymorphic variants `#A`,
macro calls `m! (...)`. Earlier iterations solved these with per-operator
special cases (`~` was a "flexible" class; `#` and `!` were pinned to
juxtaposition precedence). That path is a treadmill: every new idiom adds a
class, and users cannot predict fixity without memorizing the class table.

The adopted model has **no per-operator classes**. Two principles decide
every case; both are implemented in a single function,
`Precedence.fixity` (`flx/precedence.ml`).

### 2.1 Token classes

- **Separator punctuation** — `,` `;` `.` `|` — is *spacing-blind*: it
  always acts as infix/n-ary punctuation. `a, b`, `a ,b`, `a , b` all mean
  the same thing.
- **Every other operator** resolves fixity and precedence from spacing alone,
  uniformly.

### 2.2 Fixity from spacing (one rule for all operators)

In *prefix position* (an operand is expected), an operator token is a prefix
operator:

- attached to the next token → **tight prefix**: binds the following attached
  cluster (`~a` → `(~. a)`; `~a~b` → `(~. (.~. a b))`)
- spaced from the next token → **loose prefix**: binds the whole following
  juxtaposition sequence (`+ a b c` → `(+_ (_ a b c))`)
- followed by a terminator → **operator atom** (`(+)` → `((_) +)`)

In *infix position* (after a complete operand), for an operator token:

- spaced on both sides *or* attached on both sides → **infix**
- attached on the left only (`a? b`) → **postfix** on the last operand
- attached on the right only (`f ~a`) → operand boundary: the operator starts
  a new juxtaposition item, as a prefix. This single rule is what makes
  `f ~a ~b c` parse as `(_ f (~_' a) (~_' b) c)` with no special cases.
- followed by a terminator → **postfix on the whole left expression**
  (`f a ~` → `(_~ (_ f a))`)

This is Swift's whitespace rule adapted to a language with juxtaposition.

### 2.3 Precedence tier from attachment: "things glued together parse together"

- **Loose (spaced) operators** keep the base table, all *below*
  juxtaposition: `f a + b` is `(f a) + b`, as in OCaml/Haskell.
- **Tight (attached) operators** bind *above* juxtaposition and *below* `.`,
  preserving their relative order and associativity
  (`tight(p) = sign(p) · (juxt + |p|)`): `f a+b c` is `f (a+b) c`.
- Precedence bands:
  `terminators 0 < semi < comma < attr < loose 30–110 < juxt 200 < tight 230–310 < dot 400`.

### 2.4 The tier is recorded in the tree

`prefix`/`infix`/`postfix` nodes carry their tier (`` `tight ``/`` `loose ``),
so consumers — and a future round-trip printer — can tell `a+b` from
`(a + b)` and `~a` from `~ a`. In the printed form a tight operator head has
a `'` suffix: `a+b` is `(_+_' a b)`, `a + b` is `(_+_ a b)`. (`'` is not an
operator character, so the marker can never be confused with part of the
operator symbol.) Separator punctuation is spacing-blind and always loose.

Consequences, all pinned as golden tests in `tests/basic/ops.test`:

| Input | Parse | Note |
|---|---|---|
| `f a+b c` | `(_ f (.+. a b) c)` | attached = one argument |
| `f a + b` | `(_+_ (_ f a) b)` | spaced = application binds tighter |
| `a+b * c` | `(_*_ (.+. a b) c)` | spacing wins over base precedence |
| `f x a=1 b=2` | `(_ f x (.=. a 1) (.=. b 2))` | config/named-arg idiom |
| `a.b+c` | `(.+. (. a b) c)` | `.` is above the tight band |
| `-1 - 2` | `(_-_ (-. 1) 2)` | |
| `-1-2` | `(-. (_-. 1 2))` | the whole cluster is the operand; see §4.3 |
| `f ~a?` | `(_ f (~. (.? a)))` | optional labels for free |
| `m! (x)` | `(_ (.! m) ((_) x))` | macro-call idiom |
| `a~` vs `a ~` | `(.~ a)` vs `(_~ a)` | tier visible in the tree |

On `a+b * c`: spacing that contradicts base precedence is *accepted* with the
visual-grouping meaning (the tight cluster wins). Fortress instead *rejected*
inconsistent spacing; rejecting is a reasonable stricter mode for a future
linter/formatter, but the parse itself is deterministic and matches the eye.

The tier rule also fixed a latent bug: the earlier `tighten` (+1000) put
attached operators *above* `.`, so `a.b~c` grouped as `(. a (_~_ b c))`; it
is now `(_~_ (. a b) c)`.

Known wrinkle (documented, not special-cased): operator characters munch
maximally, so `x =#sm` lexes `=#` as one operator (→ `(_ x (=#_ sm))`).
Write `x = #sm`. A formatter should normalize this.

## 3. Code review findings

Shipped in this change:

- `Precedence.get_sp`, the `flexible` class, and the `~`/`#`/`!` special
  cases in `parse_infix` are gone; the whole spacing decision lives in
  `Precedence.fixity`, returning an explicit
  `Stop | Juxt_item | Infix of int | Postfix of int`.
- `parse_infix` and `parse_seq` are driven by that one function, so fixity is
  no longer decided half in the precedence table and half in parser matches.

Remaining findings, roughly in priority order:

### 3.1 Comments are AST atoms and change tree shape (high)

`2 + /* c */ 1` parses as `(_+_ 2 (_ (// " c") 1))` — the comment turns `1`
into an application. For a notation whose point is "source files are data",
a comment must never change the shape of neighboring nodes. Make block
comments *trivia*: either drop them in the parser or attach them as metadata
to the nearest node (needed anyway for a formatter). This requires removing
`` `comment `` from the expression type.

### 3.2 No source locations (high)

`Expr.t` carries no spans, and `fl/fl.ml` stamps everything `Location.none`.
This blocks precise parse/mapping errors, merlin integration, and useful
staging diagnostics. Retrofit is mechanical but touches everything — do it
before the tree type has more consumers. Suggested shape:
`type t = { desc : desc; span : Span.t }` (or a side table if the polymorphic
variant surface must stay). This also enables the "show where the paren was
opened" error (old TODO).

### 3.3 Literal gaps for the data-notation goal (medium)

- No floats at all (`3.14` is a lex error; `lispy.fl` already wants them).
- No char escapes: `'\n'` is a lex error.
- `01` silently lexes as two ints (`0` juxtaposed with `1`) because of the
  `(digit | nonzero digit+)` regex — should be a single token or an error.
- No hex/underscored ints, no `\u{...}` string escapes (JSON interop).
- Negative literals: with the tier rule, `-1-2` is `(-_ (_-_ 1 2))`. If
  arithmetic matters, lex `-` attached to a digit in prefix position as a
  negative literal; that makes `-1-2` = `(_-_ -1 2)` and is the conventional
  fix.

### 3.4 Canonical printer + round-trip (medium, prerequisite for staging)

The only printer today is the debug sexp form. Metaprogramming output needs a
printer that emits *valid flx*, with the property `parse (print t) = t`.
Once it exists, fuzz the reader (crowbar/afl) with that property plus
"no input raises anything but a structured parse error". The golden-test
corpus is already shaped like a spec; round-trip makes it enforceable.

### 3.5 Quote/unquote design for staging (medium)

Today `` ` `` and `$` grab one prefix expression. For MetaOCaml/go-generate
style staging you also need:

- **splice**: insert a list of trees into a `seq`/separator context —
  e.g. `$..xs` (distinct node from `$x`);
- **defined nesting**: `` ` `` increments quote level, `$` decrements; specify
  what `` ``a `` and `` `$a `` mean;
- an explicit statement that **hygiene is out of scope**: flx quoting is tree
  templating, not macro expansion — name capture is the consumer's problem.

### 3.6 AST simplification candidates (low, breaking)

- `comma/semi/dot/pipe` are one concept: `` `sep of string * t list ``.
- Consider whether `attr` needs to be a reader node at all, or is a
  convention over prefix-`@` (its current rbp=25 capture of `@a 1 = 2` is the
  one thing plain prefix parsing can't express — keep only if that matters).
- For *consumers*, consider exposing an Elixir-style uniform view —
  every node as `(head, meta, args)` — so tree-walking macros pattern-match
  one shape. The parser-facing type can stay rich; the uniform view can be a
  function.

### 3.7 Lexer internals (low)

- Template state is threaded by copying the lexer record while sharing the
  `current` ref and `lexbuf` (`{ lex0 with in_template = true }`) — it works,
  but an explicit template-depth stack in one mutable state would be clearer
  and would survive refactors.
- `consumed_whitespace` is written and never read — delete.
- `Token.sp` conflates "start of input" with "no space" (harmless today
  because prefix position dominates, but worth a comment).

### 3.8 Scope discipline for `fl/` (process note)

`lispy.fl` sketches coverage of nearly the whole OCaml parsetree (GADTs,
objects, functors, letop, ...). Let the *notation* stay frozen while the
*mapping* grows: any OCaml feature should be expressible as an
interpretation of existing tree shapes, never as a new reader construct.
When a mapping feels impossible without touching the reader, that's the
signal to redesign the mapping, not the notation.

## 4. Prior art worth keeping at hand

- **Rhombus shrubbery notation** — group/block/operator reader that defers
  meaning; the design documents discuss exactly the juxtaposition-vs-operator
  tensions flx faces.
- **Honu** (Rafkind & Flatt) — enforestation: Pratt parsing interleaved with
  macro expansion; the likely shape of flx macros if they ever run *during*
  reading.
- **Swift** — the whitespace-fixity rule adopted here.
- **Fortress** — spacing/precedence *consistency checking*; the stricter mode
  a linter could adopt.
- **Elixir** — tiny uniform AST + quote/unquote as the entire macro system;
  the best existence proof for flx's metaprogramming goal.
- **Wolfram FullForm** — everything is `head[args]`; the uniform-view target.
- **Dhall / KDL / Nickel** — scope calibration for the standalone
  data-notation use case.
