---
marp: true
paginate: true
---
<!-- page_number: true -->

# Parser combinators

---

## Reality check: Parsing is HARD

---

# Available tools

* PEG: Parsing expression grammar
* yacc/bison, often used with lex/flex
    * camllex, camlyacc
    * FsLexYacc
* ANTLR

---

# Writing my own

"What I cannot create, I do not understand"

Btw. It **really** helps if your language has functions as first-class citizens

---

# Foundation

---

# Context

```fsharp
type Context = {
    Text: string
    Index: int
}
```

---

# Result

```fsharp
type Success<'value> = {
    Value: 'value
    Context: Context
}

type Failure = {
    Expected: string
    Context: Context
}

type Result<'value> =
    | Success of Success<'value>
    | Failure of Failure
```

---

# Parser

```fsharp
type Parser<'value> = (Context -> Result<'value>)

let run parser input =
    let ctx = { Text = input; Index = 0 }
    parser ctx
```

---

# Let's start parsing!

---

# A simple string

```fsharp
"parsec"
|> run (* ??? *) 
```

---

# How to parse a string

```fsharp
let str expected =
    (fun ctx ->
        let length = String.length expected
        let fits = ctx.Index + length <= String.length ctx.Text
        if fits && ctx.Text.Substring(ctx.Index, length) = expected then
            Success {
                Value = expected
                Context = { ctx with Index = ctx.Index + length }
            }
        else
            Failure {
                Expected = expected
                Context = ctx
            }
    )
```

---

# Parsing a single string

```fsharp
"parsec"
|> run (str "parsec") 
// Success: parsec
```

---

# Parsing many strings?

```fsharp
"parser combinators are awesome"
|> run (str "parser combinators are awesome") 
// Success: parser combinators are awesome
```

---

# Uninformative failure

```fsharp
"parser combinator are awesome"
|> run (str "parser combinators are awesome")
// Failure: Expecting parser combinators are awesome
// parser combinator are awesome
// ^
```

---

# Combining parsers

---

# Combining operator

```fsharp
let (>>.) parserA parserB =
    (fun ctx ->
        match parserA ctx with
        | Success success -> parserB success.Context
        | Failure failure -> Failure failure
    )
```

---

# Parsing many strings

```fsharp
let parser = str "parser"
let space = str " "
let combinators = str "combinators"
let are = str "are"
let awesome = str "awesome"

let together =
    (parser >>. space >>. combinators >>. space >>. are >>. space >>. awesome)
```

---

# Success

```fsharp
"parser combinators are awesome"
|> run together 
// Success: awesome
```

---

# Failure

```fsharp
"parser combinator are awesome"
|> run together 
// Failure: Expecting combinators
// parser combinator are awesome
//        ^
```

---

# Adding complexity

---

# Parsing DNA

The four bases:
* Adenine
* Thymine
* Cytosine
* Guanine

---

# Parsing DNA

```fsharp
"a"
|> run (str "a") 
// Success: a
```

---

# Parsing alternatives

---

# Alternative operator

```fsharp
let (<|>) parserA parserB =
    (fun ctx ->
        match parserA ctx with
        | Success successA -> Success successA
        | Failure failureA ->
            if failureA.Context.Index <> ctx.Index then
                Failure failureA
            else
                match parserB ctx with
                | Success successB -> Success successB
                | Failure failureB ->
                    Failure {
                        Expected = sprintf "%s or %s" failureA.Expected failureB.Expected
                        Context = failureB.Context
                    }
    )
```

---

# Parsing DNA

```fsharp
"g"
|> run (str "a" <|> str "t" <|> str "c" <|> str "g") 
// Success: g

"y"
|> run (str "a" <|> str "t" <|> str "c" <|> str "g") 
// Failure: Expecting a or t or c or g
// y
// ^
```

---

# Parser transformation

---

# More than strings

```fsharp
type Molecule =
    | A
    | T
    | C
    | G
```

---

# Mapping operator

```fsharp
let (|>>) parser f =
    (fun ctx ->
        match parser ctx with
        | Success success ->
            Success {
                Value = f success.Value
                Context = success.Context
            }
        | Failure failure -> Failure failure
    )
```

---

# Parsing a molecule

```fsharp
let a = str "a" |>> fun _ -> A
let t = str "t" |>> fun _ -> T
let c = str "c" |>> fun _ -> C
let g = str "g" |>> fun _ -> G
let molecule = a <|> t <|> c <|> g
```

---

# DNA Sequence

```fsharp
"agtgcgttac"
|> run (* ??? *) 
```

---

# Parsing sequences

---

# Sequence function

```fsharp
let many parser =
    let rec loop parser acc =
        (fun ctx ->
            match parser ctx with
            | Success success -> 
                loop parser (success.Value :: acc) success.Context
            | Failure failure -> 
                if failure.Context.Index <> ctx.Index then
                    Failure failure
                else
                    Success {
                        Value = List.rev acc
                        Context = failure.Context
                    }
        )
    loop parser []
```

---

# DNA Sequence

```fsharp
"agtgcgttac"
|> run (many molecule) 
// Success: [A; G; T; G; C; G; T; T; A; C]
```

---

# Example: DNA mutation

---

# DNA mutation

Somewhere in the OCA2, there is a single gene mutation that tells you if you'll have blue or brown eyes.

* TAA**G**TG = blue eyes
* TAA**A**TG = brown eyes

```fsharp
type EyeColor =
    | Blue
    | Brown

let blue = t >>. a >>. a >>. g >>. t >>. g |>> fun _ -> Blue
let brown = t >>. a >>. a >>. a >>. t >>. g |>> fun _ -> Brown
```

---

# Success

```fsharp
"taagtg"
|> run (blue <|> brown) 
// Success: Blue
```

---

# Failure

```fsharp
"taaatg"
|> run (blue <|> brown) 
// Failure: Expecting g
// taaatg
//    ^
```

---

# Backtracking function

```fsharp
let attempt parser =
    (fun ctx ->
        match parser ctx with
        | Failure _ -> Failure { Expected = ""; Context = ctx }
        | Success success -> Success success
    )
```

---

# Success

```fsharp
"taaatg"
|> run (attempt blue <|> brown) 
// Success: Brown
```

---

# Custom error messages

```fsharp
let (<?>) parser expected =
    (fun ctx ->
        match parser ctx with
        | Success success -> Success success
        | Failure failure ->
            Failure { failure with Expected = expected }
    )
```

---

# Contextful errors

```fsharp
let blue = blue <?> "In the middle of parsing Blue"
let brown = brown <?> "In the middle of parsing Brown"

"taaatg"
|> run (blue <|> brown) 
// Failure: In the middle of parsing Blue
// taaatg
//    ^
```

---

# Allowing state machines

```fsharp
type Context<'state> = {
    State: 'state
    Text: string
    Index: int
}

type Success<'value, 'state> = {
    Value: 'value
    Context: Context<'state>
}

type Failure<'state> = {
    Expected: string
    Context: Context<'state>
}

type Result<'value, 'state> =
    | Success of Success<'value, 'state>
    | Failure of Failure<'state>

type Parser<'value, 'state> = (Context<'state> -> Result<'value, 'state>)

let run init parser text =
    let ctx = { State = init; Text = text; Index = 0 }
    parser ctx 
```

---

# Useful functions

```fsharp
let getState =
    (fun ctx ->
        Success { 
            Value = ctx.State
            Context = ctx
        }
    )

let setState state =
    (fun ctx ->
        Success {
            Value = ()
            Context = { ctx with State = state }
        }
    )

let updateState f =
    (fun ctx ->
        Success {
            Value = ()
            Context = { ctx with State = f ctx.State }
        }
    )
```

---

# Example: Balancing parenthesis

---

```fsharp
type State =
    | Empty
    | Paren of int

let addParen = function
    | Empty -> Paren 1
    | Paren n -> Paren (n + 1)

let remParen = function
    | Empty -> Paren (-1)
    | Paren n -> Paren (n - 1)

let parseParens =
    (str "(" >>. updateState addParen)
    <|> (str ")" >>. updateState remParen)
```

---

# Success

```fsharp
"((()))()())))))((("
|> run Empty (many parseParens >>. getState) 
// Success: Paren -2
```

---

# Making a parser depend on a state

```fsharp
let stateSatisfies f = 
    (fun ctx ->
        if f ctx.State then 
            Success {
                Value = ()
                Context = ctx
            }
        else
            Failure {
                Expected = "User state mismatch"
                Context = ctx
            }
    )
```
---

# Making success depend on the state

```fsharp
let isEmpty = function
    | Empty -> true
    | Paren 0 -> true
    | _ -> false

"((()))()())))))((("
|> run Empty (many parseParens >>. stateSatisfies isEmpty) 
// Failure: User state mismatch
// State = Paren -2
// ((()))()())))))(((
//                   ^
```

---

# Success

```fsharp
"((()))()())))))((((("
|> run Empty (many parseParens >>. stateSatisfies isEmpty) 
// Success: ()
```

---

# Example: Left recursion

---

# A simple AST

```fsharp
type Expr =
    | A
    | B
    | List of Expr list
```

---

# Parsing it

```fsharp
let parseA = str "a" |>> fun _ -> A
let parseB = str "b" |>> fun _ -> B
let parseList = sepBy1 parseExpr (str " ") |>> List
let parseExpr = parseList <|> parseA <|> parseB

"a"
|> run parseExpr
// Stack overflow
```

---

# Parsing it ... again

```fsharp
let parseA = str "a" |>> fun _ -> A
let parseB = str "b" |>> fun _ -> B
let parseList = sepBy1 parseExpr (str " ") |>> List
let parseExpr = parseA <|> parseB <|> parseList

"a"
|> run parseExpr
// Success: A
```

---

# Let's try a list

```fsharp
"a b"
|> run parseExpr
// Success: A
```

---

# Parsing it ... like we used to

```fsharp
let parseA = str "a" |>> fun _ -> A
let parseB = str "b" |>> fun _ -> B
let parseList = sepBy1 parseExpr (str " ") |>> List
let parseExpr = parseList <|> parseA <|> parseB

"a b"
|> run parseExpr
// Stack overflow
```

---

# Left recursion

```fsharp
let parseA = str "a" |>> fun _ -> A
let parseB = str "b" |>> fun _ -> B
let parseValue = parseA <|> parseB
let parseExpr =
    sepBy1 parseValue (str " ")
    |>> fun exprs ->
        match exprs with
        | [expr] -> expr // Value: A or B
        | _ -> List exprs

"a"
|> run parseExpr
// Success: A

"a b"
|> run parseExpr
// Success: List [A; B]
```

---

# Example: List of integers

---

# How could we parse this

```fsharp
"[1,2,3]"
|> run (* ??? *) 
```

---

# More helpers : parsing integers

```fsharp
let anyOf chars =
    (fun ctx ->
        let char = ctx.Text.[ctx.Index]
        match Seq.tryFind (fun c -> c = char) chars with
        | Some found ->
            Success {
                Value = found
                Context = { ctx with Index = ctx.Index + 1 }
            }
        | None ->
            Failure {
                Expected = sprintf "Any of these: '%O'" chars
                Context = ctx
            }
)

let many1 parser =
    let rec loop parser acc =
        (fun ctx ->
            match parser ctx with
            | Success success -> 
                loop parser (success.Value :: acc) success.Context
            | Failure failure -> 
                if failure.Context.Index <> ctx.Index then
                    Failure failure
                elif List.isEmpty acc then
                    Failure failure
                else
                    Success {
                        Value = List.rev acc
                        Context = failure.Context
                    }
        )
    loop parser []


let parseInt = 
    many1 (anyOf "1234567890")
    |>> (fun numbers ->
        int(String(Array.ofList(numbers)))
    )
```

---

# More helpers : some old friends

```fsharp
let (.>>) parserA parserB =
    (fun ctx ->
        match parserA ctx with
        | Success successA -> 
            match parserB successA.Context with
            | Success successB -> Success { Value = successA.Value; Context = successB.Context }
            | Failure failure -> Failure failure
        | Failure failure -> Failure failure
    )

let (.>>.) parserA parserB =
    (fun ctx ->
        match parserA ctx with
        | Success successA ->
            match parserB successA.Context with
            | Success successB -> Success { Value = (successA.Value, successB.Value); Context = successB.Context }
            | Failure failure -> Failure failure
        | Failure failure -> Failure failure
    )
```

---

# More helpers : recurring patterns

```fsharp
let between parserL parserR parser =
    parserL >>. parser .>> parserR

let sepBy1 parser parserSep =
    parser .>>. (many (parserSep >>. parser))
    |>> fun (x, xs) -> x :: xs
```

---

# Parsing a list of integers

```fsharp
"[1,2,3]"
|> run (between (str "[") (str "]") (sepBy1 parseInt (str ","))) 
// Success: [1; 2; 3]
```

---

# Someone ruins everything

```fsharp
"[1, 2, 3]"
|> run (between (str "[") (str "]") (sepBy1 parseInt (str ","))) 
// Failure: Expecting Any of these: '1234567890'
// [1, 2, 3]
//    ^
```

---

# Now it works

```fsharp
let ws = many (str " ")

"[ 1,2, 3 ]"
|> run (between (str "[" >>. ws) (str "]" >>. ws) (sepBy1 (parseInt .>> ws) (str "," >>. ws))) 
// Success: [1; 2; 3]
```

---

![](img/hard.jpg)

---

# TEST ... YOUR ... PARSERS

```fsharp
let eof =
    (fun ctx ->
        if ctx.Index = String.length ctx.Text - 1 then
            Success {
                Value = ()
                Context = ctx
            }
        else
            Failure {
                Expected = "eof"
                Context = ctx
            }
    )
```

---

# Trivial things to add

* Location instead of index, useful for files
* Where's my monad!??
* Separate expected from error message

---

# Warning: This isn't optimized

In the past, the relatively poor performance of parser combinator libraries has often been cited as the primary impediment to their more widespread adoption. For this reason optimal performance stood front and center as a design goal during the development of FParsec and a lot of effort has been spent on optimizing parsing speed. As a result, **FParsec has become so fast that parsers implemented with FParsec often significantly outperform parsers created by parser generator tools like fslex & fsyacc**.

---

# Performance guidelines

* Avoid backtracking
* Prefer specialized parsers
    * skip
    * builtin
* Construct parsers once
* Avoid `parse {...}` expressions
* Avoid `regex` parsers
* Consider optimizing large `choice` parsers

Source: https://www.quanttec.com/fparsec/users-guide/performance-optimizations.html

---

# Okay, I rambled long enough

Parser combinators are awesome

Success: awesome
