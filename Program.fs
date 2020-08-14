module Parsec 

open System

module NoState =
    type Context = {
        Text: string
        Index: int
    }

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

    type Parser<'value> = (Context -> Result<'value>)

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

    let run parser text =
        let ctx = { Text = text; Index = 0 }
        parser ctx 

    let test1 () =
        run (str "parsec") "parsec"
        |> printfn "%O"

    let test2 () =
        run (str "parser combinators are awesome") "parser combinators are awesome"
        |> printfn "%O"

    let test3 () =
        run (str "parser combinators are awesome") "parser combinator are awesome"
        |> printfn "%O"

    let (>>.) parserA parserB =
        (fun ctx ->
            match parserA ctx with
            | Success success -> parserB success.Context
            | Failure failure -> Failure failure
        )

    let test4 () =
        let parser = str "parser"
        let space = str " "
        let combinators = str "combinators"
        let are = str "are"
        let awesome = str "awesome"

        let together =
            (parser >>. space >>. combinators >>. space >>. are >>. space >>. awesome)

        run together "parser combinators are awesome"
        |> printfn "%O"

        run together "parser combinator are awesome"
        |> printfn "%O"

    // let (.>>) parserA parserB =
    //     (fun ctx ->
    //         match parserA ctx with
    //         | Success successA -> 
    //             match parserB successA.Context with
    //             | Success successB -> Success { Value = successA.Value; Context = successB.Context }
    //             | Failure failure -> Failure failure
    //         | Failure failure -> Failure failure
    //     )

    // let (.>>.) parserA parserB =
    //     (fun ctx ->
    //         match parserA ctx with
    //         | Success successA ->
    //             match parserB successA.Context with
    //             | Success successB -> Success { Value = (successA.Value, successB.Value); Context = successB.Context }
    //             | Failure failure -> Failure failure
    //         | Failure failure -> Failure failure
    //     )

    let test5 () =
        run (str "a") "a"
        |> printfn "%O"

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

    let test6 () =
        run (str "a" <|> str "t" <|> str "c" <|> str "g") "g"
        |> printfn "%O"
    
    type Molecule =
        | A
        | T
        | C
        | G

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

    let a = str "a" |>> fun _ -> A
    let t = str "t" |>> fun _ -> T
    let c = str "c" |>> fun _ -> C
    let g = str "g" |>> fun _ -> G
    let molecule = a <|> t <|> c <|> g

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

    let test7 () =
        run (many molecule) "agtgcgttac"
        |> printfn "%O"

    type EyeColor =
        | Blue
        | Brown

    let attempt parser =
        (fun ctx ->
            match parser ctx with
            | Failure _ -> Failure { Expected = ""; Context = ctx }
            | Success success -> Success success
        )

    let (<?>) parser expected =
        (fun ctx ->
            match parser ctx with
            | Success success -> Success success
            | Failure failure ->
                Failure { failure with Expected = expected }
        )


    let test8 () =
        let blue = t >>. a >>. a >>. g >>. t >>. g |>> fun _ -> Blue
        let brown = t >>. a >>. a >>. a >>. t >>. g |>> fun _ -> Brown
        run (blue <|> brown) "taagtg"
        |> printfn "%O"

        run (blue <|> brown) "taaatg"
        |> printfn "%O"

        run (attempt blue <|> brown) "taaatg"
        |> printfn "%O"

        let blue = blue <?> "In the middle of parsing Blue"
        let brown = brown <?> "In the middle of parsing Blue"
        run (blue <|> brown) "taaatg"
        |> printfn "%O"

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

    let (.>>) parserA parserB =
        (fun ctx ->
            match parserA ctx with
            | Success successA -> 
                match parserB successA.Context with
                | Success successB -> Success { Value = successA.Value; Context = successB.Context }
                | Failure failure -> Failure failure
            | Failure failure -> Failure failure
        )

    let between parserL parserR parser =
        parserL >>. parser .>> parserR
    
    let (.>>.) parserA parserB =
        (fun ctx ->
            match parserA ctx with
            | Success successA ->
                match parserB successA.Context with
                | Success successB -> Success { Value = (successA.Value, successB.Value); Context = successB.Context }
                | Failure failure -> Failure failure
            | Failure failure -> Failure failure
        )

    let sepBy parser parserSep =
        parser .>>. (many (parserSep >>. parser))
        |>> fun (x, xs) -> x :: xs

    let ws = many (str " ")

    let test9 () =
        run (between (str "[") (str "]") (sepBy parseInt (str ","))) "[1,2,3]"
        |> printfn "%O"

        run (between (str "[") (str "]") (sepBy parseInt (str ","))) "[1, 2, 3]"
        |> printfn "%O"

        run (between (str "[" >>. ws) (str "]" >>. ws) (sepBy (parseInt .>> ws) (str "," >>. ws))) "[ 1,2, 3 ]"
        |> printfn "%O"

module State =
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

    let run init parser text =
        let ctx = { State = init; Text = text; Index = 0 }
        parser ctx 

    let (>>.) parserA parserB =
        (fun ctx ->
            match parserA ctx with
            | Success success -> parserB success.Context
            | Failure failure -> Failure failure
        )

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

    let isEmpty = function
        | Empty -> true
        | Paren 0 -> true
        | _ -> false

    let test1 () =
        run Empty (many parseParens >>. getState) "((()))()())))))((("
        |> printfn "%O"

        run Empty (many parseParens >>. stateSatisfies isEmpty) "((()))()())))))((("
        |> printfn "%O"

        run Empty (many parseParens >>. stateSatisfies isEmpty) "((()))()())))))((((("
        |> printfn "%O"
    
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


[<EntryPoint>]
let main argv =
    NoState.test1 ()
    NoState.test2 ()
    NoState.test3 ()
    NoState.test4 ()
    NoState.test5 ()
    NoState.test6 ()
    NoState.test7 ()
    NoState.test8 ()

    State.test1 ()

    NoState.test9 ()

    0 // return an integer exit code
