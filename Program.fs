open System

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

let str (expected: string) =
    (fun ctx ->
        let length = String.length expected
        if ctx.Index + length > String.length ctx.Text then
            Failure {
                Expected = "eof"
                Context = ctx
            }
        elif ctx.Text.Substring(ctx.Index, length) = expected then
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

let run init (p: Parser<'t, 'state>) input =
    let ctx = { State = init; Text = input; Index = 0 }
    match p ctx with
    | Success success -> success.Value
    | Failure failure -> failwithf "Parser failed: expecting '%s'. From '%s'" failure.Expected failure.Context.Text.[failure.Context.Index..]

let (>>.) (pa: Parser<'a, 's>) (pb: Parser<'b, 's>) : Parser<'b, 's> =
    (fun ctx ->
        let a = pa ctx
        match a with
        | Success success -> pb success.Context
        | Failure failure -> Failure failure
    )

let (.>>) (pa: Parser<'a, 's>) (pb: Parser<'b, 's>) : Parser<'a, 's> =
    (fun ctx ->
        match pa ctx with
        | Success asuccess -> 
            match pb asuccess.Context with
            | Success bsuccess -> Success { Value = asuccess.Value; Context = bsuccess.Context }
            | Failure failure -> Failure failure
        | Failure failure -> Failure failure
    )

let (.>>.) pa pb =
    (fun ctx ->
        match pa ctx with
        | Success a ->
            match pb a.Context with
            | Success b -> Success { Value = (a.Value, b.Value); Context = b.Context }
            | Failure failure -> Failure failure
        | Failure failure -> Failure failure
    )

let anyOf (chars: seq<Char>) =
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

let many (p: Parser<'a, 's>) : Parser<'a list, 's> =
    let rec loop (p: Parser<'a, 's>) (acc: 'a list) : Parser<'a list, 's> =
        (fun ctx ->
            match p ctx with
            | Success success -> loop p (success.Value :: acc) success.Context
            | Failure failure -> 
                if failure.Context.Index <> ctx.Index then
                    Failure failure
                else
                    Success {
                        Value = List.rev acc
                        Context = failure.Context
                    }
        )
    loop p []

let (|>>) (p: Parser<'a, 's>) (f: 'a -> 'b) : Parser<'b, 's> = 
    (fun ctx ->
        match p ctx with
        | Success success ->
            Success {
                Value = f success.Value
                Context = success.Context
            }
        | Failure failure -> Failure failure
    )

let pInt =
    many (anyOf "1234567890")
    |>> (fun numbers ->
        int(String(Array.ofList(numbers)))
    )

let sepBy pElement pSep =
    pElement .>>. (many (pSep >>. pElement))
    |>> fun (x, xs) -> x :: xs

let spaces = many (str " ")

let (<|>) (pa: Parser<'a, 's>) (pb: Parser<'a, 's>) : Parser<'a, 's> =
    (fun ctx ->
        match pa ctx with
        | Failure a ->
            if a.Context.Index <> ctx.Index then
                Failure a
            else
                match pb ctx with
                | Failure b ->
                    Failure {
                        Expected = sprintf "Any of these: '%O' or '%O'" a.Expected b.Expected
                        Context = b.Context
                    }
                | b -> b
        | a -> a
    )

let attempt (p: Parser<'a, 's>) : Parser<'a, 's> =
    (fun ctx ->
        match p ctx with
        | Failure _ -> Failure { Expected = ""; Context = ctx }
        | Success success -> Success success
    )

let (<?>) (p: Parser<'a, 's>) s : Parser<'a, 's> =
    (fun ctx ->
        match p ctx with
        | Success success -> Success success
        | Failure failure ->
            Failure { failure with Expected = s }
    )

let getState : Parser<'s, 's> =
    (fun ctx ->
        Success { 
            Value = ctx.State
            Context = ctx
        }
    )

let setState (state: 's) : Parser<unit, 's> =
    (fun ctx ->
        Success {
            Value = ()
            Context = { ctx with State = state }
        }
    )

let updateState (f: 's -> 's) : Parser<unit, 's> =
    (fun ctx ->
        Success {
            Value = ()
            Context = { ctx with State = f ctx.State }
        }
    )

let stateSatisfies (f: 's -> bool) : Parser<unit, 's> =
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

let eof : Parser<unit, 'u> =
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

let parseParens =
    (str "(" >>. updateState addParen)
    <|> (str ")" >>. updateState remParen)
    <|> eof

let isEmpty = function
    | Empty -> true
    | Paren 0 -> true
    | _ -> false

module Test =
    open Xunit

    [<Fact>]
    let ``Parse only ab`` () =
        let ab = str "a" >>. str "b"
        let ac = str "a" >>. str "c"
        Assert.StrictEqual("ac", run () (ab <|> ac) "ac")

[<EntryPoint>]
let main argv =
    let input = "cow says moo"

    printfn "%O" (run () (str "cow") "cow")
    printfn "%O" (run () (str "cow" >>. str " " >>. str "says" >>. str " " >>. str "moo") input)

    printfn "%O" (run () (str "[" >>. (sepBy (pInt .>> spaces) (str "," .>> spaces)) .>> str "]") "[12,23 ,34 ]")

    printfn "%O" (run () (str "[" >>. (sepBy (str "a" <|> str "b") (str ",")) .>> str "]") "[a,b]")

    let ab = str "a" >>. str "b"
    let ac = str "a" >>. str "c"
    printfn "%O" (run () (attempt ab <|> ac) "ac")

    let ab = str "a" >>. str "b" <?> "In the middle of doing a then b"
    let ac = str "a" >>. str "c" <?> "In the middle of doing a then c"
    printfn "%O" (run () (ab <|> ac) "ab")

    printfn "%O" (run Empty ((many parseParens) >>. stateSatisfies isEmpty) "((())))(()((()()()()())())()")

    0 // return an integer exit code
