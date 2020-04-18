[<AutoOpen>]
module Prelude

open System

module Result =

    let ofOption error = function
        | Some v -> Ok v
        | None -> Error error

    let ofOptionf errorfmt = function
        | Some v -> Ok v
        | None ->
            Printf.ksprintf Error errorfmt

    let zip x1 x2 =
        match (x1, x2) with
        | (Ok x1res, Ok x2res) -> Ok (x1res, x2res)
        | (Error e, _) -> Error e
        | (_, Error e) -> Error e

type ResultBuilder() =
    member __.Return (x) = Ok x

    member __.ReturnFrom (m : Result<_, _>) = m

    member __.Bind (m, f) = Result.bind f m
    member __.Bind ((m, error) : ('T option * 'E), f) = m |> Result.ofOption error |> Result.bind f

    member __.Zero () = None

    member __.Combine (m, f) = Result.bind f m

    member __.Delay (f : unit -> _) = f

    member __.Run (f) = f()

    member __.TryWith (m, h) =
        try __.ReturnFrom(m)
        with e -> h e

    member __.TryFinally (m, compensation) =
        try __.ReturnFrom (m)
        finally compensation()

    member __.Using (res : #IDisposable, body) =
        __.TryFinally(body res, fun () -> match res with null -> () | disp -> disp.Dispose())

    member __.While (guard, f) =
        if not (guard()) then Ok () else
        do f() |> ignore
        __.While(guard, f)

    member __.For (sequence : seq<_>, body) =
        __.Using(sequence.GetEnumerator(), fun enum -> __.While(enum.MoveNext, __.Delay(fun () -> body enum.Current)))

let result = ResultBuilder()

module FParsec =

    open FParsec

    type Parser<'T> = Parser<'T, unit>

    // For setting breakpoint when debugging parser.
    let bp (p : Parser<_, _>) stream =
        p stream

    /// Executes the provided parser up to `n` times. Fails if the parser returns more than `n` results.
    let pupto n p =
        many p
        >>=? fun x -> if x.Length <= n then preturn x else fail (sprintf "pupto expected %d" n)

    /// Runs the provided parser with the provided input string, returning a Result type.
    /// The Error case of the returned value will contains the provided error string, appended
    /// with details about the parse error.
    let runWithError (p : Parser<_, _>) (error : string) (input : string) =
        match run p input with
        | ParserResult.Success (result, _, _) -> Result.Ok result
        | ParserResult.Failure (errorDetails, _, _) -> Result.Error (sprintf "%s: %s" error errorDetails)


