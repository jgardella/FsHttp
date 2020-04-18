module FsHttp.Tests.Helpers

open Xunit
open FParsec.CharParsers

module Assert =

    let IsOk = function
        | Result.Ok _ -> Assert.True(true)
        | Result.Error error -> Assert.True(false, sprintf "Expected Ok, got Error '%O'" error)

    let OkEqual (expected : 'a, result : Result<'a, _>) =
        match result with
        | Result.Ok result -> Assert.Equal<'a>(expected, result)
        | Result.Error error -> Assert.True(false, sprintf "Expected Ok, got Error '%O'" error)

    let IsError = function
        | Result.Ok result -> Assert.True(false, sprintf "Expected Error, got Ok '%O'" result)
        | Result.Error _ -> Assert.True(true)

    let ErrorEqual (expected : 'a, result : Result<_, 'a>) =
        match result with
        | Result.Ok result -> Assert.True(false, sprintf "Expected error, got Ok '%O'" result)
        | Result.Error error -> Assert.Equal<'a>(expected, error)

    let ParseSuccessEqual (expected : 'a, actual : ParserResult<'a, _>) =
        match actual with
        | ParserResult.Success (result, _, _) -> Assert.Equal<'a>(expected, result)
        | ParserResult.Failure (err, _, _) -> Assert.True(false, sprintf "Parse failed, %s" err)
