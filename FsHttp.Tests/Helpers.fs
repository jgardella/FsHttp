module FsHttp.Tests.Helpers

open Xunit
open FParsec.CharParsers

module Assert =

    let ParseSuccess (expected : 'a, actual : ParserResult<'a, _>) =
        match actual with
        | ParserResult.Success (result, _, _) -> Assert.Equal<'a>(expected, result)
        | ParserResult.Failure (err, _, _) -> Assert.True(false, sprintf "Parse failed, %s" err)


