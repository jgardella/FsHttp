module FsHttp.Tests.Parsing.Uri

open Xunit
open FParsec
open FsHttp.Core.Parsing
open FsHttp.Core.Types.Uri
open FsHttp.Tests.Helpers

module Authority =

    [<Fact>]
    let ``successfully parses authority`` () =
        let input = "user@www.example.com:80"
        let expected = { Authority.Host = "www.example.com"; UserInfo = Some "user"; Port = Some 80 }
        Assert.ParseSuccess(expected, run Uri.pauthority input)

    [<Fact>]
    let ``successfully parses authority with just host`` () =
        let input = "www.example.com"
        let expected = { Authority.Host = "www.example.com"; UserInfo = None; Port = None }
        Assert.ParseSuccess(expected, run Uri.pauthority input)

    [<Fact>]
    let ``successfully parses authority with port`` () =
        let input = "www.example.com:80"
        let expected = { Authority.Host = "www.example.com"; UserInfo = None; Port = Some 80 }
        Assert.ParseSuccess(expected, run Uri.pauthority input)

    [<Fact>]
    let ``successfully parses authority with user info`` () =
        let input = "user@www.example.com"
        let expected = { Authority.Host = "www.example.com"; UserInfo = Some "user"; Port = None }
        Assert.ParseSuccess(expected, run Uri.pauthority input)
