module FsHttp.Tests.Types.Message

open Xunit
open FParsec
open FsHttp.Core.Types
open FsHttp.Tests.Helpers

module Authority =

    [<Fact>]
    let ``successfully parses authority`` () =
        let input = "user@www.example.com:80"
        let expected = { Authority.Host = Host.RegName "www.example.com"; UserInfo = Some "user"; Port = Some 80 }
        Assert.ParseSuccess(expected, run Authority.Parser input)

    [<Fact>]
    let ``successfully parses authority with just host`` () =
        let input = "www.example.com"
        let expected = { Authority.Host = Host.RegName "www.example.com"; UserInfo = None; Port = None }
        Assert.ParseSuccess(expected, run Authority.Parser input)

    [<Fact>]
    let ``successfully parses authority with port`` () =
        let input = "www.example.com:80"
        let expected = { Authority.Host = Host.RegName "www.example.com"; UserInfo = None; Port = Some 80 }
        Assert.ParseSuccess(expected, run Authority.Parser input)

    [<Fact>]
    let ``successfully parses authority with user info`` () =
        let input = "user@www.example.com"
        let expected = { Authority.Host = Host.RegName "www.example.com"; UserInfo = Some "user"; Port = None }
        Assert.ParseSuccess(expected, run Authority.Parser input)

module RequestTarget =

    [<Fact>]
    let ``successfully parses valid absolute target`` () =
        let input = "http://example.com/hello.txt?foo=bar"
        let expected = RequestTarget.Absolute { AbsoluteTarget.Scheme = "http"; HierPart = "//example.com/hello.txt"; Query = Some "foo=bar" }
        Assert.ParseSuccess(expected, run RequestTarget.Parser input)

    [<Fact>]
    let ``successfully parses valid asterisk GET request`` () =
        let input = "*"
        let expected = RequestTarget.Asterisk
        Assert.ParseSuccess(expected, run RequestTarget.Parser input)

    [<Fact>]
    let ``successfully parses valid authority GET request`` () =
        let input = "www.example.com:80"
        let expected = RequestTarget.Authority { Authority.UserInfo = None; Host = RegName "www.example.com"; Port = Some 80 }
        Assert.ParseSuccess(expected, run RequestTarget.Parser input)
