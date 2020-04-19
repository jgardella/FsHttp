module FsHttp.Tests.Types.Message

open System
open Xunit
open FsHttp.Core.Types
open FsHttp.Core.Types.Dto.Message
open FsHttp.Core.Types.Message
open FsHttp.Tests.Helpers

module RequestTarget =

    [<Fact>]
    let ``successfully parses valid absolute request target`` () =
        let input = "http://example.com/hello.txt?foo=bar"
        let expected = RequestTarget.Absolute "http://example.com/hello.txt?foo=bar"
        Assert.OkEqual(expected, Message.tryGetRequestTarget input)

    [<Fact>]
    let ``successfully parses valid asterisk request target`` () =
        let input = "*"
        let expected = RequestTarget.Asterisk
        Assert.OkEqual(expected, Message.tryGetRequestTarget input)

    [<Fact>]
    let ``successfully parses valid authority request target`` () =
        let input = "www.example.com:80"
        let expected = RequestTarget.Authority "www.example.com:80"
        Assert.OkEqual(expected, Message.tryGetRequestTarget input)

    [<Fact>]
    let ``successfully parses valid origin request target`` () =
        let input = "/home?foo=bar"
        let expected = RequestTarget.Origin { OriginTarget.Path = "/home"; Query = Some "foo=bar"}
        Assert.OkEqual(expected, Message.tryGetRequestTarget input)

module EffectiveRequestUri =

    [<Fact>]
    let ``creates correct URI from absolute target`` () =
        let target = RequestTarget.Absolute "http://www.website.com:80/blah?foo=bar"
        let expected = UriBuilder("http", "www.website.com", 80, "/blah", "?foo=bar").Uri
        Assert.Equal(expected, Message.getEffectiveRequestUri (Some "https") false (Some "www.example.com:80") "www.blah.com" None 80 target)

    [<Fact>]
    let ``creates correct URI from authority target`` () =
        let target = RequestTarget.Authority "www.website.com"
        let expected = UriBuilder("http", "www.website.com").Uri
        Assert.Equal(expected, Message.getEffectiveRequestUri None false None "www.blah.com" (Some (Header.Host "www.blah.com")) 80 target)

    [<Fact>]
    let ``creates correct URI from origin target`` () =
        let target = RequestTarget.Origin { OriginTarget.Path = "/home"; Query = Some "foo=bar" }
        let expected = UriBuilder("http", "www.website.com", 80, "/home", "?foo=bar").Uri
        Assert.Equal(expected, Message.getEffectiveRequestUri None false None "www.website.com" None 80 target)

    [<Fact>]
    let ``creates correct URI from fixed authority and scheme`` () =
        let target = RequestTarget.Asterisk
        let fixedAuthority = "www.website.com"
        let expected = UriBuilder("https", "www.website.com").Uri
        Assert.Equal(expected, Message.getEffectiveRequestUri (Some "https") false (Some fixedAuthority) "www.blah.com" (Some (Header.Host "www.blah.com:80")) 80 target)

    [<Fact>]
    let ``creates correct URI from default authority`` () =
        let target = RequestTarget.Asterisk
        let expected = UriBuilder("http", "www.website.com").Uri
        Assert.Equal(expected, Message.getEffectiveRequestUri None false None "www.website.com" None 80 target)

    [<Fact>]
    let ``creates correct URI from asterisk target using Host header`` () =
        let target = RequestTarget.Asterisk
        let host = Header.Host "www.website.com:80"
        let expected = UriBuilder("http", "www.website.com", 80).Uri
        Assert.Equal(expected, Message.getEffectiveRequestUri (Some "http") false None "www.blah.com" (Some host) 80 target)

    [<Fact>]
    let ``creates correct URI from default authority with non-default port`` () =
        let target = RequestTarget.Asterisk
        let expected = UriBuilder("http", "www.website.com", 8080).Uri
        Assert.Equal(expected, Message.getEffectiveRequestUri (Some "http") false None "www.website.com" None 8080 target)

module RequestMessage =

    [<Fact>]
    let ``parses valid DTO to request message`` () =
        let dto = {
            Dto.Message.RequestMessage.RequestLine =
                {
                    RequestLine.Method = RequestMethod.GET
                    Target = "http://www.example.com:80"
                    HttpVersion = { HttpVersion.MajorVersion = 1; MinorVersion = 1 }
                }
            Dto.Message.Headers =
                [
                    ("Host", "www.example.com")
                    ("Connection", "close")
                ]
            Dto.Message.RequestMessage.Body = None
        }
        let serverConfig =
            {
                ServerConfig.DefaultAuthority = "www.blah.com"
                FixedScheme = None
                FixedAuthority = None
            }
        let expected = {
            RequestMessage.Host = Some (Header.Host "www.example.com")
            RequestTarget = RequestTarget.Absolute "http://www.example.com:80"
            EffectiveRequestUri = UriBuilder("http", "www.example.com", 80).Uri
            Connection = Some Header.Connection.Close
        }
        let result = Message.RequestMessage.TryOfDto serverConfig false 80 dto
        Assert.OkEqual(expected, result)
