module FsHttp.Tests.Parsing.Message

open System.Text
open Xunit
open FParsec
open FsHttp.Core.Parsing
open FsHttp.Core.Types.Message
open FsHttp.Core.Types.Uri
open FsHttp.Tests.Helpers

module AbsoluteTarget =
    [<Fact>]
    let ``successfully parses valid absolute target`` () =
        let input = "http://example.com/hello.txt?foo=bar"
        let expected = { AbsoluteTarget.Scheme = "http"; HierPart = "//example.com/hello.txt"; Query = Some "foo=bar" }
        Assert.ParseSuccess(expected, run Message.pabsolutetarget input)

[<Fact>]
let ``successfully parses valid GET request`` () =
    let input = """GET /hello.txt?foo=bar HTTP/1.1
User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3
Host: www.example.com
Accept-Language: en, mi

"""

    let expected =
        {
            RequestMessage.RequestLine = {
                RequestLine.Method = RequestMethod.GET
                Target = RequestTarget.Origin { OriginTarget.Path = "/hello.txt"; Query = Some "foo=bar" }
                HttpVersion = { HttpVersion.MajorVersion = 1; MinorVersion = 1 }
            }
            Headers =
                Map.ofList [
                    ("User-Agent", "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3")
                    ("Host",  "www.example.com")
                    ("Accept-Language", "en, mi")
                ]
            Body = None
        }

    Assert.ParseSuccess(expected, run Message.prequestmessage input)

[<Fact>]
let ``successfully parses valid absolute GET request`` () =
    let input = """GET http://example.com/hello.txt?foo=bar HTTP/1.1

"""

    let expected =
        {
            RequestMessage.RequestLine = {
                RequestLine.Method = RequestMethod.GET
                Target = RequestTarget.Absolute { AbsoluteTarget.Scheme = "http"; HierPart = "//example.com/hello.txt"; Query = Some "foo=bar" }
                HttpVersion = { HttpVersion.MajorVersion = 1; MinorVersion = 1 }
            }
            Headers = Map.empty
            Body = None
        }

    Assert.ParseSuccess(expected, run Message.prequestmessage input)

[<Fact>]
let ``successfully parses valid asterisk GET request`` () =
    let input = """GET * HTTP/1.1

"""

    let expected =
        {
            RequestMessage.RequestLine = {
                RequestLine.Method = RequestMethod.GET
                Target = RequestTarget.Asterisk
                HttpVersion = { HttpVersion.MajorVersion = 1; MinorVersion = 1 }
            }
            Headers = Map.empty
            Body = None
        }

    Assert.ParseSuccess(expected, run Message.prequestmessage input)

[<Fact>]
let ``successfully parses valid authority GET request`` () =
    let input = """GET www.example.com:80 HTTP/1.1

"""

    let expected =
        {
            RequestMessage.RequestLine = {
                RequestLine.Method = RequestMethod.GET
                Target = RequestTarget.Authority { Authority.UserInfo = None; Host = "www.example.com"; Port = Some 80 }
                HttpVersion = { HttpVersion.MajorVersion = 1; MinorVersion = 1 }
            }
            Headers = Map.empty
            Body = None
        }

    Assert.ParseSuccess(expected, run Message.prequestmessage input)

[<Fact>]
let ``successfully parses valid POST request`` () =
    let input = """POST /hello.txt HTTP/1.1
User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3
Host: www.example.com
Accept-Language: en, mi
Content-Length: 26

abcdefghijklmnopqrstuvwxyz"""

    let expected =
        {
            RequestMessage.RequestLine = {
                RequestLine.Method = RequestMethod.POST
                Target = RequestTarget.Origin { OriginTarget.Path = "/hello.txt"; Query = None }
                HttpVersion = { HttpVersion.MajorVersion = 1; MinorVersion = 1 }
            }
            Headers =
                Map.ofList [
                    ("User-Agent", "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3")
                    ("Host",  "www.example.com")
                    ("Accept-Language", "en, mi")
                    ("Content-Length", "26")
                ]
            Body = Some (RequestBody.ParsedBody (Encoding.UTF8.GetBytes "abcdefghijklmnopqrstuvwxyz"))
        }

    Assert.ParseSuccess(expected, run Message.prequestmessage input)

[<Fact>]
let ``successfully detects invalid Content-Length header`` () =
    let input = """POST /hello.txt HTTP/1.1
User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3
Host: www.example.com
Accept-Language: en, mi
Content-Length: blah

abcdefghijklmnopqrstuvwxyz"""

    let expected =
        {
            RequestMessage.RequestLine = {
                RequestLine.Method = RequestMethod.POST
                Target = RequestTarget.Origin { OriginTarget.Path = "/hello.txt"; Query = None }
                HttpVersion = { HttpVersion.MajorVersion = 1; MinorVersion = 1 }
            }
            Headers =
                Map.ofList [
                    ("User-Agent", "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3")
                    ("Host",  "www.example.com")
                    ("Accept-Language", "en, mi")
                    ("Content-Length", "blah")
                ]
            Body = Some RequestBody.InvalidContentLength
        }

    Assert.ParseSuccess(expected, run Message.prequestmessage input)

[<Fact>]
let ``successfully detects multiple Content-Length headers`` () =
    let input = """POST /hello.txt HTTP/1.1
User-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3
Host: www.example.com
Accept-Language: en, mi
Content-Length: 26
Content-Length: 27

abcdefghijklmnopqrstuvwxyz"""

    let expected =
        {
            RequestMessage.RequestLine = {
                RequestLine.Method = RequestMethod.POST
                Target = RequestTarget.Origin { OriginTarget.Path = "/hello.txt"; Query = None }
                HttpVersion = { HttpVersion.MajorVersion = 1; MinorVersion = 1 }
            }
            Headers =
                Map.ofList [
                    ("User-Agent", "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3")
                    ("Host",  "www.example.com")
                    ("Accept-Language", "en, mi")
                    ("Content-Length", "26")
                    ("Content-Length", "27")
                ]
            Body = Some RequestBody.InvalidContentLength
        }

    Assert.ParseSuccess(expected, run Message.prequestmessage input)
