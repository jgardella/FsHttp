module FsHttp.Tests.Types.Dto.Message

open System.Text
open FParsec
open Xunit
open FsHttp.Tests.Helpers
open FsHttp.Core.Types.Dto.Message

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
                Target = "/hello.txt?foo=bar"
                HttpVersion = { HttpVersion.MajorVersion = 1; MinorVersion = 1 }
            }
            Headers =
                [
                    ("User-Agent", "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3")
                    ("Host",  "www.example.com")
                    ("Accept-Language", "en, mi")
                ]
            Body = None
        }

    Assert.ParseSuccessEqual(expected, run RequestMessage.Parser input)

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
                Target = "/hello.txt"
                HttpVersion = { HttpVersion.MajorVersion = 1; MinorVersion = 1 }
            }
            Headers =
                [
                    ("User-Agent", "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3")
                    ("Host",  "www.example.com")
                    ("Accept-Language", "en, mi")
                    ("Content-Length", "26")
                ]
            Body = Some (Body.ParsedBody (Encoding.UTF8.GetBytes "abcdefghijklmnopqrstuvwxyz"))
        }

    Assert.ParseSuccessEqual(expected, run RequestMessage.Parser input)

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
                Target = "/hello.txt"
                HttpVersion = { HttpVersion.MajorVersion = 1; MinorVersion = 1 }
            }
            Headers =
                [
                    ("User-Agent", "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3")
                    ("Host",  "www.example.com")
                    ("Accept-Language", "en, mi")
                    ("Content-Length", "blah")
                ]
            Body = Some Body.InvalidContentLength
        }

    Assert.ParseSuccessEqual(expected, run RequestMessage.Parser input)

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
                Target = "/hello.txt"
                HttpVersion = { HttpVersion.MajorVersion = 1; MinorVersion = 1 }
            }
            Headers =
                [
                    ("User-Agent", "curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3")
                    ("Host",  "www.example.com")
                    ("Accept-Language", "en, mi")
                    ("Content-Length", "26")
                    ("Content-Length", "27")
                ]
            Body = Some Body.InvalidContentLength
        }

    Assert.ParseSuccessEqual(expected, run RequestMessage.Parser input)