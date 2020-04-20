module FsHttp.Core.Types.Dto.Message

open System
open System.Text
open FParsec
open FsHttp.Core.Parsing

/// An HTTP Method.
/// See <see href="https://tools.ietf.org/html/rfc7231#section-4">RFC7231 Section 4</see>.
type RequestMethod =
    | GET
    | HEAD
    | POST
    | PUT
    | DELETE
    | CONNECT
    | OPTIONS
    | TRACE
with
    static member Parser =
        choice [
            pstring "GET" >>% GET
            pstring "HEAD" >>% HEAD
            pstring "POST" >>% POST
            pstring "PUT" >>% PUT
            pstring "DELETE" >>% DELETE
            pstring "CONNECT" >>% CONNECT
            pstring "OPTIONS" >>% OPTIONS
            pstring "TRACE" >>% TRACE
        ]

type HttpVersion = {
    MajorVersion : int
    MinorVersion : int
} with
    static member Parser =
        pipe2
            <| (pstring "HTTP/" >>. digit)
            <| (pstring "." >>. digit)
            <| fun major minor -> {
                HttpVersion.MajorVersion = System.Int32.Parse (string major)
                MinorVersion = System.Int32.Parse (string minor)
            }


type RequestLine = {
    Method : RequestMethod
    Target : string
    HttpVersion : HttpVersion
} with
    static member Parser =
        pipe3
            <| (RequestMethod.Parser .>> pchar ' ')
            <| CharParsers.charsTillString " " true 2000
            <| (HttpVersion.Parser .>> newline)
            <| fun method target httpVersion -> {
                RequestLine.Method = method
                Target = target
                HttpVersion = httpVersion
            }

type StatusLine = {
    StatusCode : int
    ReasonPhrase : string
    HttpVersion : HttpVersion
}

type Body =
    | ParsedBody of byte []
    | UnknownLength
    | InvalidContentLength

module private Headers =

    let private pvchar : Parser<char> =
        satisfy (fun c -> int c >= 0x21 && int c <= 0x7e)

    let private pobstext : Parser<char> =
        satisfy (fun c -> int c >= 0x80 && int c <= 0xff)

    let private pobsfold : Parser<string> =
        newline >>. many1Chars pws

    let private pfieldvchar : Parser<char> =
        pvchar <|> pobstext

    let private pfieldcontent : Parser<string> =
        (pfieldvchar >>. optional ((many1Chars pws) >>. pfieldvchar))
        |>> ignore
        |> skipped
        |> attempt

    let private pfieldvalue : Parser<string> =
        manyStrings pfieldcontent //<|> pobsfold

    let pheaderfield : Parser<string * string> =
        (ptoken .>> pchar ':') .>>. (pows >>. pfieldvalue .>> pows)

let pbody (headers : seq<string * string>) =
    let contentLength =
        headers
        |> Seq.toList
        |> List.where (fst >> (=?) "Content-Length") |> List.map snd
    let transferEncoding =
        headers
        |> Seq.toArray
        |> Seq.where (fst >> (=?) "Transfer-Encoding")
        |> Seq.collect (fun (_, s) -> s.Split(','))
        |> Seq.toList
    match (transferEncoding, contentLength) with
    | ([], []) ->
        preturn None
    | ([], contentLength) when contentLength.Length > 1 ->
        preturn (Some Body.InvalidContentLength)
    | ([], [contentLength]) ->
        match Int32.TryParse contentLength with
        | (true, contentLength) ->
            parray contentLength anyChar
            |>> (Array.map byte >> Body.ParsedBody >> Some)
        | (false, _) -> preturn (Some Body.InvalidContentLength)
    | (transferEncoding, _) when transferEncoding |> List.tryLast = Some "chunked" ->
        // Read until transfer coding indicates the data is complete.
        fail "Transfer-Encoding not supported."
    | (_, _) ->
        // Transfer encoding which doesn't end in 'chunked', cannot determine length.
        preturn (Some Body.UnknownLength)

type RequestMessage = {
    RequestLine : RequestLine
    Headers : seq<string * string>
    Body : Body option
} with
    static member Parser =
        RequestLine.Parser .>>. (many (Headers.pheaderfield .>> newline)) .>> newline
        >>= (fun (requestLine, headers) ->
            pbody headers
            |>> fun body -> {
                RequestMessage.RequestLine = requestLine
                Headers = headers
                Body = body
            })

type ResponseMessage = {
    StatusLine : StatusLine
    Headers : seq<string * string>
    Body : byte [] option
} with
    static member Bytes (response : ResponseMessage) =
        let httpVersion = sprintf "HTTP/%d.%d" response.StatusLine.HttpVersion.MajorVersion response.StatusLine.HttpVersion.MinorVersion
        let headers = response.Headers |> Seq.map (fun (header, field) -> sprintf "%s: %s\r\n" header field) |> String.concat ""
        let messageWithHeaders =
            sprintf "%s %d %s\r\n%s\r\n"
                httpVersion
                response.StatusLine.StatusCode
                response.StatusLine.ReasonPhrase
                headers
        Array.append (Option.defaultValue [||] response.Body) (Encoding.UTF8.GetBytes messageWithHeaders)
