module FsHttp.Core.Parsing.Message

open System
open FParsec
open FsHttp.Core.Parsing.General
open FsHttp.Core.Parsing.Uri
open FsHttp.Core.Types.Message

let private pmethod =
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

let private porigintarget =
        pipe2
            <| Uri.pabsolutepath
            <| opt (pstring "?" >>. Uri.pquery)
            <| fun path query -> {
                OriginTarget.Path = path
                Query = query
            }

let pabsolutetarget =
    pipe3
        <| (Uri.pscheme .>> pstring ":")
        <| Uri.phierpart
        <| opt (pstring "?" >>. Uri.pquery)
        <| fun scheme hierPart query ->
            {
                AbsoluteTarget.Scheme = scheme
                HierPart = hierPart
                Query = query
            }

let private prequesttarget =
    choice [
        attempt (pstring "*" >>% Asterisk)
        attempt porigintarget |>> Origin
        attempt pauthority |>> Authority
        attempt pabsolutetarget |>> Absolute
    ]

let private phttpversion =
    pipe2
        <| (pstring "HTTP/" >>. digit)
        <| (pstring "." >>. digit)
        <| fun major minor -> {
            HttpVersion.MajorVersion = System.Int32.Parse (string major)
            MinorVersion = System.Int32.Parse (string minor)
        }

let private prequestline =
    pipe3
        <| (pmethod .>> pchar ' ')
        <| (prequesttarget .>> pchar ' ')
        <| (phttpversion .>> newline)
        <| fun method target httpVersion -> {
            RequestLine.Method = method
            Target = target
            HttpVersion = httpVersion
        }

let private ptchar : Parser<char> =
    choice [
        pchar '!'
        pchar '#'
        pchar '$'
        pchar '%'
        pchar '&'
        pchar '''
        pchar '*'
        pchar '+'
        pchar '-'
        pchar '.'
        pchar '^'
        pchar '_'
        pchar '`'
        pchar '|'
        pchar '~'
        digit
        asciiLetter
    ]

module private Headers =
    let private pws : Parser<char> =
        anyOf [' '; '\t']

    let private pvchar : Parser<char> =
        satisfy (fun c -> int c >= 0x21 && int c <= 0x7e)

    let private pobstext : Parser<char> =
        satisfy (fun c -> int c >= 0x80 && int c <= 0xff)

    let private pobsfold : Parser<string> =
        newline >>. many1Chars pws

    let private ptoken =
        many1Chars ptchar

    let private pfieldvchar : Parser<char> =
        pvchar <|> pobstext

    let private pfieldcontent : Parser<string> =
        (pfieldvchar >>. optional ((many1Chars pws) >>. pfieldvchar))
        |>> ignore
        |> skipped
        |> attempt

    let private pfieldvalue : Parser<string> =
        manyStrings pfieldcontent //<|> pobsfold

    let private pows : Parser<string> =
        manyChars pws

    let pheaderfield : Parser<string * string> =
        (ptoken .>> pchar ':') .>>. (pows >>. pfieldvalue .>> pows)

let private pbody (headers : seq<string * string>) =
    let contentLength =
        headers
        |> Seq.toList
        |> List.where (fst >> (=) "Content-Length") |> List.map snd
    let transferEncoding =
        headers
        |> Seq.toArray
        |> Seq.where (fst >> (=) "Transfer-Encoding")
        |> Seq.collect (fun (_, s) -> s.Split(','))
        |> Seq.toList
    match (transferEncoding, contentLength) with
    | ([], []) ->
        preturn None
    | ([], contentLength) when contentLength.Length > 1 ->
        preturn (Some RequestBody.InvalidContentLength)
    | ([], [contentLength]) ->
        match Int32.TryParse contentLength with
        | (true, contentLength) ->
            parray contentLength anyChar
            |>> (Array.map byte >> RequestBody.ParsedBody >> Some)
        | (false, _) -> preturn (Some RequestBody.InvalidContentLength)
    | (transferEncoding, _) when transferEncoding |> List.tryLast = Some "chunked" ->
        // Read until transfer coding indicates the data is complete.
        fail "Transfer-Encoding not supported."
    | (_, _) ->
        // Transfer encoding which doesn't end in 'chunked', cannot determine length.
        preturn (Some RequestBody.UnknownLength)

let prequestmessage =
    prequestline .>>. (many (Headers.pheaderfield .>> newline)) .>> newline
    >>= (fun (requestLine, headers) ->
        pbody headers
        |>> fun body -> {
            RequestMessage.RequestLine = requestLine
            Headers = Map.ofList headers
            Body = body
        })
