namespace FsHttp.Core.Types

open FParsec
open FsHttp.Core.Parsing

module private Parsing =

    let ppathabempty =
        skipMany (pstring "/" >>. psegment)
        |> skipped
        |> attempt

    let ppathabsolute =
        pstring "/" >>. optional (psegmentnz >>. skipMany (pstring "/" >>. psegment))
        |> skipped
        |> attempt

    let ppathrootless =
        many1Strings2 psegmentnz (pstring "/" >>. psegment |>> ((+) "/"))
        |>> ignore
        |> skipped
        |> attempt

    let ppathempty =
        optional (pstring "<" >>. ppchar .>> pstring ">")
        |> skipped
        |> attempt

    let phost =
        choice [
            pipliteral
            pipv4address
            pregname
        ]
        |>> ignore
        |> skipped
        |> attempt

    let pauthority =
        optional (puserinfo .>>? pstring "@")
        >>? phost
        >>? opt (pstring ":" >>. many1Chars digit)
        |>> ignore
        |> skipped
        |> attempt

    let phierpart =
        choice [
            pstring "//" >>. pauthority >>? ppathabempty
            ppathabsolute
            ppathrootless
            ppathempty
        ]
        |>> ignore
        |> skipped
        |> attempt

    let pabsolute =
        (pscheme .>> pstring ":")
        >>? phierpart
        >>? optional (pstring "?" >>. pquery)
        |>> ignore
        |> skipped
        |> attempt

type OriginTarget = {
    Path : string
    Query : string option
} with
    static member Parser =
        pipe2
            <| pabsolutepath
            <| opt (pstring "?" >>. pquery)
            <| fun path query -> {
                OriginTarget.Path = path
                Query = query
            }

/// The target resource upn which to apply the request.
/// See <see href="https://tools.ietf.org/html/rfc7230#section-5.3">RFC7230 Section 5.3</see>.
type RequestTarget =
    | Origin of OriginTarget
    | Absolute of string
    | Authority of string
    | Asterisk
with
    static member Parser =
        choice [
            attempt (pstring "*" >>% Asterisk)
            attempt OriginTarget.Parser |>> Origin
            attempt Parsing.pauthority |>> Authority
            attempt Parsing.pabsolute |>> Absolute
        ]
