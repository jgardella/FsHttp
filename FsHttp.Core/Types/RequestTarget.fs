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

    let phierpart =
        choice [
            pstring "//" >>. Authority.Parser >>? ppathabempty
            ppathabsolute
            ppathrootless
            ppathempty
        ]
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

type AbsoluteTarget = {
    Scheme : string
    HierPart : string
    Query : string option
} with
    static member Parser =
        pipe3
            <| (pscheme .>> pstring ":")
            <| Parsing.phierpart
            <| opt (pstring "?" >>. pquery)
            <| fun scheme hierPart query ->
                {
                    AbsoluteTarget.Scheme = scheme
                    HierPart = hierPart
                    Query = query
                }

/// The target resource upn which to apply the request.
/// See <see href="https://tools.ietf.org/html/rfc7230#section-5.3">RFC7230 Section 5.3</see>.
type RequestTarget =
    | Origin of OriginTarget
    | Absolute of AbsoluteTarget
    | Authority of Authority
    | Asterisk
with
    static member Parser =
        choice [
            attempt (pstring "*" >>% Asterisk)
            attempt OriginTarget.Parser |>> Origin
            attempt Authority.Parser |>> Authority
            attempt AbsoluteTarget.Parser |>> Absolute
        ]
