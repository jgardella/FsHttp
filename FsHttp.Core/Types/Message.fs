module FsHttp.Core.Types.Message

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

type OriginTarget = {
    Path : string
    Query : string option
}

type AbsoluteTarget = {
    Scheme : string
    HierPart : string
    Query : string option
}

/// The target resource upn which to apply the request.
/// See <see href="https://tools.ietf.org/html/rfc7230#section-5.3">RFC7230 Section 5.3</see>.
type RequestTarget =
    | Origin of OriginTarget
    | Absolute of AbsoluteTarget
    | Authority of Uri.Authority
    | Asterisk

type HttpVersion = {
    MajorVersion : int
    MinorVersion : int
}

type RequestLine = {
    Method : RequestMethod
    Target : RequestTarget
    HttpVersion : HttpVersion
}

type StatusLine = {
    StatusCode : int
    ReasonPhrase : string
}

type RequestBody =
    | ParsedBody of byte []
    | UnknownLength
    | InvalidContentLength

type RequestMessage = {
    RequestLine : RequestLine
    Headers : Map<string, string>
    Body : RequestBody option
}
