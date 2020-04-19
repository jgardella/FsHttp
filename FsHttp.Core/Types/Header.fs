[<RequireQualifiedAccess>]
module FsHttp.Core.Types.Header

open FParsec
open FsHttp.Core

/// Tries to get exactly one header field with the provided header.
let private tryExactlyOneHeader (targetHeader : string) (headers : seq<string * string>) =
    headers
    |> Seq.where (fst >> ((=?) targetHeader))
    |> Seq.tryExactlyOne
    |> Option.map snd

/// Tries to get one header field with the provided header.
let private tryOneOrNoHeader (targetHeader : string) (headers : seq<string * string>) =
    match headers |> Seq.where (fst >> ((=?) targetHeader)) |> Seq.toList with
    | [(_, value)] -> Result.Ok (Some value)
    | [] -> Result.Ok None
    | _ -> Result.Error (sprintf "Multiple '%s' headers" targetHeader)

type Host = Host of string
with
    static member Parser =
        Parsing.phost >>? opt (pchar ':' >>? manyChars digit)
        |>> ignore
        |> skipped
        |> attempt
        |>> (fun s -> if s = "" then None else Some (Host s))

/// There must be exactly one 'Host' header with a properly formatted value.
/// See <see href="https://tools.ietf.org/html/rfc7230#section-5.4">RFC 7230 Section 5.4</see>.
let internal tryGetHost (headers : seq<string * string>) =
    tryExactlyOneHeader "Host" headers
    |> Result.ofOptionf "Expect exactly one 'Host' header."
    |> Result.bind (FParsec.runWithError Host.Parser "Invalid format for 'Host' header")

[<RequireQualifiedAccess>]
type Connection =
    /// Indicates client or server will close the connection after this message.
    | Close
    /// List of other connection-related headers.
    | Headers of seq<string>

/// There may be one 'Connection' header with a properly formatted value.
/// It should either have value 'close' or a comma-separated list of other connection-related headers.
/// See <see href="https://tools.ietf.org/html/rfc7230#section-6.1">RFC 7230 Section 6.1</see>.
let internal tryGetConnection (headers : seq<string * string>) = result {
    match! tryOneOrNoHeader "Connection" headers with
    | Some connection ->
        let! connectionHeader =
            connection
            |> FParsec.runWithError (sepBy1 Parsing.ptoken (Parsing.pows >>? pchar ',' >>? Parsing.pows)) "Invalid format for 'Connection' header"
        match connectionHeader with
        | ["close"] -> return Some Connection.Close
        | headers -> return Some (Connection.Headers headers)
    | None ->
        return None
}
