module FsHttp.Core.Types.Message

open FsHttp.Core.Types

type RequestError =
    | BadRequest of string

/// Tries to get exactly one header with the provided header.
let internal tryExactlyOneHeader (targetHeader : string) (headers : seq<string * string>) =
    headers
    |> Seq.where (fst >> ((=) targetHeader))
    |> Seq.tryExactlyOne
    |> Option.map snd

/// There must be exactly one 'Host' header with a properly formatted value.
let internal tryGetHost (headers : seq<string * string>) =
    tryExactlyOneHeader "Host" headers
    |> Result.ofOptionf "Expect exactly one 'Host' header."
    |> Result.bind (FParsec.runWithError Host.Parser "Invalid format for 'Host' header")
    |> Result.mapError RequestError.BadRequest

/// Request target must be properly formatted.
let internal tryGetRequestTarget (target : string) =
    FParsec.runWithError RequestTarget.Parser "Invalid request target" target
    |> Result.mapError RequestError.BadRequest

/// An HTTP Request message which has been fully validated.
type RequestMessage = private {
    Host : Host
    RequestTarget : RequestTarget
} with
    /// Tries to create a request message from a DTO.
    /// Returns Error if the request described by the DTO is not valid.
    static member TryOfDto (dto : Dto.Message.RequestMessage) = result {
        let! host = tryGetHost dto.Headers
        let! requestTarget = tryGetRequestTarget dto.RequestLine.Target
        return {
            RequestMessage.Host = host
            RequestTarget = requestTarget
        }
    }

