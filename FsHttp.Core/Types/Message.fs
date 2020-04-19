module FsHttp.Core.Types.Message

open System
open FsHttp.Core.Types
open FsHttp.Core.Types.Dto.Message

type ServerConfig = {
    FixedScheme : string option
    FixedAuthority : string option
    DefaultAuthority : string
}

type RequestError =
    | BadRequest of string

/// Request target must be properly formatted.
let internal tryGetRequestTarget (target : string) =
    FParsec.runWithError RequestTarget.Parser "Invalid request target" target

/// Returns true if the provided port is the default TCP port for the provided scheme.
let internal isDefaultPortForScheme (scheme : string) (tcpPort : int) =
    match (scheme, tcpPort) with
    | ("http", 80) -> true
    | _ -> false

/// Builds the effective request URI from the provided request information
/// See <see href="https://tools.ietf.org/html/rfc7230#section-5.5">RFC 7230 Section 5.5</see>.
let internal getEffectiveRequestUri (fixedScheme : string option) (isSecureConnection : bool) (fixedAuthority : string option) (defaultAuthority : string) (host : Header.Host option) (tcpPort : int) (requestTarget : RequestTarget) =
    let scheme =
        fixedScheme
        |> Option.defaultValue (if isSecureConnection then "https" else "http")

    let defaultAuthority =
        if not (isDefaultPortForScheme scheme tcpPort) then
            sprintf "%s:%d" defaultAuthority tcpPort
        else
            defaultAuthority

    let makeUri authority path query =
        let authority =
            fixedAuthority
            |> Option.orElse authority
            |> Option.orElse (host |> Option.map (fun (Header.Host host) -> host))
            |> Option.defaultValue defaultAuthority
        let query =
            query
            |> Option.map (fun query -> "?" + query)
            |> Option.defaultValue ""
        Uri (sprintf "%s://%s%s%s" scheme authority path query)

    match requestTarget with
    | Absolute absoluteTarget ->
        Uri absoluteTarget
    | Authority authority ->
        makeUri (Some authority) "" None
    | Asterisk ->
        makeUri None "" None
    | Origin origin ->
        makeUri None origin.Path origin.Query

/// An HTTP Request message which has been fully validated.
type RequestMessage = {
    Host : Header.Host option
    RequestTarget : RequestTarget
    EffectiveRequestUri : Uri
    Connection : Header.Connection option
} with
    /// Tries to create a request message from a DTO.
    /// Returns Error if the request described by the DTO is not valid.
    static member TryOfDto (serverConfig : ServerConfig) (isSecureConnection : bool) (tcpPort : int) (dto : Dto.Message.RequestMessage) = result {
        let! host = Header.tryGetHost dto.Headers
        let! requestTarget = tryGetRequestTarget dto.RequestLine.Target
        let effectiveRequestUri = getEffectiveRequestUri serverConfig.FixedScheme isSecureConnection serverConfig.FixedAuthority serverConfig.DefaultAuthority host tcpPort requestTarget
        let! connection = Header.tryGetConnection dto.Headers
        return {
            RequestMessage.Host = host
            RequestTarget = requestTarget
            EffectiveRequestUri = effectiveRequestUri
            Connection = connection
        }
    }

