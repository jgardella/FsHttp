namespace FsHttp.Core.Types

open System
open FParsec
open FsHttp.Core.Parsing

type Host =
    | IPLiteral of string
    | IPv4 of string
    | RegName of string
with
    static member Parser =
        choice [
            pipliteral |>> IPLiteral
            pipv4address |>> IPv4
            pregname |>> RegName
        ]

type Authority = {
    UserInfo : string option
    Host : Host
    Port : int option
} with
    static member Parser =
        pipe3
            <| opt (puserinfo .>>? pstring "@")
            <| Host.Parser
            <| opt (pstring ":" >>. many1Chars digit)
            <| fun userInfo host port -> {
                Authority.UserInfo = userInfo
                Host = host
                Port = port |> Option.map Int32.Parse
            }

