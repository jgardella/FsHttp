module FsHttp.Core.Server

open System
open System.Net.Sockets
open System.Text
open FParsec
open FsHttp.Core.Types
open FsHttp.Core.Types.Message

let rec handleConnection (connectionId : Guid) (stream : NetworkStream) = async {
    let response =
        match runParserOnStream Dto.Message.RequestMessage.Parser () (string connectionId) stream Encoding.UTF8 with
        | ParserResult.Success (request, _, _) -> ()
        | ParserResult.Failure (error, _, _) -> ()
    let encodedResponse = [||]
    do! stream.WriteAsync(encodedResponse, 0, encodedResponse.Length) |> Async.AwaitTask
    return! handleConnection connectionId stream
}

let rec listen (listener : TcpListener) = async {
    let! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
    let connectionId = Guid.NewGuid()
    let stream = client.GetStream()
    do! handleConnection connectionId stream |> Async.StartChild |> Async.Ignore
    return! listen listener
}

let start (_ : ServerConfig) =
    let listener = TcpListener.Create(80)
    listener.Start()
    listen listener
