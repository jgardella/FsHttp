module FsHttp.Core.Server

open System
open System.Net.Sockets
open System.Text
open FParsec
open Serilog
open FsHttp.Core.Types
open FsHttp.Core.Types.Dto.Message

let handleConnection (stream : NetworkStream) = async {
    let connectionId = Guid.NewGuid()
    let buffer = Array.init 8192 (fun _ -> byte 0)
    stream.ReadTimeout <- 500

    let rec loop (bufferOffset : int) = async {
        let! readBytes = stream.ReadAsync(buffer, bufferOffset, 100) |> Async.AwaitTask
        let (response, newBufferOffset) =
            match run Dto.Message.RequestMessage.Parser (Encoding.UTF8.GetString(buffer, 0, bufferOffset + readBytes)) with
            | ParserResult.Success (request, _, _) ->
                Log.Information ("Successfully parsed received message on connection {connectionId}", connectionId)
                (Some {
                    ResponseMessage.StatusLine =
                        {
                            StatusLine.HttpVersion = { HttpVersion.MajorVersion = 1; MinorVersion = 1 }
                            StatusCode = 200
                            ReasonPhrase = "OK"
                        }
                    Headers = [("Content-Length", "0")]
                    Body = None
                }, 0)
            | ParserResult.Failure (error, _, _) when bufferOffset + readBytes >= 8192 ->
                Log.Information ("Max buffer size reached and failed to parse received message on connection {connectionId}, error: {error}", connectionId, error)
                (Some {
                    ResponseMessage.StatusLine =
                        {
                            StatusLine.HttpVersion = { HttpVersion.MajorVersion = 1; MinorVersion = 1 }
                            StatusCode = 400
                            ReasonPhrase = "Bad Request"
                        }
                    Headers = [("Content-Length", "0")]
                    Body = None
                }, 0)
            | ParserResult.Failure (_, _, _) -> (None, bufferOffset + readBytes)
        do!
            response
            |> Option.iterAsync (fun response ->
                let encodedResponse = ResponseMessage.Bytes response
                stream.WriteAsync(encodedResponse, 0, encodedResponse.Length) |> Async.AwaitTask)
        if readBytes > 0 then
            return! loop newBufferOffset
        else
            Log.Information ("Reached end of network stream for connection {connectionId}", connectionId)
            return ()
    }

    do!
        try
            Log.Information ("Initiating connection {connectionId}", connectionId)
            loop 0
        with
            | exn ->
                Log.Warning ("Exception for connection {connectionId}, exception: {exception}", connectionId, exn)
                async.Return ()

    Log.Information ("Closing connection {connectionId}", connectionId)
    stream.Close()
}

let rec listen (listener : TcpListener) = async {
    let! client = listener.AcceptTcpClientAsync() |> Async.AwaitTask
    let stream = client.GetStream()
    do! handleConnection stream |> Async.StartChild |> Async.Ignore
    return! listen listener
}

let start () =
    Log.Information "FsHttp server started"
    let listener = TcpListener.Create(80)
    listener.Start()
    listen listener
