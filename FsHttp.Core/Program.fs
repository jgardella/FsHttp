open Serilog
open FsHttp.Core

Log.Logger <- LoggerConfiguration().WriteTo.Console().CreateLogger()

[<EntryPoint>]
let main _argv =
    Server.start () |> Async.RunSynchronously
    0
