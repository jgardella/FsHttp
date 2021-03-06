module FsHttp.Tests.Types.Header

open Xunit
open FsHttp.Core.Types
open FsHttp.Tests.Helpers

module Host =

    [<Fact>]
    let ``succesfully gets valid host from headers`` () =
        let headers = [("Host", "www.example.com:80")]
        let expected = "www.example.com:80" |> Header.Host |> Some
        Assert.OkEqual(expected, Header.tryGetHost headers)

    [<Fact>]
    let ``succesfully gets valid empty host from headers`` () =
        let headers = [("Host", "")]
        let expected = None
        Assert.OkEqual(expected, Header.tryGetHost headers)

    [<Fact>]
    let ``error for no Host header`` () =
        let headers = Seq.empty
        Assert.IsError(Header.tryGetHost headers)

    [<Fact>]
    let ``error for multiple Host headers`` () =
        let headers = [
            ("Host", "")
            ("Host", "www.example.com")
        ]
        Assert.IsError(Header.tryGetHost headers)

    [<Fact>]
    let ``error for invalid formatting in Host header`` () =
        let headers = [("Host", "www.exam^^^ple.com")]
        Assert.IsError(Header.tryGetHost headers)

module Connection =

    [<Fact>]
    let ``succesfully gets close connection header from headers`` () =
        let headers = [("Connection", "close")]
        let expected = Some Header.Connection.Close
        Assert.OkEqual(expected, Header.tryGetConnection headers)

    [<Fact>]
    let ``succesfully gets valid connection header with header fields from headers`` () =
        let headers = [("Connection", "keep-alive, transfer-encoding")]
        let expected = Some (Header.Connection.Headers ["keep-alive"; "transfer-encoding"])
        Assert.OkEqual(expected, Header.tryGetConnection headers)

    [<Fact>]
    let ``succesfully handles no connection header`` () =
        let headers = Seq.empty
        let expected = None
        Assert.OkEqual(expected, Header.tryGetConnection headers)

    [<Fact>]
    let ``error for empty Connection header`` () =
        let headers = [("Connection", "")]
        Assert.IsError(Header.tryGetConnection headers)

    [<Fact>]
    let ``error for multiple Connection headers`` () =
        let headers = [
            ("Connection", "close")
            ("Connection", "close")
        ]
        Assert.IsError(Header.tryGetConnection headers)

module Upgrade =

    [<Fact>]
    let ``succesfully gets upgrade header from headers`` () =
        let headers = [("Upgrade", "HTTP/1.1")]
        let expected = Some [
            { Header.Upgrade.ProtocolName = "HTTP"; Header.Upgrade.ProtocolVersion = Some "1.1" }
        ]
        Assert.OkEqual(expected, Header.tryGetUpgrade headers)

    [<Fact>]
    let ``succesfully gets upgrade header without version from headers`` () =
        let headers = [("Upgrade", "SMTP")]
        let expected = Some [
            { Header.Upgrade.ProtocolName = "SMTP"; Header.Upgrade.ProtocolVersion = None }
        ]
        Assert.OkEqual(expected, Header.tryGetUpgrade headers)

    [<Fact>]
    let ``succesfully handles no upgrade header`` () =
        let headers = Seq.empty
        let expected = None
        Assert.OkEqual(expected, Header.tryGetUpgrade headers)

    [<Fact>]
    let ``error for empty Upgrade header`` () =
        let headers = [("Upgrade", "")]
        Assert.IsError(Header.tryGetUpgrade headers)

    [<Fact>]
    let ``error for multiple Upgrade headers`` () =
        let headers = [
            ("Upgrade", "HTTP/1.1")
            ("Upgrade", "HTTP/2.0")
        ]
        Assert.IsError(Header.tryGetUpgrade headers)

