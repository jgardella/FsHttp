module FsHttp.Core.Parsing.General

open FParsec

type Parser<'T> = Parser<'T, unit>

// For setting breakpoint when debugging parser.
let bp (p : Parser<_, _>) stream =
    p stream