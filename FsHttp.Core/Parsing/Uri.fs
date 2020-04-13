module FsHttp.Core.Parsing.Uri

open System
open FParsec
open FsHttp.Core.Parsing.General
open FsHttp.Core.Types.Uri

let private punreserved : Parser<string> =
    choice [
        letter
        digit
        pchar '-'
        pchar '.'
        pchar '_'
        pchar '~'
    ] |>> string

let private ppctencoded =
    (pchar '%' >>. hex >>. hex)
    |>> ignore
    |> skipped
    |> attempt

let private psubdelims =
    choice [
        pchar '!'
        pchar '$'
        pchar '&'
        pchar '''
        pchar '('
        pchar ')'
        pchar '*'
        pchar '+'
        pchar ','
        pchar ';'
        pchar '='
    ] |>> string

let private ppchar =
    choice [
        punreserved |>> string
        ppctencoded
        psubdelims |>> string
        pstring ":"
        pstring "@"
    ]

let pscheme : Parser<string> =
    many1Chars2 letter (letter <|> anyOf ['+';'-';'.'])
    |>> ignore
    |> skipped
    |> attempt

let private psegment = manyStrings ppchar

let private psegmentnz = many1Strings ppchar

let pquery : Parser<string> =
    manyStrings (choice [
        ppchar
        pstring "/"
        pstring "?"
    ])

let pabsolutepath : Parser<string> =
    skipMany1 (pstring "/" >>. psegment)
    |> skipped
    |> attempt

let private puserinfo : Parser<string> =
    manyStrings (choice [
        punreserved
        ppctencoded
        psubdelims
        pstring ":"
    ])

let private pregname =
    manyStrings (choice [
        punreserved
        ppctencoded
        psubdelims
    ])

let private pipvfuture =
    pstring "v" .>> many1 hex .>> pstring "." .>> many1 (punreserved <|> psubdelims <|> pstring ":")
    |>> ignore
    |> skipped
    |> attempt

let private pdecoctet =
    choice [
        anyOf ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] >>. digit
        pstring "1" >>. digit >>. digit
        pstring "2" >>. anyOf ['0'; '1'; '2'; '3'; '4'] >>. digit
        pstring "25" >>. anyOf ['0'; '1'; '2'; '3'; '4'; '5']
    ] |>> ignore |> skipped |> attempt

let private pipv4address =
    pdecoctet .>> pstring "." .>> pdecoctet .>> pstring "." .>> pdecoctet .>> pstring "." .>> pdecoctet

let private ph16 : Parser<string> =
    choice [
        hex |>> ignore
        skipArray 2 hex
        skipArray 3 hex
        skipArray 4 hex
    ] |> skipped |> attempt

let private pls32 =
    choice [
        ph16 >>. pstring ":" >>. ph16
        pipv4address
    ] |>> ignore |> skipped |> attempt

let private pupto n p =
    many p
    >>= fun x -> if x.Length <= n then preturn x else fail (sprintf "pupto expected %d" n)

let private pipv6address =
    choice [
        skipArray 6 (ph16 >>. pstring ":") >>. pls32
        pstring "::" >>. skipArray 5 (ph16 >>. pstring ":") >>. pls32
        optional ph16 >>. pstring "::" >>. skipArray 4 (ph16 >>. pstring ":") >>. pls32
        optional (ph16 >>. pstring ":") >>. ph16 >>. pstring "::" >>. skipArray 3 (ph16 >>. pstring ":") >>. pls32
        pupto 2 (ph16 >>. pstring ":") >>. ph16 >>. pstring "::" >>. skipArray 2 (ph16 >>. pstring ":") >>. pls32
        pupto 3 (ph16 >>. pstring ":") >>. ph16 >>. pstring "::" >>. ph16 >>. pstring "::" >>. pls32
        pupto 4 (ph16 >>. pstring ":") >>. ph16 >>. pstring "::" >>. pls32
        pupto 5 (ph16 >>. pstring ":") >>. ph16 >>. pstring "::" >>. ph16
        pupto 6 (ph16 >>. pstring ":") >>. ph16 >>. pstring "::"
    ]

let private pipliteral =
    pstring "[" >>. (pipv6address <|> pipvfuture) .>> pstring "]"
    |>> ignore
    |> skipped
    |> attempt

let private phost : Parser<string> =
    choice [
        pipliteral
        pipv4address
        pregname
    ]

let pauthority =
    pipe3
        <| opt (puserinfo .>>? pstring "@")
        <| phost
        <| opt (pstring ":" >>. many1Chars digit)
        <| fun userInfo host port -> {
            Authority.UserInfo = userInfo
            Host = host
            Port = port |> Option.map Int32.Parse
        }

let private ppathabempty =
    skipMany (pstring "/" >>. psegment)
    |> skipped
    |> attempt

let private ppathabsolute =
    pstring "/" >>. optional (psegmentnz >>. skipMany (pstring "/" >>. psegment))
    |> skipped
    |> attempt

let private ppathrootless =
    many1Strings2 psegmentnz (pstring "/" >>. psegment |>> ((+) "/"))
    |>> ignore
    |> skipped
    |> attempt

let private ppathempty =
    optional (pstring "<" >>. ppchar .>> pstring ">")
    |> skipped
    |> attempt

let phierpart =
    choice [
        pstring "//" >>. pauthority >>? ppathabempty
        ppathabsolute
        ppathrootless
        ppathempty
    ]
    |>> ignore
    |> skipped
    |> attempt
