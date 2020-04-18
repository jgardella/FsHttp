module FsHttp.Core.Parsing

open FParsec

let punreserved : Parser<string> =
    choice [
        letter
        digit
        pchar '-'
        pchar '.'
        pchar '_'
        pchar '~'
    ] |>> string

let ppctencoded =
    (pchar '%' >>. hex >>. hex)
    |>> ignore
    |> skipped
    |> attempt

let psubdelims =
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

let ppchar =
    choice [
        punreserved |>> string
        ppctencoded
        psubdelims |>> string
        pstring ":"
        pstring "@"
    ]

let psegment = manyStrings ppchar

let psegmentnz = many1Strings ppchar

let pscheme : Parser<string> =
    many1Chars2 letter (letter <|> anyOf ['+';'-';'.'])
    |>> ignore
    |> skipped
    |> attempt

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

let puserinfo : Parser<string> =
    manyStrings (choice [
        punreserved
        ppctencoded
        psubdelims
        pstring ":"
    ])

let pregname =
    manyStrings (choice [
        punreserved
        ppctencoded
        psubdelims
    ])

let pipvfuture =
    pstring "v" .>> many1 hex .>> pstring "." .>> many1 (punreserved <|> psubdelims <|> pstring ":")
    |>> ignore
    |> skipped
    |> attempt

let pdecoctet =
    choice [
        anyOf ['1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] >>. digit
        pstring "1" >>. digit >>. digit
        pstring "2" >>. anyOf ['0'; '1'; '2'; '3'; '4'] >>. digit
        pstring "25" >>. anyOf ['0'; '1'; '2'; '3'; '4'; '5']
    ] |>> ignore |> skipped |> attempt

let pipv4address =
    pdecoctet .>> pstring "." .>> pdecoctet .>> pstring "." .>> pdecoctet .>> pstring "." .>> pdecoctet

let ph16 : Parser<string> =
    choice [
        hex |>> ignore
        skipArray 2 hex
        skipArray 3 hex
        skipArray 4 hex
    ] |> skipped |> attempt

let pls32 =
    choice [
        ph16 >>. pstring ":" >>. ph16
        pipv4address
    ] |>> ignore |> skipped |> attempt

let pipv6address =
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

let pipliteral =
    pstring "[" >>. (pipv6address <|> pipvfuture) .>> pstring "]"
    |>> ignore
    |> skipped
    |> attempt

let ptchar : Parser<char> =
    choice [
        pchar '!'
        pchar '#'
        pchar '$'
        pchar '%'
        pchar '&'
        pchar '''
        pchar '*'
        pchar '+'
        pchar '-'
        pchar '.'
        pchar '^'
        pchar '_'
        pchar '`'
        pchar '|'
        pchar '~'
        digit
        asciiLetter
    ]
