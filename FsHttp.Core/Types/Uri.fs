module FsHttp.Core.Types.Uri

type Authority = {
    UserInfo : string option
    Host : string
    Port : int option
}