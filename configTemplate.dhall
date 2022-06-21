let Verbosity  = < Sql   | Debug   | Info    | Warning | Error >
let Mode       = < None  | Display | Write   | Both >
let LoggerConf = { cVerbosity : Verbosity, cMode : Mode, cFilePath : Text}
let DBConf     = { cHost : Text, cPort : Integer, cUser : Text, cPassword : Text, cPagSize : Integer}
let Conf       = { loggerConf : LoggerConf, DBConf : DBConf, cPort : Integer, cAddress : Integer}
in 
{ cLogger = 
    { cVerbosity = Verbosity.Debug
    , cMode      = Mode.Both
    , cFilePath  = "log.txt"
    }
, cDatabase = 
    { cHost      = "localhost"
    , cPort = +5432
    , cUser = ""
    , cPassword = ""
    , cPagSize = +20
    }
, cPort     = +3000
, cAddress = "http://localhost"
}
