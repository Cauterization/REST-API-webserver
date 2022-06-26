let Verbosity  = < Sql   | Debug   | Info    | Warning | Error >
let Mode       = < None  | Display | Write   | Both >
let LoggerConf = { cVerbosity : Verbosity
                 , cMode : Mode
                 , cFilePath : Text
                 }
let DBConf     = { cHost : Text
                 , cPort : Integer
                 , cUser : Text
                 , cPassword : Text
                 , cDatabase : Text
                 , cPagSize : Integer
                 }
let Conf       = { cLogger : LoggerConf
                 , cDB : DBConf
                 , cPort : Integer
                 , cAddress : Integer
                 }
in 
{ cLogger = 
    { cVerbosity = Verbosity.Sql
    , cMode      = Mode.Both
    , cFilePath  = "log.txt"
    }
, cDB = 
    { cHost      = "localhost"
    , cPort = +5432
    , cUser = ""
    , cPassword = ""
    , cDatabase = ""
    , cPagSize = +20
    }
, cPort     = +3000
, cAddress = "http://localhost"
}
