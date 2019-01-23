let Path = ./Path.dhall

let Record = ./Record.dhall

in  { title :
        Text
    , description :
        Text
    , paths :
        List Path
    , definitions :
        List Record
    }
