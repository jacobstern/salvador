let Path = ./Path.dhall

let NamedRecord = ./NamedRecord.dhall

in  { title :
        Text
    , description :
        Text
    , paths :
        List Path
    , definitions :
        List NamedRecord
    }
