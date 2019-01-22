let Path = ./Path.dhall

let Definition = ./Definition.dhall

in  { name :
        Text
    , description :
        Text
    , paths :
        List Path
    , definitions :
        List Definition
    }
