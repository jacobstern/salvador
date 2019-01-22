let Validation = ./Validation.dhall

let Module = ./Module.dhall

in  { title :
        Text
    , description :
        Text
    , validation :
        Validation
    , modules :
        List Module
    }
