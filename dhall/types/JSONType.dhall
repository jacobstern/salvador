let ValueType = ./ValueType.dhall

let ReferenceType = ./ReferenceType.dhall

in  < ValueJSON :
        ValueType
    | ListJSON :
        ValueType
    | ReferenceJSON :
        ReferenceType
    | ReferenceListJSON :
        ReferenceType
    >
