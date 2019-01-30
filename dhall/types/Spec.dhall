let Validation = ./Validation.dhall

let Module = ./Module.dhall

in  { title : Text, validation : Validation, modules : List Module }
