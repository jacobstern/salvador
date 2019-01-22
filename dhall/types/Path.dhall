let PathSegment = ./PathSegment.dhall

let Endpoint = ./Endpoint.dhall

in  { location : List PathSegment, endpoints : List Endpoint }
