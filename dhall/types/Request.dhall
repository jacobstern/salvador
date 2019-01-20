let QueryParameterRequest = ./QueryParameterRequest.dhall

let RequestBodyRequest = ./RequestBodyRequest.dhall

in  < Get :
        QueryParameterRequest
    | Post :
        RequestBodyRequest
    | Patch :
        RequestBodyRequest
    | Put :
        RequestBodyRequest
    | Delete :
        QueryParameterRequest
    >
