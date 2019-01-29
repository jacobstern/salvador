let Request = ./Request.dhall

let Response = ./Response.dhall

in  { title : Text, description : Text, request : Request, response : Response }
