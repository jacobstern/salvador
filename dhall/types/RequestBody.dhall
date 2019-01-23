let AnonymousRecord = ./AnonymousRecord.dhall

let JSONBody = ./JSONBody.dhall

in  < RecordRequestBody : AnonymousRecord | ArbitraryRequestBody : JSONBody >
