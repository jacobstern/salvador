let LiteralSegment = ./LiteralSegment.dhall

let CaptureSegment = ./CaptureSegment.dhall

in  < LiteralPathSegment :
        LiteralSegment
    | CapturePathSegment :
        CaptureSegment
    >
