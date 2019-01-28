module Salvador.Markdown
  ( renderDocumentationGFM
  )
where

import           Data.Foldable                  ( foldl' )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Lazy                 ( toStrict )
import           Data.Text.Lazy.Builder         ( Builder
                                                , fromText
                                                , toLazyText
                                                , singleton
                                                )

import           Data.Vector.Fixed              ( Arity
                                                , VecList
                                                )
import qualified Data.Vector.Fixed             as Fixed
import           Salvador.Spec

type TableRow n = VecList n Text

lineBreak :: Builder
lineBreak = singleton '\n'

makeParagraph :: Builder -> Builder
makeParagraph contents = contents <> lineBreak <> lineBreak

renderHeader :: Int -> Text -> Builder
renderHeader level header =
  fromText (Text.replicate level "#")
    <> singleton ' '
    <> fromText (Text.strip header)
    <> lineBreak
    <> lineBreak

renderTitle :: Text -> Builder
renderTitle = renderHeader 1

renderDescription :: Text -> Builder
renderDescription = makeParagraph . fromText . Text.strip

templatePathURL :: [PathSegment] -> Text
templatePathURL = mappend "/" . Text.intercalate "/" . fmap templateSegment
 where
  templateSegment (LiteralPathSegment LiteralSegment {..}) = literalSegment
  templateSegment (CapturePathSegment CaptureSegment {..}) =
    "{" <> captureName <> "}"

requestMethodName :: Request -> Text
requestMethodName (Get    _) = "GET"
requestMethodName (Post   _) = "POST"
requestMethodName (Put    _) = "PUT"
requestMethodName (Patch  _) = "PATCH"
requestMethodName (Delete _) = "DELETE"

tableColumnWidths :: Arity n => NonEmpty (TableRow n) -> VecList n Int
tableColumnWidths = foldl' reduce (Fixed.replicate 0)
  where reduce acc = Fixed.zipWith max acc . Fixed.map Text.length

renderTableRow :: Arity n => VecList n Int -> TableRow n -> Builder
renderTableRow widths rows = "|" <> fromText innerContents <> "|" <> lineBreak
 where
  pad width text = text <> Text.replicate (width - Text.length text) " "
  padContents width text = " " <> pad width text <> " "
  innerContents =
    (Text.intercalate "|" . Fixed.toList . Fixed.zipWith padContents widths)
      rows

renderTableDelimiter :: Arity n => VecList n Int -> Builder
renderTableDelimiter widths = "|" <> fromText innerContents <> "|" <> lineBreak
 where
  contents width = " " <> Text.replicate width "-" <> " "
  innerContents =
    (Text.intercalate "|" . Fixed.toList . Fixed.map contents) widths

renderTableGFM :: Arity n => NonEmpty (TableRow n) -> Builder
renderTableGFM table@(header :| rest) =
  renderTableRow widths header
    <> renderTableDelimiter widths
    <> foldMap (renderTableRow widths) rest
    <> lineBreak
  where widths = tableColumnWidths table

renderEndpointHeader :: [PathSegment] -> Endpoint -> Builder
renderEndpointHeader segments Endpoint {..} =
  renderHeader 2
    $  requestMethodName endpointRequest
    <> " "
    <> templatePathURL segments

getCaptureSegments :: [PathSegment] -> [CaptureSegment]
getCaptureSegments segments = do
  CapturePathSegment captureSegment <- segments
  pure captureSegment

displayValueType :: Value -> Text
displayValueType (IntegerValue) = "Integer"
displayValueType (NaturalValue) = "Natural"
displayValueType (DoubleValue ) = "Double"
displayValueType (TextValue   ) = "Text"

renderPathCaptures :: [PathSegment] -> Builder
renderPathCaptures segments = case getCaptureSegments segments of
  []              -> mempty
  captureSegments -> renderTableGFM
    (Fixed.mk3 "Capture" "Type" "Description" :| fmap tableRow captureSegments)
 where
  tableRow CaptureSegment {..} =
    Fixed.mk3 captureName (displayValueType captureValueType) captureDescription

renderEndpoint :: [PathSegment] -> Endpoint -> Builder
renderEndpoint segments endpoint@Endpoint {..} =
  renderEndpointHeader segments endpoint
    <> renderDescription endpointDescription

renderPath :: Path -> Builder
renderPath Path {..} = foldMap (renderEndpoint pathLocation) pathEndpoints
  <> renderPathCaptures pathLocation

renderModule :: Module -> Builder
renderModule Module {..} =
  renderHeader 1 moduleTitle
    <> renderDescription moduleDescription
    <> foldMap renderPath modulePaths

renderDocumentationBuilder :: Spec -> Builder
renderDocumentationBuilder Spec {..} =
  renderTitle specTitle
    <> renderDescription specDescription
    <> foldMap renderModule specModules

renderDocumentationGFM :: Spec -> Text
renderDocumentationGFM = toStrict . toLazyText . renderDocumentationBuilder
