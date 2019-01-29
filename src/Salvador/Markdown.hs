module Salvador.Markdown
  ( renderDocumentationGFM
  )
where

import           Data.Foldable                  ( foldl' )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Lazy                 ( toStrict )
import qualified Data.Text.Lazy.Builder        as Builder
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

makeJSONCodeBlock :: Builder -> Builder
makeJSONCodeBlock contents =
  "```json" <> lineBreak <> contents <> lineBreak <> "```" <> lineBreak

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

getCaptureSegments :: [PathSegment] -> [CaptureSegment]
getCaptureSegments segments = do
  CapturePathSegment captureSegment <- segments
  pure captureSegment

headerToSlug :: Text -> Text
headerToSlug = Text.map replaceSpaces . Text.toLower
 where
  replaceSpaces ' ' = '-'
  replaceSpaces c   = c

headerLink :: Text -> Text
headerLink header = "#" <> headerToSlug header

displayValue :: Value -> Text
displayValue (IntegerValue) = "Int"
displayValue (NaturalValue) = "Natural"
displayValue (DoubleValue ) = "Double"
displayValue (TextValue   ) = "Text"

displayReference :: Text -> Text
displayReference name = "[" <> name <> "](" <> headerLink name <> ")"

displayReferenceList :: Text -> Text
displayReferenceList name = "List of " <> displayReference name

displayRequired :: Bool -> Text
displayRequired True  = "Required"
displayRequired False = "Optional"

displayValueList :: Value -> Text
displayValueList value = "List of " <> displayValue value

displayParameterType :: ParameterType -> Text
displayParameterType (ValueParameter (ValueType value)) = displayValue value
displayParameterType (ListParameter  (ValueType value)) = displayValueList value

displayJSONType :: JSONType -> Text
displayJSONType (ValueJSON     (ValueType     value)) = displayValue value
displayJSONType (ListJSON      (ValueType     value)) = displayValueList value
displayJSONType (ReferenceJSON (ReferenceType name )) = displayReference name
displayJSONType (ReferenceListJSON (ReferenceType name)) =
  displayReferenceList name

renderEndpointHeader :: [PathSegment] -> Endpoint -> Builder
renderEndpointHeader path Endpoint {..} = renderHeader 2 request
 where
  request = requestMethodName endpointRequest <> " " <> templatePathURL path

renderPathCaptures :: [PathSegment] -> Builder
renderPathCaptures path = case getCaptureSegments path of
  []              -> mempty
  captureSegments -> renderHeader 3 "URL Parameters" <> renderTableGFM
    (Fixed.mk3 "Parameter" "Type" "Description" :| fmap tableRow captureSegments
    )
 where
  tableRow CaptureSegment {..} =
    Fixed.mk3 captureName (displayValue captureValueType) captureDescription

renderQueryParameters :: QueryParameterRequest -> Builder
renderQueryParameters QueryParameterRequest {..} = case requestParameters of
  []         -> mempty
  parameters -> renderHeader 3 "Query Parameters" <> renderTableGFM
    (  Fixed.mk4 "Parameter" "Type" "Required" "Description"
    :| fmap tableRow parameters
    )
 where
  tableRow QueryParameter {..} = Fixed.mk4
    parameterName
    (displayParameterType parameterType)
    (displayRequired parameterRequired)
    parameterDescription

renderRecordFields :: [Field] -> Builder
renderRecordFields fields = renderTableGFM
  (Fixed.mk4 "Field" "Type" "Required" "Description" :| fmap tableRow fields)
 where
  tableRow Field {..} = Fixed.mk4 fieldName
                                  (displayJSONType fieldType)
                                  (displayRequired fieldRequired)
                                  fieldDescription

renderJSONRequestBody :: JSONBody -> Builder
renderJSONRequestBody JSONBody {..} = renderJSONType bodyType

renderJSONType :: JSONType -> Builder
renderJSONType = makeParagraph . Builder.fromText . displayJSONType

renderAnonymousRequestBody :: AnonymousRecord -> Builder
renderAnonymousRequestBody AnonymousRecord {..} =
  renderRecordFields anonymousFields

renderRequestBody :: RequestBodyRequest -> Builder
renderRequestBody request = renderHeader 3 "Request Body"
  <> bodyContents request
 where
  bodyContents (RequestBodyRequest (RecordRequestBody record)) =
    renderAnonymousRequestBody record
  bodyContents (RequestBodyRequest (ArbitraryRequestBody jsonBody)) =
    renderJSONRequestBody jsonBody

renderRequest :: Request -> Builder
renderRequest (Get    queryParameters) = renderQueryParameters queryParameters
renderRequest (Post   requestBody    ) = renderRequestBody requestBody
renderRequest (Patch  requestBody    ) = renderRequestBody requestBody
renderRequest (Put    requestBody    ) = renderRequestBody requestBody
renderRequest (Delete queryParameters) = renderQueryParameters queryParameters

renderJSONCodeBlock :: Text -> Builder
renderJSONCodeBlock = makeJSONCodeBlock . Builder.fromText . Text.strip

renderResponsesTable :: Response -> Builder
renderResponsesTable Response {..} =
  renderHeader 3 "Responses" <> renderTableGFM
    (Fixed.mk2 "Status Code" "Content" :| [Fixed.mk2 statusCode content])
 where
  statusCode = (Text.pack . show) responseStatusCode
  content    = case responseContent of
    (JSONResponse JSONContent {..}) -> displayJSONType contentType
    NoContentResponse               -> "No content"

renderResponseExample :: Response -> Builder
renderResponseExample Response {..} = case responseContent of
  (JSONResponse JSONContent {..}) ->
    renderHeader 3 "Example Response" <> renderJSONCodeBlock contentExample
  NoContentResponse -> mempty

renderResponse :: Response -> Builder
renderResponse response =
  renderResponsesTable response <> renderResponseExample response

renderEndpoint :: [PathSegment] -> Endpoint -> Builder
renderEndpoint path endpoint@Endpoint {..} =
  renderEndpointHeader path endpoint
    <> renderDescription endpointDescription
    <> renderPathCaptures path
    <> renderRequest endpointRequest
    <> renderResponse endpointResponse

renderPath :: Path -> Builder
renderPath Path {..} = foldMap (renderEndpoint pathLocation) pathEndpoints

renderDefinition :: Record -> Builder
renderDefinition Record {..} =
  renderHeader 3 recordName <> renderRecordFields recordFields

renderDefinitions :: [Record] -> Builder
renderDefinitions records =
  renderHeader 2 "Definitions" <> foldMap renderDefinition records

renderModule :: Module -> Builder
renderModule Module {..} =
  renderHeader 1 moduleTitle
    <> renderDescription moduleDescription
    <> foldMap renderPath modulePaths
    <> renderDefinitions moduleDefinitions

renderDocumentationBuilder :: Spec -> Builder
renderDocumentationBuilder Spec {..} =
  renderTitle specTitle
    <> renderDescription specDescription
    <> foldMap renderModule specModules

renderDocumentationGFM :: Spec -> Text
renderDocumentationGFM = toStrict . toLazyText . renderDocumentationBuilder
