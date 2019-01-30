module Salvador.Documentation
  ( renderDocumentation
  , renderDocumentationDoc
  , headerToSlug
  )
where

import qualified Data.Char                     as Char
import           Data.Foldable                  ( foldl' )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Vector.Fixed              ( Arity
                                                , VecList
                                                )
import qualified Data.Vector.Fixed             as Fixed
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
                                                ( renderStrict )
import           Salvador.Spec

type TableRow n = VecList n Text

makeCodeBlock :: Text -> Doc ann -> Doc ann
makeCodeBlock lang = enclose prefix suffix
 where
  prefix = "```" <> pretty lang <> hardline
  suffix = hardline <> "```" <> hardline <> hardline

renderHeader :: Int -> Text -> Doc ann
renderHeader level header =
  pretty (Text.replicate level "#")
    <+> pretty (Text.strip header)
    <>  hardline
    <>  hardline

renderBlock :: Text -> Doc ann
renderBlock contents = pretty contents <> hardline <> hardline

renderBoldBlock :: Text -> Doc ann
renderBoldBlock contents = bolded contents <> hardline <> hardline
  where bolded = enclose "**" "**" . pretty

renderDescription :: Text -> Doc ann
renderDescription contents =
  pretty (Text.strip contents) <> hardline <> hardline

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

renderTableRow :: Arity n => VecList n Int -> TableRow n -> Doc ann
renderTableRow widths rows = "|" <> pretty innerContents <> "|" <> hardline
 where
  pad width' text = text <> Text.replicate (width' - Text.length text) " "
  padContents width' text = " " <> pad width' text <> " "
  innerContents =
    (Text.intercalate "|" . Fixed.toList . Fixed.zipWith padContents widths)
      rows

renderTableDelimiter :: Arity n => VecList n Int -> Doc ann
renderTableDelimiter widths = "|" <> pretty innerContents <> "|" <> hardline
 where
  contents width' = " " <> Text.replicate width' "-" <> " "
  innerContents =
    (Text.intercalate "|" . Fixed.toList . Fixed.map contents) widths

renderTableGFM :: Arity n => NonEmpty (TableRow n) -> Doc ann
renderTableGFM table@(header :| rest) =
  renderTableRow widths header
    <> renderTableDelimiter widths
    <> foldMap (renderTableRow widths) rest
    <> hardline
  where widths = tableColumnWidths table

captureSegments :: [PathSegment] -> [CaptureSegment]
captureSegments path = do
  CapturePathSegment captureSegment <- path
  pure captureSegment

headerToSlug :: Text -> Text
headerToSlug = Text.map spaceToDash . removeInvalidChars . Text.toLower
 where
   -- https://gist.github.com/asabaylus/3071099#gistcomment-1593627
  spaceToDash c = if c == ' ' then '-' else c
  isValidChar c = or [Char.isAlphaNum c, c == '-', c == ' ']
  removeInvalidChars = Text.filter isValidChar

headerLink :: Text -> Text
headerLink header = "#" <> headerToSlug header

displayValue :: Value -> Text
displayValue (IntegerValue) = "Int"
displayValue (NaturalValue) = "Natural"
displayValue (DoubleValue ) = "Double"
displayValue (TextValue   ) = "Text"

displayValueList :: Value -> Text
displayValueList value = "List of " <> displayValue value

reference :: Text -> Text
reference name = "[" <> name <> "](" <> headerLink name <> ")"

referenceList :: Text -> Text
referenceList name = "List of " <> reference name

displayRequired :: Bool -> Text
displayRequired True  = "Yes"
displayRequired False = "Optional"

displayParameterType :: ParameterType -> Text
displayParameterType (ValueParameter (ValueType value)) = displayValue value
displayParameterType (ListParameter  (ValueType value)) = displayValueList value

displayJSONType :: JSONType -> Text
displayJSONType (ValueJSON         (ValueType     value)) = displayValue value
displayJSONType (ListJSON (ValueType value)) = displayValueList value
displayJSONType (ReferenceJSON     (ReferenceType name )) = reference name
displayJSONType (ReferenceListJSON (ReferenceType name )) = referenceList name

renderEndpointHeader :: [PathSegment] -> Endpoint -> Doc ann
renderEndpointHeader path Endpoint {..} = renderHeader 2 request
 where
  request = requestMethodName endpointRequest <> " " <> templatePathURL path

renderPathCaptures :: [PathSegment] -> Doc ann
renderPathCaptures path = case captureSegments path of
  []       -> mempty
  segments -> renderBoldBlock "URL Parameters" <> renderTableGFM
    (Fixed.mk3 "Parameter" "Type" "Description" :| fmap tableRow segments)
 where
  tableRow CaptureSegment {..} =
    Fixed.mk3 captureName (displayValue captureValueType) captureDescription

renderQueryParameters :: QueryParameterRequest -> Doc ann
renderQueryParameters QueryParameterRequest {..} = case requestParameters of
  []         -> mempty
  parameters -> renderBoldBlock "Query Parameters" <> renderTableGFM
    (  Fixed.mk4 "Parameter" "Type" "Required" "Description"
    :| fmap tableRow parameters
    )
 where
  tableRow QueryParameter {..} = Fixed.mk4
    parameterName
    (displayParameterType parameterType)
    (displayRequired parameterRequired)
    parameterDescription

renderRecordFields :: [Field] -> Doc ann
renderRecordFields fields = renderTableGFM
  (Fixed.mk4 "Field" "Type" "Required" "Description" :| fmap tableRow fields)
 where
  tableRow Field {..} = Fixed.mk4 fieldName
                                  (displayJSONType fieldType)
                                  (displayRequired fieldRequired)
                                  fieldDescription

renderJSONRequestBody :: JSONBody -> Doc ann
renderJSONRequestBody JSONBody {..} = renderJSONType bodyType

renderJSONType :: JSONType -> Doc ann
renderJSONType = renderBlock . displayJSONType

renderAnonymousRequestBody :: AnonymousRecord -> Doc ann
renderAnonymousRequestBody AnonymousRecord {..} =
  renderRecordFields anonymousFields

renderRequestBody :: RequestBodyRequest -> Doc ann
renderRequestBody request = renderBoldBlock "Request Body"
  <> bodyContents request
 where
  bodyContents (RequestBodyRequest (RecordRequestBody record)) =
    renderAnonymousRequestBody record
  bodyContents (RequestBodyRequest (ArbitraryRequestBody jsonBody)) =
    renderJSONRequestBody jsonBody

renderRequest :: Request -> Doc ann
renderRequest (Get    queryParameters) = renderQueryParameters queryParameters
renderRequest (Post   requestBody    ) = renderRequestBody requestBody
renderRequest (Patch  requestBody    ) = renderRequestBody requestBody
renderRequest (Put    requestBody    ) = renderRequestBody requestBody
renderRequest (Delete queryParameters) = renderQueryParameters queryParameters

renderResponseExample :: ResponseContent -> Doc ann
renderResponseExample (JSONResponse JSONContent {..}) =
  renderBoldBlock "Example Response Content"
    <> (makeCodeBlock "json" . pretty . Text.strip) contentExample
renderResponseExample NoContentResponse = mempty

renderResponse :: Response -> Doc ann
renderResponse Response {..} =
  renderBoldBlock "Response Status Code"
    <> renderBlock statusCode
    <> renderBoldBlock "Response Content"
    <> renderBlock content
    <> renderResponseExample responseContent
 where
  statusCode = (Text.pack . show) responseStatusCode
  content    = case responseContent of
    (JSONResponse JSONContent {..}) -> displayJSONType contentType
    NoContentResponse               -> "No content"


renderEndpoint :: [PathSegment] -> Endpoint -> Doc ann
renderEndpoint path endpoint@Endpoint {..} =
  renderEndpointHeader path endpoint
    <> renderDescription endpointDescription
    <> renderPathCaptures path
    <> renderRequest endpointRequest
    <> renderResponse endpointResponse

renderPath :: Path -> Doc ann
renderPath Path {..} = foldMap (renderEndpoint pathLocation) pathEndpoints

renderDefinition :: NamedRecord -> Doc ann
renderDefinition NamedRecord {..} =
  renderHeader 3 recordName
    <> renderDescription recordDescription
    <> renderRecordFields recordFields

renderDefinitions :: [NamedRecord] -> Doc ann
renderDefinitions [] = mempty
renderDefinitions records =
  renderHeader 2 "Definitions" <> foldMap renderDefinition records

renderModule :: Module -> Doc ann
renderModule Module {..} =
  renderHeader 1 moduleTitle
    <> renderDescription moduleDescription
    <> foldMap renderPath modulePaths
    <> renderDefinitions moduleDefinitions

renderDocumentationDoc :: Spec -> Doc ann
renderDocumentationDoc Spec {..} = foldMap renderModule specModules

renderDocumentation :: Spec -> Text
renderDocumentation =
  renderStrict . layoutPretty layoutOptions . renderDocumentationDoc
  where layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine 80 1}
