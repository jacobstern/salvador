module Salvador.Markdown
  ( renderDocumentationGFM
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Text.Lazy                 ( toStrict )
import           Data.Text.Lazy.Builder         ( Builder
                                                , fromText
                                                , toLazyText
                                                , singleton
                                                )
import           Salvador.Spec

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
    templateSegment (LiteralPathSegment LiteralSegment{..}) = literalSegment
    templateSegment (CapturePathSegment CaptureSegment{..}) =
      "{" <> captureName <> "}"

renderEndpointHeader :: [PathSegment] -> Endpoint -> Builder
renderEndpointHeader segments _ = renderHeader 3 (templatePathURL segments)

renderPath :: Path -> Builder
renderPath Path{..} =
  foldMap (renderEndpointHeader pathLocation) pathEndpoints

renderModule :: Module -> Builder
renderModule Module {..} =
  renderHeader 2 moduleTitle
    <> renderDescription moduleDescription
    <> foldMap renderPath modulePaths

renderDocumentationBuilder :: Spec -> Builder
renderDocumentationBuilder Spec {..} =
  renderTitle specTitle
    <> renderDescription specDescription
    <> foldMap renderModule specModules

renderDocumentationGFM :: Spec -> Text
renderDocumentationGFM = toStrict . toLazyText . renderDocumentationBuilder
