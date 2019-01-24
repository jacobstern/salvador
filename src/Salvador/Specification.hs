module Salvador.Specification
    ( AnonymousRecord(..)
    , CaptureSegment(..)
    , Endpoint(..)
    , Field(..)
    , JSONBody(..)
    , JSONContent(..)
    , JSONType(..)
    , LiteralSegment(..)
    , Module(..)
    , OptionalValidation(..)
    , ParameterType(..)
    , Path(..)
    , PathSegment(..)
    , QueryParameter(..)
    , QueryParameterRequest(..)
    , Record(..)
    , ReferenceType(..)
    , Request(..)
    , RequestBody(..)
    , RequestBodyRequest(..)
    , Response(..)
    , ResponseContent(..)
    , Spec(..)
    , Validation(..)
    , Value(..)
    , ValueType(..)
    , interpretOptions
    )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Char                     as Char
import           GHC.Natural                    ( Natural )
import           GHC.Generics                   ( Generic )
import           Dhall                          ( Interpret(..)
                                                , InterpretOptions(..)
                                                , Type(..)
                                                , defaultInterpretOptions
                                                )
import           Dhall.Core                     ( Expr(..) )
import qualified Dhall.Map

newtype AnonymousRecord = AnonymousRecord
    { anonymousFields ::  [Field]
    } deriving (Eq, Show, Ord, Generic)

instance Interpret AnonymousRecord

data CaptureSegment = CaptureSegment
    { captureName :: Text
    , captureDescription :: Text
    , captureType :: Value
    } deriving (Eq, Show, Ord, Generic)

instance Interpret CaptureSegment

data Endpoint = Endpoint
    { endpointDescription :: Text
    , endpointRequest :: Request
    , endpointResponse :: Response
    } deriving (Eq, Show, Ord, Generic)

instance Interpret Endpoint

data Field = Field
    { fieldName :: Text
    , fieldRequired :: Bool
    , fieldDescription :: Text
    , fieldType :: JSONType
    } deriving (Eq, Show, Ord, Generic)

instance Interpret Field

newtype JSONBody = JSONBody
    { bodyType :: JSONType
    } deriving (Eq, Show, Ord, Generic)

instance Interpret JSONBody

data JSONContent = JSONContent
    { contentType :: JSONType
    , contentExample :: Text
    } deriving (Eq, Show, Ord, Generic)

instance Interpret JSONContent

data JSONType
    = ValueJSON ValueType | ListJSON ValueType | ReferenceJSON ReferenceType
    | ReferenceListJSON ReferenceType
    deriving (Eq, Show, Ord, Generic)

instance Interpret JSONType where
    autoWith options = Type extractOut expectedOut
      where
        valueType     = autoWith options :: Type ValueType
        referenceType = autoWith options :: Type ReferenceType
        extractOut (UnionLit name expr _)
            | name == "ValueJSON"
            = ValueJSON <$> extract valueType expr
            | name == "ListJSON"
            = ListJSON <$> extract valueType expr
            | name == "ReferenceJSON"
            = ReferenceJSON <$> extract referenceType expr
            | name == "ReferenceListJSON"
            = ReferenceListJSON <$> extract referenceType expr
            | otherwise
            = Nothing
        extractOut _ = Nothing
        expectedOut = Union
            (Dhall.Map.fromList
                [ ("ValueJSON"        , expected valueType)
                , ("ListJSON"         , expected valueType)
                , ("ReferenceJSON"    , expected referenceType)
                , ("ReferenceListJSON", expected referenceType)
                ]
            )


newtype LiteralSegment = LiteralSegment
    { literalSegment :: Text }
    deriving (Eq, Show, Ord, Generic)

instance Interpret LiteralSegment

data Module = Module
    { moduleTitle :: Text
    , moduleDescription :: Text
    , modulePaths :: [Path]
    , moduleDefinitions :: [Record]
    } deriving (Eq, Show, Ord, Generic)

instance Interpret Module

data OptionalValidation
    = AllowNull | AllowNullUndefinedMissing | AllowUndefinedMissing
    deriving (Eq, Show, Ord, Generic, Enum)

instance Interpret OptionalValidation

data ParameterType = ValueParameter ValueType | ListParameter ValueType
    deriving (Eq, Show, Ord, Generic)

instance Interpret ParameterType where
    autoWith options = Type extractOut expectedOut
      where
        valueType = autoWith options :: Type ValueType
        extractOut (UnionLit name expr _)
            | name == "ValueParameter"
            = ValueParameter <$> extract valueType expr
            | name == "ListParameter"
            = ListParameter <$> extract valueType expr
            | otherwise
            = Nothing
        extractOut _ = Nothing
        expectedOut = Union
            (Dhall.Map.fromList
                [ ("ValueParameter", expected valueType)
                , ("ListParameter" , expected valueType)
                ]
            )

data Path = Path
    { pathLocation :: [PathSegment]
    , pathEndpoints :: [Endpoint]
    } deriving (Eq, Show, Ord, Generic)

instance Interpret Path

data PathSegment
    = LiteralPathSegment LiteralSegment | CapturePathSegment CaptureSegment
    deriving (Eq, Show, Ord, Generic)

instance Interpret PathSegment where
    autoWith options = Type extractOut expectedOut
      where
        literalSegment = autoWith options :: Type LiteralSegment
        captureSegment = autoWith options :: Type CaptureSegment
        extractOut (UnionLit name expr _)
            | name == "LiteralPathSegment"
            = LiteralPathSegment <$> extract literalSegment expr
            | name == "CapturePathSegment"
            = CapturePathSegment <$> extract captureSegment expr
            | otherwise
            = Nothing
        extractOut _ = Nothing
        expectedOut = Union
            (Dhall.Map.fromList
                [ ("LiteralPathSegment", expected literalSegment)
                , ("CapturePathSegment", expected captureSegment)
                ]
            )

data QueryParameter = QueryParameter
    { parameterName :: Text
    , parameterType :: ParameterType
    , parameterRequired :: Bool
    , parameterDescription :: Text
    } deriving (Eq, Show, Ord, Generic)

instance Interpret QueryParameter

newtype QueryParameterRequest = QueryParameterRequest
    { requestParameters :: [QueryParameter]
    } deriving (Eq, Show, Ord, Generic)

instance Interpret QueryParameterRequest

data Record = Record
    { recordName :: Text
    , recordFields :: [Field]
    } deriving (Eq, Show, Ord, Generic)

instance Interpret Record

newtype ReferenceType = ReferenceType
    { referenceName :: Text
    } deriving (Eq, Show, Ord, Generic)

instance Interpret ReferenceType

data Request
    = Get QueryParameterRequest | Post RequestBodyRequest
    | Patch RequestBodyRequest | Put RequestBodyRequest
    | Delete QueryParameterRequest
    deriving (Eq, Show, Ord, Generic)

instance Interpret Request where
    autoWith options = Type extractOut expectedOut
      where
        queryParameterRequest = autoWith options :: Type QueryParameterRequest
        requestBodyRequest    = autoWith options :: Type RequestBodyRequest
        extractOut (UnionLit name expr _)
            | name == "Get"    = Get <$> extract queryParameterRequest expr
            | name == "Post"   = Post <$> extract requestBodyRequest expr
            | name == "Patch"  = Patch <$> extract requestBodyRequest expr
            | name == "Put"    = Put <$> extract requestBodyRequest expr
            | name == "Delete" = Delete <$> extract queryParameterRequest expr
            | otherwise        = Nothing
        extractOut _ = Nothing
        expectedOut = Union
            (Dhall.Map.fromList
                [ ("Get"   , expected queryParameterRequest)
                , ("Post"  , expected requestBodyRequest)
                , ("Patch" , expected requestBodyRequest)
                , ("Put"   , expected requestBodyRequest)
                , ("Delete", expected queryParameterRequest)
                ]
            )

data RequestBody
    = RecordRequestBody AnonymousRecord
    | ArbitraryRequestBody JSONBody
    deriving (Eq, Show, Ord, Generic)

instance Interpret RequestBody where
    autoWith options = Type extractOut expectedOut
      where
        anonymousRecord = autoWith options :: Type AnonymousRecord
        jsonBody        = autoWith options :: Type JSONBody
        extractOut (UnionLit name expr _)
            | name == "RecordRequestBody"
            = RecordRequestBody <$> extract anonymousRecord expr
            | name == "ArbitraryRequestBody"
            = ArbitraryRequestBody <$> extract jsonBody expr
            | otherwise
            = Nothing
        extractOut _ = Nothing
        expectedOut = Union
            (Dhall.Map.fromList
                [ ("RecordRequestBody"   , expected anonymousRecord)
                , ("ArbitraryRequestBody", expected jsonBody)
                ]
            )

newtype RequestBodyRequest = RequestBodyRequest
    { requestBody :: RequestBody
    } deriving (Eq, Show, Ord, Generic)

instance Interpret RequestBodyRequest

data Response = Response
    { responseStatusCode :: Natural
    , responseContent :: ResponseContent
    } deriving (Eq, Show, Ord, Generic)

instance Interpret Response

data ResponseContent = JSONResponse JSONContent | NoContentResponse
    deriving (Eq, Show, Ord, Generic)

instance Interpret ResponseContent where
    autoWith options = Type extractOut expectedOut
      where
        jsonContent = autoWith options :: Type JSONContent
        extractOut (UnionLit name expr _)
            | name == "JSONResponse" = JSONResponse <$> extract jsonContent expr
            | name == "NoContentResponse" = Just NoContentResponse
            | otherwise = Nothing
        extractOut _ = Nothing
        expectedOut = Union
            (Dhall.Map.fromList
                [ ("JSONResponse"     , expected jsonContent)
                , ("NoContentResponse", Dhall.Core.Record mempty)
                ]
            )

data Spec = Spec
    { specTitle :: Text
    , specDescription :: Text
    , specValidation :: Validation
    , specModules :: [Module]
    } deriving (Eq, Show, Ord, Generic)

instance Interpret Spec

data Validation = Validation
    { validationAcceptOptional :: OptionalValidation
    , validationReturnOptional :: OptionalValidation
    } deriving (Eq, Show, Ord, Generic)

instance Interpret Validation

data Value = IntegerValue | NaturalValue | DoubleValue | TextValue
    deriving (Eq, Show, Ord, Enum, Generic)

instance Interpret Value

newtype ValueType = ValueType
    { valueType :: Value
    } deriving (Eq, Show, Ord, Generic)

instance Interpret ValueType

dropFirstCamelCaseWord :: Text -> Text
dropFirstCamelCaseWord = firstToLower . Text.dropWhile Char.isLower
  where
    firstToLower t = case Text.uncons t of
        Just (c, rest) -> Text.cons (Char.toLower c) rest
        Nothing        -> t

interpretOptions :: InterpretOptions
interpretOptions =
    defaultInterpretOptions { fieldModifier = dropFirstCamelCaseWord }
