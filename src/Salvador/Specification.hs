module Salvador.Specification
    ( AnonymousRecord(..)
    , CaptureSegment(..)
    , Endpoint(..)
    , Field(..)
    , JSONBody (..)
    , JSONContent(..)
    , JSONType(..)
    , LiteralSegment(..)
    , Module(..)
    , OptionalValidation(..)
    , ParameterType(..)
    , Path(..)
    , PathSegment(..)
    , QueryParameter(..)
    , QueryParameterRequest (..)
    , Record(..)
    , Spec(..)
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
import           Dhall                          ( Interpret
                                                , InterpretOptions(..)
                                                , defaultInterpretOptions
                                                )

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
    , endpointRequest :: Text
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

instance Interpret JSONType

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

instance Interpret ParameterType

data Path = Path
    { pathLocation :: [PathSegment]
    , pathEndpoints :: [Endpoint]
    } deriving (Eq, Show, Ord, Generic)

instance Interpret Path

data PathSegment
    = LiteralPathSegment LiteralSegment | CapturePathSegment CaptureSegment
    deriving (Eq, Show, Ord, Generic)

instance Interpret PathSegment

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

instance Interpret Request

data RequestBody
    = RecordRequestBody AnonymousRecord
    | ArbitraryRequestBody JSONBody
    deriving (Eq, Show, Ord, Generic)

instance Interpret RequestBody

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

instance Interpret ResponseContent

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
