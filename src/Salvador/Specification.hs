module Salvador.Specification where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Char                     as Char
import           GHC.Generics                   ( Generic )
import           Dhall                          ( Interpret
                                                , InterpretOptions(..)
                                                , defaultInterpretOptions
                                                )

data ParameterType = ValueParameter ValueType | ListParameter ValueType
    deriving (Eq, Show, Ord, Generic)

instance Interpret ParameterType

data Value = IntegerValue | NaturalValue | DoubleValue | TextValue
    deriving (Eq, Show, Ord, Generic)

instance Interpret Value

newtype ValueType = ValueType
    { getValueType :: Value
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
