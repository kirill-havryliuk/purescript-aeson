-- | Using json-bigint library to parse JSON storing numbers using
-- | BigNumber from bignumber.js. API and behaviour is intended to be close to
-- | Aeson.
-- | Using hack for stringify, see Aeson.js.

module Aeson (
  (.:)
  , (.:?)
  , Aeson
  , AesonCases
  , class EncodeAeson
  , class GEncodeAeson
  , class DecodeAeson
  , class DecodeAesonField
  , class GDecodeAeson
  , caseAeson
  , caseAesonArray
  , caseAesonBigInt
  , caseAesonBoolean
  , caseAesonNull
  , caseAesonNumber
  , caseAesonObject
  , caseAesonString
  , caseAesonUInt
  , constAesonCases
  , decodeAeson
  , decodeAesonField
  , decodeJsonString
  , encodeAeson
  , gDecodeAeson
  , gEncodeAeson
  , getField
  , getFieldOptional
  , getFieldOptional'
  , getNestedAeson
  , jsonToAeson
  , parseJsonStringToAeson
  , stringifyAeson
  , isNull
  , isBoolean
  , isNumber
  , isString
  , isArray
  , isObject
  , toNull
  , toBoolean
  , toNumber
  , toString
  , toArray
  , toObject
  , fromString
  , aesonNull
  , JsonDecodeError(..)
) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (Json, stringify) as Argonaut
import Data.Array (toUnfoldable, fromFoldable, (!!))
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (Either(Right, Left), fromRight, note)
import Data.Foldable (foldM)
import Data.Int as Int
import Data.List as L
import Data.List.Lazy as LL
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Number (fromString) as Number
import Data.Number.Format (toString) as Number
import Data.Sequence (Seq)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Data.Typelevel.Undefined (undefined)
import Data.UInt (UInt)
import Data.UInt as UInt
import Foreign.Object (Object)
import Foreign.Object as FO
import Prim.Row as Row
import Prim.RowList as RL
import Record as Record
import Type.Prelude (Proxy(Proxy))
import Untagged.Union (class InOneOf, type (|+|), asOneOf)
import Unsafe.Coerce (unsafeCoerce)
import Data.Nullable (null)

-- | A piece of JSON where all numbers are extracted into `NumberIndex`.
newtype Aeson = Aeson JsonBigInt

instance Eq Aeson where
  eq a b = stringifyAeson a == stringifyAeson b

instance Show Aeson where
  show = stringifyAeson

data JsonDecodeError = TypeMismatch String | AtKey String JsonDecodeError | MissingValue | ParsingError

derive instance Eq JsonDecodeError

instance Show JsonDecodeError where
  show (TypeMismatch x) = "TypeMismatch " <> x
  show (AtKey k x) = "AtKey " <> k <> " (" <> show x <> ")"
  show MissingValue = "MissingValue"
  show ParsingError = "ParsingError"

class DecodeAeson (a :: Type) where
  decodeAeson :: Aeson -> Either JsonDecodeError a

-------- Parsing: String -> Aeson --------

foreign import data JsonBigInt :: Type

foreign import parseJsonBigInt ::
  (forall a. Maybe a) -> (forall a. a -> Maybe a) ->  String -> Maybe JsonBigInt

parseJsonStringToAeson :: String -> Either JsonDecodeError Aeson
parseJsonStringToAeson payload = Aeson <$> (note ParsingError $ (parseJsonBigInt Nothing Just payload))

-- -------- Stringifying: Aeson -> String

foreign import stringifyJsonBigInt :: JsonBigInt -> String

stringifyAeson :: Aeson -> String
stringifyAeson (Aeson json) = stringifyJsonBigInt json

-- -------- Json <-> Aeson --------

-- | Recodes Argonaut Json to Aeson.
-- | NOTE. The operation is costly as its stringifies given Json
-- |       and reparses resulting string as Aeson.
jsonToAeson :: Argonaut.Json -> Aeson
jsonToAeson = Argonaut.stringify >>> decodeJsonString >>> fromRight shouldNotHappen
  where
  -- valid json should always decode without errors
  shouldNotHappen = undefined

-------- Aeson manipulation and field accessors --------

getField
  :: forall (a :: Type)
   . DecodeAeson a
  => Object Aeson
  -> String
  -> Either JsonDecodeError a
getField aesonObject field = getField' decodeAeson aesonObject field
  where
  -- | Adapted from `Data.Argonaut.Decode.Decoders`
  getField'
    :: (Aeson -> Either JsonDecodeError a)
    -> Object Aeson
    -> String
    -> Either JsonDecodeError a
  getField' decoder obj str =
    maybe
      (Left $ AtKey str MissingValue)
      (lmap (AtKey str) <<< decoder)
      (FO.lookup str obj)

infix 7 getField as .:

-- | Attempt to get the value for a given key on an `Object Aeson`.
-- |
-- | The result will be `Right Nothing` if the key and value are not present,
-- | but will fail if the key is present but the value cannot be converted to the right type.
-- |
-- | This function will treat `null` as a value and attempt to decode it into your desired type.
-- | If you would like to treat `null` values the same as absent values, use
-- | `getFieldOptional'` (`.:?`) instead.
getFieldOptional
  :: forall (a :: Type)
   . DecodeAeson a
  => Object Aeson
  -> String
  -> Either JsonDecodeError (Maybe a)
getFieldOptional = getFieldOptional_ decodeAeson
  where
  getFieldOptional_
    :: (Aeson -> Either JsonDecodeError a)
    -> Object Aeson
    -> String
    -> Either JsonDecodeError (Maybe a)
  getFieldOptional_ decoder obj str =
    maybe (pure Nothing) (map Just <<< decode) (FO.lookup str obj)
    where
    decode = lmap (AtKey str) <<< decoder

infix 7 getFieldOptional as .:!

-- | Attempt to get the value for a given key on an `Object Aeson`.
-- |
-- | The result will be `Right Nothing` if the key and value are not present,
-- | or if the key is present and the value is `null`.
-- |
-- | Use this accessor if the key and value are optional in your object.
-- | If the key and value are mandatory, use `getField` (`.:`) instead.
getFieldOptional'
  :: forall (a :: Type)
   . DecodeAeson a
  => Object Aeson
  -> String
  -> Either JsonDecodeError (Maybe a)
getFieldOptional' = getFieldOptional'_ decodeAeson
  where
  getFieldOptional'_
    :: (Aeson -> Either JsonDecodeError a)
    -> Object Aeson
    -> String
    -> Either JsonDecodeError (Maybe a)
  getFieldOptional'_ decoder obj str =
    maybe (pure Nothing) decode (FO.lookup str obj)
    where
    decode aeson =
      if isNull aeson then
        pure Nothing
      else
        Just <$> (lmap (AtKey str) <<< decoder) aeson

infix 7 getFieldOptional' as .:?

-- | Returns an Aeson available under a sequence of keys in given Aeson.
-- | If not possible returns JsonDecodeError.
getNestedAeson :: Aeson -> Array String -> Either JsonDecodeError Aeson
getNestedAeson aeson keys =
  note (TypeMismatch "Expected nested object") $
     foldM lookup aeson keys
  where
  lookup :: Aeson -> String -> Maybe Aeson
  lookup j lbl = caseAesonObject Nothing (FO.lookup lbl) j

-- From bignumber library
foreign import data BigNumber :: Type

foreign import bigNumberFromString :: String -> BigNumber


-- | Utility abbrevation. See `caseAeson` for an example usage.
type AesonCases a =
  { caseNull :: Unit -> a
  , caseBoolean :: Boolean -> a
  , caseNumber :: String -> a
  , caseString :: String -> a
  , caseArray :: Array Aeson -> a
  , caseObject :: Object Aeson -> a
  }

foreign import caseJson ::
  forall a. (Unit -> a) -> (Boolean -> a)
    -> (String -> a) -> (String -> a)
    -> (Array JsonBigInt -> a) -> (Object JsonBigInt -> a)
    -> JsonBigInt -> a

caseAeson
  :: forall (a :: Type)
   . AesonCases a
  -> Aeson
  -> a
caseAeson
  { caseNull, caseBoolean, caseNumber, caseString, caseArray, caseObject }
  (Aeson json) =
    caseJson caseNull caseBoolean caseNumber caseString ((map Aeson) >>> caseArray) ((map Aeson) >>> caseObject) json

constAesonCases :: forall (a :: Type). a -> AesonCases a
constAesonCases v =
  { caseObject: c
  , caseNull: c
  , caseBoolean: c
  , caseString: c
  , caseNumber: c
  , caseArray: c
  }
  where
  c :: forall (b :: Type). b -> a
  c = const v

caseAesonObject :: forall (a :: Type). a -> (Object Aeson -> a) -> Aeson -> a
caseAesonObject def f = caseAeson (constAesonCases def # _ { caseObject = f })

caseAesonString :: forall (a :: Type). a -> (String -> a) -> Aeson -> a
caseAesonString def f = caseAeson (constAesonCases def # _ { caseString = f })

caseAesonArray :: forall (a :: Type). a -> (Array Aeson -> a) -> Aeson -> a
caseAesonArray def f = caseAeson (constAesonCases def # _ { caseArray = f })

caseAesonBoolean :: forall (a :: Type). a -> (Boolean -> a) -> Aeson -> a
caseAesonBoolean def f = caseAeson (constAesonCases def # _ { caseBoolean = f })

-- | String representation is used to allow users to choose numeric representation downstream.
caseAesonNumber :: forall (a :: Type). a -> (String -> a) -> Aeson -> a
caseAesonNumber def f = caseAeson (constAesonCases def # _ { caseNumber = f })

-- | `caseAesonNumber` specialized to `UInt` (fails if no parse)
caseAesonUInt :: forall (a :: Type). a -> (UInt -> a) -> Aeson -> a
caseAesonUInt def f = caseAesonNumber def \str ->
  case UInt.fromString str of
    Nothing -> def
    Just res -> f res

-- | `caseAesonNumber` specialized to `BigInt` (fails if no parse)
caseAesonBigInt :: forall (a :: Type). a -> (BigInt -> a) -> Aeson -> a
caseAesonBigInt def f = caseAesonNumber def \str ->
  case BigInt.fromString str of
    Nothing -> def
    Just res -> f res

caseAesonNull :: forall (a :: Type). a -> (Unit -> a) -> Aeson -> a
caseAesonNull def f = caseAeson (constAesonCases def # _ { caseNull = f })

verbAesonType :: forall a b. b -> (a -> b) -> (b -> (a -> b) -> Aeson -> b) -> Aeson -> b
verbAesonType def f g = g def f

isAesonType :: forall a. (Boolean -> (a -> Boolean) -> Aeson -> Boolean) -> Aeson -> Boolean
isAesonType = verbAesonType false (const true)

-- | Check if the provided `Json` is the `null` value
isNull :: Aeson -> Boolean
isNull = isAesonType caseAesonNull

-- | Check if the provided `Aeson` is a `Boolean`
isBoolean :: Aeson -> Boolean
isBoolean = isAesonType caseAesonBoolean

-- | Check if the provided `Aeson` is a `Number`
isNumber :: Aeson -> Boolean
isNumber = isAesonType caseAesonNumber

-- | Check if the provided `Aeson` is a `String`
isString :: Aeson -> Boolean
isString = isAesonType caseAesonString

-- | Check if the provided `Aeson` is an `Array`
isArray :: Aeson -> Boolean
isArray = isAesonType caseAesonArray

-- | Check if the provided `Aeson` is an `Object`
isObject :: Aeson -> Boolean
isObject = isAesonType caseAesonObject

toAesonType
  :: forall a
   . (Maybe a -> (a -> Maybe a) -> Aeson -> Maybe a)
  -> Aeson
  -> Maybe a
toAesonType = verbAesonType Nothing Just

-- | Convert `Aeson` to the ``Unit` value if the `Aeson` is the null value
toNull :: Aeson -> Maybe Unit
toNull = toAesonType caseAesonNull

-- | Convert `Aeson` to a `Boolean` value, if the `Aeson` is a boolean.
toBoolean :: Aeson -> Maybe Boolean
toBoolean = toAesonType caseAesonBoolean

-- | Convert `Aeson` to a `Number` value, if the `Aeson` is a number.
toNumber :: Aeson -> Maybe String
toNumber = toAesonType caseAesonNumber

-- | Convert `Aeson` to a `String` value, if the `Aeson` is a string. To write a
-- | `Aeson` value to a JSON string, see `stringify`.
toString :: Aeson -> Maybe String
toString = toAesonType caseAesonString

-- | Convert `Aeson` to an `Array` of `Aeson` values, if the `Aeson` is an array.
toArray :: Aeson -> Maybe (Array Aeson)
toArray = toAesonType caseAesonArray

-- | Convert `Aeson` to an `Object` of `Aeson` values, if the `Aeson` is an object.
toObject :: Aeson -> Maybe (Object Aeson)
toObject = toAesonType caseAesonObject

-- | Construct the `Json` representation of a `String` value.
-- | Note that this function only produces `Json` containing a single piece of `String`
-- | data (similar to `fromBoolean`, `fromNumber`, etc.).
-- | This function does NOT convert the `String` encoding of a JSON value to `Json` - For that
-- | purpose, you'll need to use `jsonParser`.
fromString :: String -> Aeson
fromString str = Aeson $ unsafeCoerceToJsonBigInt str

aesonNull :: Aeson
aesonNull = Aeson $ unsafeCoerceToJsonBigInt null

-------- Decode helpers --------

-- | Decodes a value encoded as JSON via Aeson decoding algorithm.
decodeJsonString
  :: forall (a :: Type). DecodeAeson a => String -> Either JsonDecodeError a
decodeJsonString = parseJsonStringToAeson >=> decodeAeson

-------- DecodeAeson instances --------

decodeNumber
  :: forall a. (String -> Maybe a) -> Aeson -> Either JsonDecodeError a
decodeNumber parse aeson =
  caseAesonNumber typeError ((note (TypeMismatch "TODO")) <<< parse) aeson
    where
      typeError = Left $ TypeMismatch $ "Couldn't parse to integral: "  -- TODO <> numberStr

instance DecodeAeson UInt where
  decodeAeson = decodeNumber UInt.fromString

instance DecodeAeson Int where
  decodeAeson = decodeNumber Int.fromString

instance DecodeAeson BigInt where
  decodeAeson = decodeNumber BigInt.fromString

instance DecodeAeson Number where
  decodeAeson = decodeNumber Number.fromString

instance DecodeAeson Boolean where
  decodeAeson aeson = caseAesonBoolean typeError Right aeson
    where
      typeError = Left $ TypeMismatch $ "Is not Boolean" -- TODO

instance DecodeAeson String where
  decodeAeson aeson = caseAesonString typeError Right aeson
    where
      typeError = Left $ TypeMismatch $ "Is not String" -- TODO

instance DecodeAeson Aeson where
  decodeAeson = pure

instance DecodeAeson a => DecodeAeson (Object a) where
  decodeAeson = caseAesonObject (Left (TypeMismatch "Expected Object"))
    (traverse decodeAeson)

instance (DecodeAeson a, DecodeAeson b) => DecodeAeson (Tuple a b) where
  decodeAeson = caseAesonArray (Left (TypeMismatch "Expected Array (Tuple)"))
    \arr ->
      case arr !! 0, arr !! 1, arr !! 2 of
        Just a, Just b, Nothing ->
          Tuple <$> decodeAeson a <*> decodeAeson b
        _, _, _ ->
          Left (TypeMismatch "Expected Array with length 2")

instance
  ( GDecodeAeson row list
  , RL.RowToList row list
  ) =>
  DecodeAeson (Record row) where
  decodeAeson json =
    case toObject json of
      Just object -> gDecodeAeson object (Proxy :: Proxy list)
      Nothing -> Left $ TypeMismatch "Object"

instance
  ( InOneOf b a b
  , DecodeAeson a
  , DecodeAeson b
  ) =>
  DecodeAeson (a |+| b) where
  decodeAeson j =
    asOneOf <$> (decodeAeson j :: Either JsonDecodeError a)
      <|> asOneOf <$> (decodeAeson j :: Either JsonDecodeError b)

instance DecodeAeson a => DecodeAeson (Array a) where
  decodeAeson aeson = caseAesonArray typeError (traverse decodeAeson) aeson
    where
      typeError :: forall b . Either JsonDecodeError b
      typeError = Left $ TypeMismatch $ "Is not Array" -- TODO

instance DecodeAeson a => DecodeAeson (L.List a) where
  decodeAeson x = toUnfoldable <$> decodeAeson x

instance DecodeAeson a => DecodeAeson (LL.List a) where
  decodeAeson x = toUnfoldable <$> decodeAeson x

instance DecodeAeson a => DecodeAeson (Seq a) where
  decodeAeson x = toUnfoldable <$> decodeAeson x

instance DecodeAeson a => DecodeAeson (Maybe a) where
  decodeAeson aeson =
    caseAeson
    { caseNull: \_ -> Right Nothing
    , caseBoolean: \_ -> Just <$> decodeAeson aeson
    , caseNumber: \_ -> Just <$> decodeAeson aeson
    , caseString: \_ -> Just <$> decodeAeson aeson
    , caseArray: \_ -> Just <$> decodeAeson aeson
    , caseObject: \_ -> Just <$> decodeAeson aeson
    }
    aeson

class
  GDecodeAeson (row :: Row Type) (list :: RL.RowList Type)
  | list -> row where
  gDecodeAeson
    :: forall proxy
     . Object Aeson
    -> proxy list
    -> Either JsonDecodeError (Record row)

instance GDecodeAeson () RL.Nil where
  gDecodeAeson _ _ = Right {}

instance
  ( DecodeAesonField value
  , GDecodeAeson rowTail tail
  , IsSymbol field
  , Row.Cons field value rowTail row
  , Row.Lacks field rowTail
  ) =>
  GDecodeAeson row (RL.Cons field value tail) where
  gDecodeAeson object _ = do
    let
      _field = Proxy :: Proxy field
      fieldName = reflectSymbol _field
      fieldValue = FO.lookup fieldName object

    case decodeAesonField fieldValue of
      Just fieldVal -> do
        val <- lmap (AtKey fieldName) fieldVal
        rest <- gDecodeAeson object (Proxy :: Proxy tail)
        Right $ Record.insert _field val rest

      Nothing ->
        Left $ AtKey fieldName MissingValue

class DecodeAesonField a where
  decodeAesonField :: Maybe Aeson -> Maybe (Either JsonDecodeError a)

instance DecodeAeson a => DecodeAesonField (Maybe a) where
  decodeAesonField = Just <<< maybe (Right Nothing) decodeAeson

else instance DecodeAeson a => DecodeAesonField a where
  decodeAesonField j = decodeAeson <$> j

-------- EncodeAeson --------


-- json-bigint uses regular JS to store JSON internaly
unsafeCoerceToJsonBigInt :: forall a . a -> JsonBigInt
unsafeCoerceToJsonBigInt = unsafeCoerce

class EncodeAeson (a :: Type) where
  encodeAeson :: a -> Aeson

instance EncodeAeson Int where
  encodeAeson = Aeson <<< unsafeCoerceToJsonBigInt

instance EncodeAeson BigInt where
  encodeAeson =
    Aeson <<< unsafeCoerceToJsonBigInt <<< bigNumberFromString <<< BigInt.toString

instance EncodeAeson UInt where
  encodeAeson = encodeAeson <<< UInt.toInt

instance EncodeAeson Number where
  encodeAeson =
    Aeson <<< unsafeCoerceToJsonBigInt <<< bigNumberFromString <<< Number.toString

instance EncodeAeson String where
  encodeAeson = Aeson <<< unsafeCoerceToJsonBigInt

instance EncodeAeson Boolean where
  encodeAeson = Aeson <<< unsafeCoerceToJsonBigInt

instance EncodeAeson Aeson where
  encodeAeson = identity

instance EncodeAeson a => EncodeAeson (Object a) where
  encodeAeson input = Aeson $ unsafeCoerceToJsonBigInt $ map encodeAeson input

instance
  ( GEncodeAeson row list
  , RL.RowToList row list
  ) =>
  EncodeAeson (Record row) where
  encodeAeson rec = encodeAeson $ gEncodeAeson rec (Proxy :: Proxy list)

instance EncodeAeson a => EncodeAeson (Array a) where
  encodeAeson x = Aeson $ unsafeCoerceToJsonBigInt $ map encodeAeson x

instance (EncodeAeson a, EncodeAeson b) => EncodeAeson (Tuple a b) where
  encodeAeson (Tuple a b) = Aeson $ unsafeCoerceToJsonBigInt [ encodeAeson a, encodeAeson b ]

instance EncodeAeson a => EncodeAeson (L.List a) where
  encodeAeson = encodeAeson <<< fromFoldable

instance EncodeAeson a => EncodeAeson (LL.List a) where
  encodeAeson = encodeAeson <<< fromFoldable

instance EncodeAeson a => EncodeAeson (Seq a) where
  encodeAeson = encodeAeson <<< fromFoldable

instance EncodeAeson a => EncodeAeson (Maybe a) where
  encodeAeson Nothing = aesonNull
  encodeAeson (Just a) = encodeAeson a

class GEncodeAeson (row :: Row Type) (list :: RL.RowList Type) where
  gEncodeAeson
    :: forall proxy. Record row -> proxy list -> FO.Object Aeson

instance gEncodeAesonNil :: GEncodeAeson row RL.Nil where
  gEncodeAeson _ _ = FO.empty

instance gEncodeAesonCons ::
  ( EncodeAeson value
  , GEncodeAeson row tail
  , IsSymbol field
  , Row.Cons field value tail' row
  ) =>
  GEncodeAeson row (RL.Cons field value tail) where
  gEncodeAeson row _ = do
    let _field = Proxy :: Proxy field
    FO.insert
      (reflectSymbol _field)
      (encodeAeson $ Record.get _field row)
      (gEncodeAeson row (Proxy :: Proxy tail))
