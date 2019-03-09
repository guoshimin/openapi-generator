{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances where

import OpenAPIPetstore.Model
import OpenAPIPetstore.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

-- * Models
 
_AdditionalPropertiesClass_sizedGen :: Int -> Gen (AdditionalPropertiesClass)
_AdditionalPropertiesClass_sizedGen 0 = AdditionalPropertiesClass
  <$> pure Nothing -- additionalPropertiesClassMapProperty :: Maybe (Map.Map String Text)
  <*> pure Nothing -- additionalPropertiesClassMapOfMapProperty :: Maybe (Map.Map String (Map.Map String Text))

_AdditionalPropertiesClass_sizedGen n | n > 0 = AdditionalPropertiesClass
  <$> arbitrary -- additionalPropertiesClassMapProperty :: Maybe (Map.Map String Text)
  <*> arbitrary -- additionalPropertiesClassMapOfMapProperty :: Maybe (Map.Map String (Map.Map String Text))

instance Arbitrary AdditionalPropertiesClass where
  arbitrary = sized _AdditionalPropertiesClass_sizedGen

_Animal_sizedGen :: Int -> Gen (Animal)
_Animal_sizedGen 0 = Animal
  <$> arbitrary -- animalClassName :: Text
  <*> pure Nothing -- animalColor :: Maybe Text

_Animal_sizedGen n | n > 0 = Animal
  <$> arbitrary -- animalClassName :: Text
  <*> arbitrary -- animalColor :: Maybe Text

instance Arbitrary Animal where
  arbitrary = sized _Animal_sizedGen

_ApiResponse_sizedGen :: Int -> Gen (ApiResponse)
_ApiResponse_sizedGen 0 = ApiResponse
  <$> pure Nothing -- apiResponseCode :: Maybe Int
  <*> pure Nothing -- apiResponseType :: Maybe Text
  <*> pure Nothing -- apiResponseMessage :: Maybe Text

_ApiResponse_sizedGen n | n > 0 = ApiResponse
  <$> arbitrary -- apiResponseCode :: Maybe Int
  <*> arbitrary -- apiResponseType :: Maybe Text
  <*> arbitrary -- apiResponseMessage :: Maybe Text

instance Arbitrary ApiResponse where
  arbitrary = sized _ApiResponse_sizedGen

_ArrayOfArrayOfNumberOnly_sizedGen :: Int -> Gen (ArrayOfArrayOfNumberOnly)
_ArrayOfArrayOfNumberOnly_sizedGen 0 = ArrayOfArrayOfNumberOnly
  <$> pure Nothing -- arrayOfArrayOfNumberOnlyArrayArrayNumber :: Maybe [[Double]]

_ArrayOfArrayOfNumberOnly_sizedGen n | n > 0 = ArrayOfArrayOfNumberOnly
  <$> arbitrary -- arrayOfArrayOfNumberOnlyArrayArrayNumber :: Maybe [[Double]]

instance Arbitrary ArrayOfArrayOfNumberOnly where
  arbitrary = sized _ArrayOfArrayOfNumberOnly_sizedGen

_ArrayOfNumberOnly_sizedGen :: Int -> Gen (ArrayOfNumberOnly)
_ArrayOfNumberOnly_sizedGen 0 = ArrayOfNumberOnly
  <$> pure Nothing -- arrayOfNumberOnlyArrayNumber :: Maybe [Double]

_ArrayOfNumberOnly_sizedGen n | n > 0 = ArrayOfNumberOnly
  <$> arbitrary -- arrayOfNumberOnlyArrayNumber :: Maybe [Double]

instance Arbitrary ArrayOfNumberOnly where
  arbitrary = sized _ArrayOfNumberOnly_sizedGen

_ArrayTest_sizedGen :: Int -> Gen (ArrayTest)
_ArrayTest_sizedGen 0 = ArrayTest
  <$> pure Nothing -- arrayTestArrayOfString :: Maybe [Text]
  <*> pure Nothing -- arrayTestArrayArrayOfInteger :: Maybe [[Integer]]
  <*> pure Nothing -- arrayTestArrayArrayOfModel :: Maybe [[ReadOnlyFirst]]

_ArrayTest_sizedGen n | n > 0 = ArrayTest
  <$> arbitrary -- arrayTestArrayOfString :: Maybe [Text]
  <*> arbitrary -- arrayTestArrayArrayOfInteger :: Maybe [[Integer]]
  <*> arbitrary -- arrayTestArrayArrayOfModel :: Maybe [[ReadOnlyFirst]]

instance Arbitrary ArrayTest where
  arbitrary = sized _ArrayTest_sizedGen

_Capitalization_sizedGen :: Int -> Gen (Capitalization)
_Capitalization_sizedGen 0 = Capitalization
  <$> pure Nothing -- capitalizationSmallCamel :: Maybe Text
  <*> pure Nothing -- capitalizationCapitalCamel :: Maybe Text
  <*> pure Nothing -- capitalizationSmallSnake :: Maybe Text
  <*> pure Nothing -- capitalizationCapitalSnake :: Maybe Text
  <*> pure Nothing -- capitalizationScaEthFlowPoints :: Maybe Text
  <*> pure Nothing -- capitalizationAttName :: Maybe Text

_Capitalization_sizedGen n | n > 0 = Capitalization
  <$> arbitrary -- capitalizationSmallCamel :: Maybe Text
  <*> arbitrary -- capitalizationCapitalCamel :: Maybe Text
  <*> arbitrary -- capitalizationSmallSnake :: Maybe Text
  <*> arbitrary -- capitalizationCapitalSnake :: Maybe Text
  <*> arbitrary -- capitalizationScaEthFlowPoints :: Maybe Text
  <*> arbitrary -- capitalizationAttName :: Maybe Text

instance Arbitrary Capitalization where
  arbitrary = sized _Capitalization_sizedGen

_Cat_sizedGen :: Int -> Gen (Cat)
_Cat_sizedGen 0 = Cat
  <$> arbitrary -- catClassName :: Text
  <*> pure Nothing -- catColor :: Maybe Text
  <*> pure Nothing -- catDeclawed :: Maybe Bool

_Cat_sizedGen n | n > 0 = Cat
  <$> arbitrary -- catClassName :: Text
  <*> arbitrary -- catColor :: Maybe Text
  <*> arbitrary -- catDeclawed :: Maybe Bool

instance Arbitrary Cat where
  arbitrary = sized _Cat_sizedGen

_Category_sizedGen :: Int -> Gen (Category)
_Category_sizedGen 0 = Category
  <$> pure Nothing -- categoryId :: Maybe Integer
  <*> arbitrary -- categoryName :: Text

_Category_sizedGen n | n > 0 = Category
  <$> arbitrary -- categoryId :: Maybe Integer
  <*> arbitrary -- categoryName :: Text

instance Arbitrary Category where
  arbitrary = sized _Category_sizedGen

_ClassModel_sizedGen :: Int -> Gen (ClassModel)
_ClassModel_sizedGen 0 = ClassModel
  <$> pure Nothing -- classModelClass :: Maybe Text

_ClassModel_sizedGen n | n > 0 = ClassModel
  <$> arbitrary -- classModelClass :: Maybe Text

instance Arbitrary ClassModel where
  arbitrary = sized _ClassModel_sizedGen

_Client_sizedGen :: Int -> Gen (Client)
_Client_sizedGen 0 = Client
  <$> pure Nothing -- clientClient :: Maybe Text

_Client_sizedGen n | n > 0 = Client
  <$> arbitrary -- clientClient :: Maybe Text

instance Arbitrary Client where
  arbitrary = sized _Client_sizedGen

_Dog_sizedGen :: Int -> Gen (Dog)
_Dog_sizedGen 0 = Dog
  <$> arbitrary -- dogClassName :: Text
  <*> pure Nothing -- dogColor :: Maybe Text
  <*> pure Nothing -- dogBreed :: Maybe Text

_Dog_sizedGen n | n > 0 = Dog
  <$> arbitrary -- dogClassName :: Text
  <*> arbitrary -- dogColor :: Maybe Text
  <*> arbitrary -- dogBreed :: Maybe Text

instance Arbitrary Dog where
  arbitrary = sized _Dog_sizedGen

_EnumArrays_sizedGen :: Int -> Gen (EnumArrays)
_EnumArrays_sizedGen 0 = EnumArrays
  <$> pure Nothing -- enumArraysJustSymbol :: Maybe Text
  <*> pure Nothing -- enumArraysArrayEnum :: Maybe [Text]

_EnumArrays_sizedGen n | n > 0 = EnumArrays
  <$> arbitrary -- enumArraysJustSymbol :: Maybe Text
  <*> arbitrary -- enumArraysArrayEnum :: Maybe [Text]

instance Arbitrary EnumArrays where
  arbitrary = sized _EnumArrays_sizedGen

_EnumTest_sizedGen :: Int -> Gen (EnumTest)
_EnumTest_sizedGen 0 = EnumTest
  <$> pure Nothing -- enumTestEnumString :: Maybe Text
  <*> arbitrary -- enumTestEnumStringRequired :: Text
  <*> pure Nothing -- enumTestEnumInteger :: Maybe Int
  <*> pure Nothing -- enumTestEnumNumber :: Maybe Double
  <*> pure Nothing -- enumTestOuterEnum :: Maybe OuterEnum

_EnumTest_sizedGen n | n > 0 = EnumTest
  <$> arbitrary -- enumTestEnumString :: Maybe Text
  <*> arbitrary -- enumTestEnumStringRequired :: Text
  <*> arbitrary -- enumTestEnumInteger :: Maybe Int
  <*> arbitrary -- enumTestEnumNumber :: Maybe Double
  <*> liftM Just (_OuterEnum_sizedGen (n `div` 2)) -- enumTestOuterEnum :: Maybe OuterEnum

instance Arbitrary EnumTest where
  arbitrary = sized _EnumTest_sizedGen

_File_sizedGen :: Int -> Gen (File)
_File_sizedGen 0 = File
  <$> pure Nothing -- fileSourceUri :: Maybe Text

_File_sizedGen n | n > 0 = File
  <$> arbitrary -- fileSourceUri :: Maybe Text

instance Arbitrary File where
  arbitrary = sized _File_sizedGen

_FileSchemaTestClass_sizedGen :: Int -> Gen (FileSchemaTestClass)
_FileSchemaTestClass_sizedGen 0 = FileSchemaTestClass
  <$> pure Nothing -- fileSchemaTestClassFile :: Maybe File
  <*> pure Nothing -- fileSchemaTestClassFiles :: Maybe [File]

_FileSchemaTestClass_sizedGen n | n > 0 = FileSchemaTestClass
  <$> liftM Just (_File_sizedGen (n `div` 2)) -- fileSchemaTestClassFile :: Maybe File
  <*> arbitrary -- fileSchemaTestClassFiles :: Maybe [File]

instance Arbitrary FileSchemaTestClass where
  arbitrary = sized _FileSchemaTestClass_sizedGen

_FormatTest_sizedGen :: Int -> Gen (FormatTest)
_FormatTest_sizedGen 0 = FormatTest
  <$> pure Nothing -- formatTestInteger :: Maybe Int
  <*> pure Nothing -- formatTestInt32 :: Maybe Int
  <*> pure Nothing -- formatTestInt64 :: Maybe Integer
  <*> arbitrary -- formatTestNumber :: Double
  <*> pure Nothing -- formatTestFloat :: Maybe Float
  <*> pure Nothing -- formatTestDouble :: Maybe Double
  <*> pure Nothing -- formatTestString :: Maybe Text
  <*> arbitrary -- formatTestByte :: ByteArray
  <*> pure Nothing -- formatTestBinary :: Maybe FilePath
  <*> arbitrary -- formatTestDate :: Date
  <*> pure Nothing -- formatTestDateTime :: Maybe DateTime
  <*> pure Nothing -- formatTestUuid :: Maybe Text
  <*> arbitrary -- formatTestPassword :: Text

_FormatTest_sizedGen n | n > 0 = FormatTest
  <$> arbitrary -- formatTestInteger :: Maybe Int
  <*> arbitrary -- formatTestInt32 :: Maybe Int
  <*> arbitrary -- formatTestInt64 :: Maybe Integer
  <*> arbitrary -- formatTestNumber :: Double
  <*> arbitrary -- formatTestFloat :: Maybe Float
  <*> arbitrary -- formatTestDouble :: Maybe Double
  <*> arbitrary -- formatTestString :: Maybe Text
  <*> arbitrary -- formatTestByte :: ByteArray
  <*> arbitrary -- formatTestBinary :: Maybe FilePath
  <*> arbitrary -- formatTestDate :: Date
  <*> arbitrary -- formatTestDateTime :: Maybe DateTime
  <*> arbitrary -- formatTestUuid :: Maybe Text
  <*> arbitrary -- formatTestPassword :: Text

instance Arbitrary FormatTest where
  arbitrary = sized _FormatTest_sizedGen

_HasOnlyReadOnly_sizedGen :: Int -> Gen (HasOnlyReadOnly)
_HasOnlyReadOnly_sizedGen 0 = HasOnlyReadOnly
  <$> pure Nothing -- hasOnlyReadOnlyBar :: Maybe Text
  <*> pure Nothing -- hasOnlyReadOnlyFoo :: Maybe Text

_HasOnlyReadOnly_sizedGen n | n > 0 = HasOnlyReadOnly
  <$> arbitrary -- hasOnlyReadOnlyBar :: Maybe Text
  <*> arbitrary -- hasOnlyReadOnlyFoo :: Maybe Text

instance Arbitrary HasOnlyReadOnly where
  arbitrary = sized _HasOnlyReadOnly_sizedGen

_MapTest_sizedGen :: Int -> Gen (MapTest)
_MapTest_sizedGen 0 = MapTest
  <$> pure Nothing -- mapTestMapMapOfString :: Maybe (Map.Map String (Map.Map String Text))
  <*> pure Nothing -- mapTestMapOfEnumString :: Maybe (Map.Map String Text)
  <*> pure Nothing -- mapTestDirectMap :: Maybe (Map.Map String Bool)
  <*> pure Nothing -- mapTestIndirectMap :: Maybe (Map.Map String Bool)

_MapTest_sizedGen n | n > 0 = MapTest
  <$> arbitrary -- mapTestMapMapOfString :: Maybe (Map.Map String (Map.Map String Text))
  <*> arbitrary -- mapTestMapOfEnumString :: Maybe (Map.Map String Text)
  <*> arbitrary -- mapTestDirectMap :: Maybe (Map.Map String Bool)
  <*> arbitrary -- mapTestIndirectMap :: Maybe (Map.Map String Bool)

instance Arbitrary MapTest where
  arbitrary = sized _MapTest_sizedGen

_MixedPropertiesAndAdditionalPropertiesClass_sizedGen :: Int -> Gen (MixedPropertiesAndAdditionalPropertiesClass)
_MixedPropertiesAndAdditionalPropertiesClass_sizedGen 0 = MixedPropertiesAndAdditionalPropertiesClass
  <$> pure Nothing -- mixedPropertiesAndAdditionalPropertiesClassUuid :: Maybe Text
  <*> pure Nothing -- mixedPropertiesAndAdditionalPropertiesClassDateTime :: Maybe DateTime
  <*> pure Nothing -- mixedPropertiesAndAdditionalPropertiesClassMap :: Maybe (Map.Map String Animal)

_MixedPropertiesAndAdditionalPropertiesClass_sizedGen n | n > 0 = MixedPropertiesAndAdditionalPropertiesClass
  <$> arbitrary -- mixedPropertiesAndAdditionalPropertiesClassUuid :: Maybe Text
  <*> arbitrary -- mixedPropertiesAndAdditionalPropertiesClassDateTime :: Maybe DateTime
  <*> arbitrary -- mixedPropertiesAndAdditionalPropertiesClassMap :: Maybe (Map.Map String Animal)

instance Arbitrary MixedPropertiesAndAdditionalPropertiesClass where
  arbitrary = sized _MixedPropertiesAndAdditionalPropertiesClass_sizedGen

_Model200Response_sizedGen :: Int -> Gen (Model200Response)
_Model200Response_sizedGen 0 = Model200Response
  <$> pure Nothing -- model200ResponseName :: Maybe Int
  <*> pure Nothing -- model200ResponseClass :: Maybe Text

_Model200Response_sizedGen n | n > 0 = Model200Response
  <$> arbitrary -- model200ResponseName :: Maybe Int
  <*> arbitrary -- model200ResponseClass :: Maybe Text

instance Arbitrary Model200Response where
  arbitrary = sized _Model200Response_sizedGen

_ModelList_sizedGen :: Int -> Gen (ModelList)
_ModelList_sizedGen 0 = ModelList
  <$> pure Nothing -- modelList123list :: Maybe Text

_ModelList_sizedGen n | n > 0 = ModelList
  <$> arbitrary -- modelList123list :: Maybe Text

instance Arbitrary ModelList where
  arbitrary = sized _ModelList_sizedGen

_ModelReturn_sizedGen :: Int -> Gen (ModelReturn)
_ModelReturn_sizedGen 0 = ModelReturn
  <$> pure Nothing -- modelReturnReturn :: Maybe Int

_ModelReturn_sizedGen n | n > 0 = ModelReturn
  <$> arbitrary -- modelReturnReturn :: Maybe Int

instance Arbitrary ModelReturn where
  arbitrary = sized _ModelReturn_sizedGen

_Name_sizedGen :: Int -> Gen (Name)
_Name_sizedGen 0 = Name
  <$> arbitrary -- nameName :: Int
  <*> pure Nothing -- nameSnakeCase :: Maybe Int
  <*> pure Nothing -- nameProperty :: Maybe Text
  <*> pure Nothing -- name123number :: Maybe Int

_Name_sizedGen n | n > 0 = Name
  <$> arbitrary -- nameName :: Int
  <*> arbitrary -- nameSnakeCase :: Maybe Int
  <*> arbitrary -- nameProperty :: Maybe Text
  <*> arbitrary -- name123number :: Maybe Int

instance Arbitrary Name where
  arbitrary = sized _Name_sizedGen

_NumberOnly_sizedGen :: Int -> Gen (NumberOnly)
_NumberOnly_sizedGen 0 = NumberOnly
  <$> pure Nothing -- numberOnlyJustNumber :: Maybe Double

_NumberOnly_sizedGen n | n > 0 = NumberOnly
  <$> arbitrary -- numberOnlyJustNumber :: Maybe Double

instance Arbitrary NumberOnly where
  arbitrary = sized _NumberOnly_sizedGen

_Order_sizedGen :: Int -> Gen (Order)
_Order_sizedGen 0 = Order
  <$> pure Nothing -- orderId :: Maybe Integer
  <*> pure Nothing -- orderPetId :: Maybe Integer
  <*> pure Nothing -- orderQuantity :: Maybe Int
  <*> pure Nothing -- orderShipDate :: Maybe DateTime
  <*> pure Nothing -- orderStatus :: Maybe Text
  <*> pure Nothing -- orderComplete :: Maybe Bool

_Order_sizedGen n | n > 0 = Order
  <$> arbitrary -- orderId :: Maybe Integer
  <*> arbitrary -- orderPetId :: Maybe Integer
  <*> arbitrary -- orderQuantity :: Maybe Int
  <*> arbitrary -- orderShipDate :: Maybe DateTime
  <*> arbitrary -- orderStatus :: Maybe Text
  <*> arbitrary -- orderComplete :: Maybe Bool

instance Arbitrary Order where
  arbitrary = sized _Order_sizedGen

_OuterComposite_sizedGen :: Int -> Gen (OuterComposite)
_OuterComposite_sizedGen 0 = OuterComposite
  <$> pure Nothing -- outerCompositeMyNumber :: Maybe Double
  <*> pure Nothing -- outerCompositeMyString :: Maybe Text
  <*> pure Nothing -- outerCompositeMyBoolean :: Maybe Bool

_OuterComposite_sizedGen n | n > 0 = OuterComposite
  <$> arbitrary -- outerCompositeMyNumber :: Maybe Double
  <*> arbitrary -- outerCompositeMyString :: Maybe Text
  <*> arbitrary -- outerCompositeMyBoolean :: Maybe Bool

instance Arbitrary OuterComposite where
  arbitrary = sized _OuterComposite_sizedGen

_Pet_sizedGen :: Int -> Gen (Pet)
_Pet_sizedGen 0 = Pet
  <$> pure Nothing -- petId :: Maybe Integer
  <*> pure Nothing -- petCategory :: Maybe Category
  <*> arbitrary -- petName :: Text
  <*> arbitrary -- petPhotoUrls :: [Text]
  <*> pure Nothing -- petTags :: Maybe [Tag]
  <*> pure Nothing -- petStatus :: Maybe Text

_Pet_sizedGen n | n > 0 = Pet
  <$> arbitrary -- petId :: Maybe Integer
  <*> liftM Just (_Category_sizedGen (n `div` 2)) -- petCategory :: Maybe Category
  <*> arbitrary -- petName :: Text
  <*> arbitrary -- petPhotoUrls :: [Text]
  <*> arbitrary -- petTags :: Maybe [Tag]
  <*> arbitrary -- petStatus :: Maybe Text

instance Arbitrary Pet where
  arbitrary = sized _Pet_sizedGen

_ReadOnlyFirst_sizedGen :: Int -> Gen (ReadOnlyFirst)
_ReadOnlyFirst_sizedGen 0 = ReadOnlyFirst
  <$> pure Nothing -- readOnlyFirstBar :: Maybe Text
  <*> pure Nothing -- readOnlyFirstBaz :: Maybe Text

_ReadOnlyFirst_sizedGen n | n > 0 = ReadOnlyFirst
  <$> arbitrary -- readOnlyFirstBar :: Maybe Text
  <*> arbitrary -- readOnlyFirstBaz :: Maybe Text

instance Arbitrary ReadOnlyFirst where
  arbitrary = sized _ReadOnlyFirst_sizedGen

_SpecialModelName_sizedGen :: Int -> Gen (SpecialModelName)
_SpecialModelName_sizedGen 0 = SpecialModelName
  <$> pure Nothing -- specialModelNameSpecialPropertyName :: Maybe Integer

_SpecialModelName_sizedGen n | n > 0 = SpecialModelName
  <$> arbitrary -- specialModelNameSpecialPropertyName :: Maybe Integer

instance Arbitrary SpecialModelName where
  arbitrary = sized _SpecialModelName_sizedGen

_Tag_sizedGen :: Int -> Gen (Tag)
_Tag_sizedGen 0 = Tag
  <$> pure Nothing -- tagId :: Maybe Integer
  <*> pure Nothing -- tagName :: Maybe Text

_Tag_sizedGen n | n > 0 = Tag
  <$> arbitrary -- tagId :: Maybe Integer
  <*> arbitrary -- tagName :: Maybe Text

instance Arbitrary Tag where
  arbitrary = sized _Tag_sizedGen

_TypeHolderDefault_sizedGen :: Int -> Gen (TypeHolderDefault)
_TypeHolderDefault_sizedGen 0 = TypeHolderDefault
  <$> arbitrary -- typeHolderDefaultStringItem :: Text
  <*> arbitrary -- typeHolderDefaultNumberItem :: Double
  <*> arbitrary -- typeHolderDefaultIntegerItem :: Int
  <*> arbitrary -- typeHolderDefaultBoolItem :: Bool
  <*> arbitrary -- typeHolderDefaultArrayItem :: [Int]

_TypeHolderDefault_sizedGen n | n > 0 = TypeHolderDefault
  <$> arbitrary -- typeHolderDefaultStringItem :: Text
  <*> arbitrary -- typeHolderDefaultNumberItem :: Double
  <*> arbitrary -- typeHolderDefaultIntegerItem :: Int
  <*> arbitrary -- typeHolderDefaultBoolItem :: Bool
  <*> arbitrary -- typeHolderDefaultArrayItem :: [Int]

instance Arbitrary TypeHolderDefault where
  arbitrary = sized _TypeHolderDefault_sizedGen

_TypeHolderExample_sizedGen :: Int -> Gen (TypeHolderExample)
_TypeHolderExample_sizedGen 0 = TypeHolderExample
  <$> arbitrary -- typeHolderExampleStringItem :: Text
  <*> arbitrary -- typeHolderExampleNumberItem :: Double
  <*> arbitrary -- typeHolderExampleIntegerItem :: Int
  <*> arbitrary -- typeHolderExampleBoolItem :: Bool
  <*> arbitrary -- typeHolderExampleArrayItem :: [Int]

_TypeHolderExample_sizedGen n | n > 0 = TypeHolderExample
  <$> arbitrary -- typeHolderExampleStringItem :: Text
  <*> arbitrary -- typeHolderExampleNumberItem :: Double
  <*> arbitrary -- typeHolderExampleIntegerItem :: Int
  <*> arbitrary -- typeHolderExampleBoolItem :: Bool
  <*> arbitrary -- typeHolderExampleArrayItem :: [Int]

instance Arbitrary TypeHolderExample where
  arbitrary = sized _TypeHolderExample_sizedGen

_User_sizedGen :: Int -> Gen (User)
_User_sizedGen 0 = User
  <$> pure Nothing -- userId :: Maybe Integer
  <*> pure Nothing -- userUsername :: Maybe Text
  <*> pure Nothing -- userFirstName :: Maybe Text
  <*> pure Nothing -- userLastName :: Maybe Text
  <*> pure Nothing -- userEmail :: Maybe Text
  <*> pure Nothing -- userPassword :: Maybe Text
  <*> pure Nothing -- userPhone :: Maybe Text
  <*> pure Nothing -- userUserStatus :: Maybe Int

_User_sizedGen n | n > 0 = User
  <$> arbitrary -- userId :: Maybe Integer
  <*> arbitrary -- userUsername :: Maybe Text
  <*> arbitrary -- userFirstName :: Maybe Text
  <*> arbitrary -- userLastName :: Maybe Text
  <*> arbitrary -- userEmail :: Maybe Text
  <*> arbitrary -- userPassword :: Maybe Text
  <*> arbitrary -- userPhone :: Maybe Text
  <*> arbitrary -- userUserStatus :: Maybe Int

instance Arbitrary User where
  arbitrary = sized _User_sizedGen

_XmlItem_sizedGen :: Int -> Gen (XmlItem)
_XmlItem_sizedGen 0 = XmlItem
  <$> pure Nothing -- xmlItemAttributeString :: Maybe Text
  <*> pure Nothing -- xmlItemAttributeNumber :: Maybe Double
  <*> pure Nothing -- xmlItemAttributeInteger :: Maybe Int
  <*> pure Nothing -- xmlItemAttributeBoolean :: Maybe Bool
  <*> pure Nothing -- xmlItemWrappedArray :: Maybe [Int]
  <*> pure Nothing -- xmlItemNameString :: Maybe Text
  <*> pure Nothing -- xmlItemNameNumber :: Maybe Double
  <*> pure Nothing -- xmlItemNameInteger :: Maybe Int
  <*> pure Nothing -- xmlItemNameBoolean :: Maybe Bool
  <*> pure Nothing -- xmlItemNameArray :: Maybe [Int]
  <*> pure Nothing -- xmlItemNameWrappedArray :: Maybe [Int]
  <*> pure Nothing -- xmlItemPrefixString :: Maybe Text
  <*> pure Nothing -- xmlItemPrefixNumber :: Maybe Double
  <*> pure Nothing -- xmlItemPrefixInteger :: Maybe Int
  <*> pure Nothing -- xmlItemPrefixBoolean :: Maybe Bool
  <*> pure Nothing -- xmlItemPrefixArray :: Maybe [Int]
  <*> pure Nothing -- xmlItemPrefixWrappedArray :: Maybe [Int]
  <*> pure Nothing -- xmlItemNamespaceString :: Maybe Text
  <*> pure Nothing -- xmlItemNamespaceNumber :: Maybe Double
  <*> pure Nothing -- xmlItemNamespaceInteger :: Maybe Int
  <*> pure Nothing -- xmlItemNamespaceBoolean :: Maybe Bool
  <*> pure Nothing -- xmlItemNamespaceArray :: Maybe [Int]
  <*> pure Nothing -- xmlItemNamespaceWrappedArray :: Maybe [Int]
  <*> pure Nothing -- xmlItemPrefixNsString :: Maybe Text
  <*> pure Nothing -- xmlItemPrefixNsNumber :: Maybe Double
  <*> pure Nothing -- xmlItemPrefixNsInteger :: Maybe Int
  <*> pure Nothing -- xmlItemPrefixNsBoolean :: Maybe Bool
  <*> pure Nothing -- xmlItemPrefixNsArray :: Maybe [Int]
  <*> pure Nothing -- xmlItemPrefixNsWrappedArray :: Maybe [Int]

_XmlItem_sizedGen n | n > 0 = XmlItem
  <$> arbitrary -- xmlItemAttributeString :: Maybe Text
  <*> arbitrary -- xmlItemAttributeNumber :: Maybe Double
  <*> arbitrary -- xmlItemAttributeInteger :: Maybe Int
  <*> arbitrary -- xmlItemAttributeBoolean :: Maybe Bool
  <*> arbitrary -- xmlItemWrappedArray :: Maybe [Int]
  <*> arbitrary -- xmlItemNameString :: Maybe Text
  <*> arbitrary -- xmlItemNameNumber :: Maybe Double
  <*> arbitrary -- xmlItemNameInteger :: Maybe Int
  <*> arbitrary -- xmlItemNameBoolean :: Maybe Bool
  <*> arbitrary -- xmlItemNameArray :: Maybe [Int]
  <*> arbitrary -- xmlItemNameWrappedArray :: Maybe [Int]
  <*> arbitrary -- xmlItemPrefixString :: Maybe Text
  <*> arbitrary -- xmlItemPrefixNumber :: Maybe Double
  <*> arbitrary -- xmlItemPrefixInteger :: Maybe Int
  <*> arbitrary -- xmlItemPrefixBoolean :: Maybe Bool
  <*> arbitrary -- xmlItemPrefixArray :: Maybe [Int]
  <*> arbitrary -- xmlItemPrefixWrappedArray :: Maybe [Int]
  <*> arbitrary -- xmlItemNamespaceString :: Maybe Text
  <*> arbitrary -- xmlItemNamespaceNumber :: Maybe Double
  <*> arbitrary -- xmlItemNamespaceInteger :: Maybe Int
  <*> arbitrary -- xmlItemNamespaceBoolean :: Maybe Bool
  <*> arbitrary -- xmlItemNamespaceArray :: Maybe [Int]
  <*> arbitrary -- xmlItemNamespaceWrappedArray :: Maybe [Int]
  <*> arbitrary -- xmlItemPrefixNsString :: Maybe Text
  <*> arbitrary -- xmlItemPrefixNsNumber :: Maybe Double
  <*> arbitrary -- xmlItemPrefixNsInteger :: Maybe Int
  <*> arbitrary -- xmlItemPrefixNsBoolean :: Maybe Bool
  <*> arbitrary -- xmlItemPrefixNsArray :: Maybe [Int]
  <*> arbitrary -- xmlItemPrefixNsWrappedArray :: Maybe [Int]

instance Arbitrary XmlItem where
  arbitrary = sized _XmlItem_sizedGen




instance Arbitrary E'ArrayEnum where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'EnumFormString where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'EnumFormStringArray where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'EnumInteger where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'EnumNumber where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'EnumQueryInteger where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'EnumString where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'JustSymbol where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Status2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary EnumClass where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary OuterEnum where
  arbitrary = arbitraryBoundedEnum

