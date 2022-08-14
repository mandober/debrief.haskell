# Data.Aeason

```hs
import Data.Aeason qualified as A
-- Display all 135 possibilities? (y or n)
A..!=                      A.ToJSON1                  A.genericToJSONKey
A..:                       A.ToJSON2                  A.json
A..:!                      A.ToJSONKey                A.json'
A..:?                      A.ToJSONKeyFunction        A.keyModifier
A..=                       A.ToJSONKeyText            A.liftParseJSON
A.<?>                      A.ToJSONKeyValue           A.liftParseJSON2
A.Array                    A.TwoElemArray             A.liftParseJSONList
A.Bool                     A.UntaggedValue            A.liftParseJSONList2
A.DotNetTime               A.Value                    A.liftToEncoding
A.Encoding                 A.Zero                     A.liftToEncoding2
A.Error                    A.allNullaryToStringTag    A.liftToEncodingList
A.FromArgs                 A.camelTo2                 A.liftToEncodingList2
A.FromJSON                 A.constructorTagModifier   A.liftToJSON
A.FromJSON1                A.contentsFieldName        A.liftToJSON2
A.FromJSON2                A.decode                   A.liftToJSONList
A.FromJSONKey              A.decode'                  A.liftToJSONList2
A.FromJSONKeyCoerce        A.decodeFileStrict         A.object
A.FromJSONKeyFunction      A.decodeFileStrict'        A.omitNothingFields
A.FromJSONKeyText          A.decodeStrict             A.pairs
A.FromJSONKeyTextParser    A.decodeStrict'            A.parseIndexedJSON
A.FromJSONKeyValue         A.defaultJSONKeyOptions    A.parseJSON
A.GFromJSON                A.defaultOptions           A.parseJSON1
A.GFromJSONKey             A.defaultTaggedObject      A.parseJSON2
A.GToEncoding              A.eitherDecode             A.parseJSONList
A.GToJSON                  A.eitherDecode'            A.rejectUnknownFields
A.GToJSON'                 A.eitherDecodeFileStrict   A.sumEncoding
A.GToJSONKey               A.eitherDecodeFileStrict'  A.tagFieldName
A.JSONKeyOptions           A.eitherDecodeStrict       A.tagSingleConstructors
A.JSONPath                 A.eitherDecodeStrict'      A.toEncoding
A.Key                      A.encode                   A.toEncoding1
A.KeyValue                 A.encodeFile               A.toEncoding2
A.Null                     A.fieldLabelModifier       A.toEncodingList
A.Number                   A.foldable                 A.toJSON
A.Object                   A.fromDotNetTime           A.toJSON1
A.ObjectWithSingleField    A.fromEncoding             A.toJSON2
A.One                      A.fromJSON                 A.toJSONKey
A.Options                  A.fromJSONKey              A.toJSONKeyList
A.Result                   A.fromJSONKeyList          A.toJSONList
A.Series                   A.genericFromJSONKey       A.unwrapUnaryRecords
A.String                   A.genericLiftParseJSON     A.withArray
A.Success                  A.genericLiftToEncoding    A.withBool
A.SumEncoding              A.genericLiftToJSON        A.withEmbeddedJSON
A.TaggedObject             A.genericParseJSON         A.withObject
A.ToArgs                   A.genericToEncoding        A.withScientific
A.ToJSON                   A.genericToJSON            A.withText
```
