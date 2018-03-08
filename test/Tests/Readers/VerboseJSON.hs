module Tests.Readers.VerboseJSON (tests) where

import Data.Aeson.Types as JSON
import Text.Pandoc.Definition
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Readers.VerboseJSON
import Test.Tasty
import Test.Tasty.QuickCheck

clearNote :: Inline -> Inline
clearNote = walk f
    where
        f (Note _) = Space
        f x = x

testParseInline :: Inline -> Bool
testParseInline v = parse parseInline x == fromJSON x
    where
        x = toJSON $ clearNote v

testParseBlock :: Block -> Bool
testParseBlock v = parse parseBlock x == fromJSON x
    where
        x = toJSON v

testParsePandoc :: Pandoc -> Bool
testParsePandoc v = parse parsePandoc x == fromJSON x
    where
        x = toJSON v

-- TODO: need arbitrary instance for Value
-- testExactParsing :: JSON.Value -> Bool
-- testExactParsing v =
--     (parseEither parsePandoc v) `sameAs` (parseEither parseJSON v)
--     where
--         sameAs (Right x) (Right y) = x == y
--         sameAs (Left _) (Left _) = True -- Error messages may differ (more verbose)
--         sameAs _ _ = False

tests :: [TestTree]
tests = map (localOption (QuickCheckTests 1000))
    [ testProperty "parse valid Inline JSON" testParseInline
    , testProperty "parse valid Block JSON" testParseBlock
    , testProperty "parse valid Pandoc JSON" testParsePandoc
    ]
