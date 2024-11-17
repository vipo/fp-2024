{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck as QC

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified


instance Arbitrary Lib2.BookGenre where
    arbitrary :: Gen Lib2.BookGenre
    arbitrary = elements [Lib2.Fantasy, Lib2.Scientific, Lib2.Detective, Lib2.Dictionary]

instance Arbitrary Lib2.BookAudience where
    arbitrary :: Gen Lib2.BookAudience
    arbitrary = elements [Lib2.Children, Lib2.Teenager, Lib2.Adult]

instance Arbitrary Lib2.BookInfo where
    arbitrary :: Gen Lib2.BookInfo
    arbitrary = Lib2.BookInfo 
        <$> (listOf1 $ elements ['A'..'Z'])
        <*> (listOf1 $ elements ['A'..'Z']) 
        <*> arbitrary                        
        <*> arbitrary                        

instance Arbitrary Lib2.ReaderInfo where
    arbitrary :: Gen Lib2.ReaderInfo
    arbitrary = Lib2.ReaderInfo
        <$> (listOf1 $ elements ['A'..'Z']) 
        <*> (arbitrary `suchThat` (>= 0))

instance Arbitrary Lib2.Query where
    arbitrary :: Gen Lib2.Query
    arbitrary = oneof
        [ Lib2.AddBookQuery <$> arbitrary
        , Lib2.AddReaderQuery <$> arbitrary
        ]

instance Arbitrary Lib3.Statements where
    arbitrary :: Gen Lib3.Statements
    arbitrary = oneof [Lib3.Single <$> arbitrary, Lib3.Batch <$> listOf arbitrary]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,

    testCase "Parsing empty query" $
      Lib2.parseQuery "" @?= Left "No parser matched",

    testCase "Parsing BorrowQuery" $ 
      Lib2.parseQuery "borrow HarryPotter Rowling Fantasy Children John 123" @?= 
        Right (Lib2.BorrowQuery 
          (Lib2.BookInfo "HarryPotter" "Rowling" Lib2.Fantasy Lib2.Children) 
          (Lib2.ReaderInfo "John" 123)
        ),

    testCase "Parsing ReturnQuery" $ 
      Lib2.parseQuery "return HarryPotter Rowling Fantasy Children John 123" @?= 
        Right (Lib2.ReturnQuery 
          (Lib2.BookInfo "HarryPotter" "Rowling" Lib2.Fantasy Lib2.Children) 
          (Lib2.ReaderInfo "John" 123)
        ),

    testCase "Parsing AddBookQuery" $ 
      Lib2.parseQuery "add-book HarryPotter Rowling Fantasy Children" @?= 
        Right (Lib2.AddBookQuery 
          (Lib2.BookInfo "HarryPotter" "Rowling" Lib2.Fantasy Lib2.Children)
        ),

    testCase "Parsing AddReaderQuery" $ 
      Lib2.parseQuery "add-reader John 123" @?= 
        Right (Lib2.AddReaderQuery 
          (Lib2.ReaderInfo "John" 123)
        ),

    testCase "Parsing RemoveBookQuery" $ 
      Lib2.parseQuery "remove-book HarryPotter Rowling Fantasy Children" @?= 
        Right (Lib2.RemoveBookQuery 
          (Lib2.BookInfo "HarryPotter" "Rowling" Lib2.Fantasy Lib2.Children)
        ),

    testCase "Parsing RemoveReaderQuery" $ 
      Lib2.parseQuery "remove-reader John 123" @?= 
        Right (Lib2.RemoveReaderQuery 
          (Lib2.ReaderInfo "John" 123)
        ),
    
    testCase "Parsing MergeQuery" $ 
      Lib2.parseQuery "merge HarryPotter Rowling Fantasy Children merge LordOfTheRings Tolkien Fantasy Teenager" @?= 
        Right (Lib2.MergeQuery 
          (Lib2.BookInfo "HarryPotter" "Rowling" Lib2.Fantasy Lib2.Children) 
          (Just (Lib2.MergeQuery 
            (Lib2.BookInfo "LordOfTheRings" "Tolkien" Lib2.Fantasy Lib2.Teenager) 
            Nothing))
        ),
        
    testCase "Parsing invalid command" $
      Lib2.parseQuery "invalidCommand" @?= Left "No parser matched"
  ]

propertyTests :: TestTree
propertyTests = testGroup "Lib3 Property Tests"
  [ testCase "Test single" $
        let s = Lib3.Single (Lib2.AddReaderQuery (Lib2.ReaderInfo "John" 1)) 
         in Lib3.parseStatements (Lib3.renderStatements s) @?= Right (s, ""),

      testCase "Test batch" $
        let s1 = Lib2.AddReaderQuery (Lib2.ReaderInfo "John" 1)
            s2 = Lib2.AddBookQuery (Lib2.BookInfo "Hobbit" "Tolkien" Lib2.Fantasy Lib2.Children)
            s3 = Lib2.AddReaderQuery (Lib2.ReaderInfo "Bob" 2)
            b = Lib3.Batch [s1, s2, s3]
         in Lib3.parseStatements (Lib3.renderStatements b) @?= Right (b, ""),

      QC.testProperty "rendered and parsed" $
        \s -> Lib3.parseStatements (Lib3.renderStatements s) == Right (s, "")
       
  ]
