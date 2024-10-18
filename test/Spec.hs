{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

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
