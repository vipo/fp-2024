import Test.DocTest ( doctest )

main :: IO ()
main = doctest ["-isrc", "src/Lessons/Lesson01.hs"]
