module Main(main) where
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args) :: Int
  let file = args !! 1
  readLines n file

readLines::Int -> String -> IO()
readLines n name = do
  content <- readFile name
  printLines n $ lines content

printLines::Int -> [String] -> IO()
printLines 0 _ = return()
printLines _ [] = return()
printLines n (x:xs) = do
  putStrLn x
  printLines (n-1) xs
