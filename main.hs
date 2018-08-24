import System.Environment (getArgs)
import System.IO
import Control.Exception

parseArgs :: [[Char]] -> ([Char],Maybe [Char])
parseArgs (pattern : file : _ ) = (pattern , Just(file))
parseArgs (pattern : _) = (pattern , Nothing)

contain :: [Char] -> [Char] -> Bool
contain pattern text | length pattern > length text = False
                      | pattern == (take (length pattern) text) = True
                      | pattern /= (take (length pattern) text) = (contain pattern (tail text))


matchFile :: [Char] -> [Char] -> IO [[Char]]
matchFile pattern file = do
  string <- readFile file
  let strings = lines string
  let result = filter (\x -> contain pattern x) strings
  return result

stdinMode :: [Char] -> IO()
stdinMode pattern = do
  eof <- isEOF
  if not eof
    then getLine >>= (\line ->
      if contain pattern line
        then evaluate pattern >>=
        (\x -> do
          putStrLn line
          return x >>= stdinMode )
        else evaluate pattern >>= stdinMode)
    else return ()

printList :: [[Char]] -> IO()
printList [] = return ()
printList (x:xs) = putStrLn x >> printList xs

modeChange :: [Char] -> Maybe[Char] -> IO ()
modeChange pattern (Just file) = matchFile pattern file >>= printList
modeChange pattern Nothing = evaluate pattern >>= stdinMode

main :: IO()
main = do
  args <- getArgs
  let (pattern,file) = parseArgs args
  modeChange pattern file
