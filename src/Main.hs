import Text.Regex.Posix
import qualified Data.Classifier as C

trainInput :: C.Classifier -> IO C.Classifier
trainInput cls = do
  str <- getLine
  case str of
    [] -> do
      putStrLn "\nNow classify some strings:\n"
      return cls
    _  -> do
      let match = str =~ "^([^:]+):(.+)$" :: [[String]]
      case match of
        (x:xs) -> trainInput $ C.train cls (x !! 1) (x !! 2)
        _      -> do
          putStrLn "Invalid format, please try again."
          trainInput cls

classifyInput :: C.Classifier -> IO C.Classifier
classifyInput cls = do
  str <- getLine
  case str of
    [] -> return cls
    _  -> do
      putStrLn $ ">> " ++ (show $ C.classify cls str)
      classifyInput cls

main :: IO()
main = do
  putStrLn "Enter training data (i.e. category:text):\n"
  cls <- trainInput C.empty
  classifyInput cls
  putStrLn "Done."
