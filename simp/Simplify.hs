import Control.Applicative
import Sat

main :: IO ()
main = putStr =<< printDimacs . simplify . parseDimacs <$> getContents