import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Int                   ( Int64 )
import           Network.HTTP.Conduit       ( simpleHttp )
import           Test.QuickCheck            ( Property
                                            , quickCheckWith
                                            , stdArgs
                                            , (==>)
                                            )
import           Test.QuickCheck.Test       ( maxSuccess )
import           Test.QuickCheck.Monadic    ( assert
                                            , monadicIO
                                            , run
                                            )

expectedLength :: Int64
expectedLength = 128

propHasExpectedLength :: Int -> Property
propHasExpectedLength i =
  i > 0 ==> monadicIO test
    where
      url = "http://127.0.0.1:5000/sha512sum/" ++ show i
      test = do
        response <- run $ simpleHttp url
        let actualLength = LC.length response
        assert $ actualLength == expectedLength

main :: IO ()
main = quickCheckWith stdArgs
                      { maxSuccess = 5 }
                      propHasExpectedLength

