import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as ByteString (length)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 as ByteString (unpack)
import Data.Conduit (ConduitM, ($=+), ($$+-), await, yield)
import Data.Conduit.List (sinkNull)
import Network.HTTP.Conduit (http, parseUrl, responseBody, responseHeaders,
                             withManager)
import Network.HTTP.Types (hContentLength)
import System.Console.AsciiProgress (ProgressBar, Options(..), complete, def,
                                     newProgressBar, tickN)

main :: IO ()
main = withManager $ \manager -> do
    -- Start the request
    req <- parseUrl "https://i.imgur.com/8CJGhZQ.gif"
    res <- http req manager
    -- Get the Content-Length and initialize the progress bar
    let Just cl = lookup hContentLength (responseHeaders res)
    pg <- liftIO $ newProgressBar def { pgTotal = read (ByteString.unpack cl)
                                      , pgWidth = 100
                                      }
    -- Consume the response updating the progress bar
    responseBody res $=+ updateProgress pg $$+- sinkNull
    -- Force the progress bar to complete
    liftIO $ complete pg
    liftIO $ putStrLn "Done"

updateProgress :: MonadIO m => ProgressBar -> ConduitM ByteString ByteString m ()
updateProgress pg = await >>= maybe (return ()) (\chunk -> do
    let len = ByteString.length chunk
    liftIO $ tickN pg len
    yield chunk
    updateProgress pg)
