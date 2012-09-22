-- | Uploads vin data to redis
--
-- Usage:
--
-- @
--vin2redis <program> <file>
-- @
--
module Main (
    main
    ) where

import Control.Monad
import qualified Control.Exception as E
import Control.Concurrent
import System.Environment
import System.FilePath

import qualified Vin.Import as Vin
import qualified Vin.Models as Vin
import qualified Vin.Model as Vin

main :: IO ()
main = do
    args@(~([p, f])) <- getArgs

    models' <- Vin.runWithDict "CarModels.json" Vin.models
    models'' <- maybe (error "Unable to load models") return models'

    let
        programs = map Vin.modelProgram models''

    if length args /= 2
        then do
            putStrLn "Usage: vin2redis <program> <file>"
            putStrLn "  where program is one of:"
            mapM_ (putStrLn . ("    " ++)) programs
        else do
            mvar <- newMVar (0, 0)
            _ <- forkIO $ forever $ do
                threadDelay 5000000
                (u, t) <- readMVar mvar
                putStrLn $ "Uploading... Uploaded: " ++ show u ++ " Total processed: " ++ show t

            Vin.importData models'' f "errors.csv" "errors.log" p (Vin.extension $ takeExtension f) (stats mvar)
                `E.catch` onError
            (u, t) <- takeMVar mvar -- blocks forked thread
            mapM_ putStrLn [
                "Uploaded: ", show u,
                "Total processed: ", show t,
                "",
                "Log: errors.log",
                "Invalid data: errors.csv"]

    where
        stats v u t = void $ swapMVar v (u, t)
        onError :: E.SomeException -> IO ()
        onError _ = return ()
