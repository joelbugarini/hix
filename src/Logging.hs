{-# LANGUAGE OverloadedStrings #-}

module Logging where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (openFile, IOMode(..), hClose, Handle)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import Data.Aeson (ToJSON(..), object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (pack)
import System.FilePath ((</>))
import Control.Monad (when)
import Data.Maybe (fromMaybe)

-- Log levels
data LogLevel = DEBUG | INFO | WARNING | ERROR
    deriving (Show, Eq, Ord)

instance ToJSON LogLevel where
    toJSON level = toJSON (show level)

-- Log entry structure
data LogEntry = LogEntry
    { timestamp :: String
    , level :: LogLevel
    , component :: String
    , message :: Text
    , details :: Maybe Text
    } deriving (Show)

instance ToJSON LogEntry where
    toJSON (LogEntry ts lvl comp msg dets) = object
        [ "timestamp" .= ts
        , "level" .= lvl
        , "component" .= comp
        , "message" .= msg
        , "details" .= dets
        ]

-- Configuration
data LogConfig = LogConfig
    { logLevel :: LogLevel
    , logDir :: FilePath
    , maxLogSize :: Int  -- in bytes
    , maxLogFiles :: Int
    }

defaultConfig :: LogConfig
defaultConfig = LogConfig
    { logLevel = INFO
    , logDir = ".hix/logs"
    , maxLogSize = 5 * 1024 * 1024  -- 5MB
    , maxLogFiles = 5
    }

-- Global state
data LogState = LogState
    { config :: LogConfig
    , currentLogFile :: FilePath
    , currentHandle :: Maybe Handle
    }

-- Initialize logging with configuration
initLogging :: LogConfig -> IO LogState
initLogging config = do
    createDirectoryIfMissing True (logDir config)
    let logFile = logDir config </> "hix.log"
    h <- openFile logFile AppendMode
    return $ LogState config logFile (Just h)

-- Close logging
closeLogging :: LogState -> IO ()
closeLogging state = do
    case currentHandle state of
        Just h -> hClose h
        Nothing -> return ()

-- Rotate logs if needed
rotateLogs :: LogState -> IO LogState
rotateLogs state = do
    exists <- doesFileExist (currentLogFile state)
    if not exists
        then return state
        else do
            size <- fromIntegral . BL.length <$> BL.readFile (currentLogFile state)
            if size >= maxLogSize (config state)
                then do
                    -- Close current handle
                    case currentHandle state of
                        Just h -> hClose h
                        Nothing -> return ()
                    
                    -- Rotate old logs
                    let rotate i = do
                            let oldFile = logDir (config state) </> ("hix." ++ show i ++ ".log")
                            let newFile = logDir (config state) </> ("hix." ++ show (i + 1) ++ ".log")
                            exists <- doesFileExist oldFile
                            when exists $ do
                                if i < maxLogFiles (config state) - 1
                                    then do
                                        exists <- doesFileExist newFile
                                        when exists $ do
                                            BL.writeFile newFile =<< BL.readFile oldFile
                                    else return ()
                                BL.writeFile oldFile BL.empty
                    
                    -- Rotate all log files
                    mapM_ rotate [maxLogFiles (config state) - 1, maxLogFiles (config state) - 2 .. 0]
                    
                    -- Create new log file
                    h <- openFile (currentLogFile state) AppendMode
                    return $ state { currentHandle = Just h }
                else return state

-- Log a message
logMessage :: LogState -> LogLevel -> String -> Text -> Maybe Text -> IO LogState
logMessage state level component message details = do
    if level >= logLevel (config state)
        then do
            timestamp <- getCurrentTime
            let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
            let entry = LogEntry timeStr level component message details
            let jsonEntry = pack $ show (toJSON entry) ++ "\n"
            
            -- Rotate logs if needed
            newState <- rotateLogs state
            
            -- Write log entry
            case currentHandle newState of
                Just h -> do
                    BL.hPutStr h jsonEntry
                    return newState
                Nothing -> return newState
        else return state

-- Convenience logging functions
logDebug :: LogState -> String -> Text -> Maybe Text -> IO LogState
logDebug state = logMessage state DEBUG

logInfo :: LogState -> String -> Text -> Maybe Text -> IO LogState
logInfo state = logMessage state INFO

logWarning :: LogState -> String -> Text -> Maybe Text -> IO LogState
logWarning state = logMessage state WARNING

logError :: LogState -> String -> Text -> Maybe Text -> IO LogState
logError state = logMessage state ERROR

-- Specific logging functions for different components
logTemplateContext :: LogState -> String -> Text -> Text -> IO LogState
logTemplateContext state testName expectedContext detectedContext = do
    let details = Just $ T.pack $ "Expected: " ++ T.unpack expectedContext ++ "\nDetected: " ++ T.unpack detectedContext
    logInfo state "Template" (T.pack $ "Test context: " ++ testName) details

logBlockContext :: LogState -> String -> Text -> IO LogState
logBlockContext state blockName context = do
    logInfo state "Block" (T.pack $ "Block: " ++ blockName) (Just context)

logParserError :: LogState -> String -> Text -> IO LogState
logParserError state template error = do
    logError state "Parser" (T.pack $ "Failed to parse template: " ++ template) (Just error)

logRenderError :: LogState -> String -> Text -> IO LogState
logRenderError state component error = do
    logError state "Renderer" (T.pack $ "Failed to render " ++ component) (Just error) 