module Options where

import System.Console.GetOpt
import System.Exit
import System.Environment
import System.IO


data Options = Options  
    { optVerbose     :: Bool
    , optHTMLOutput  :: Bool
    , optBranchDelay :: Int
    , optMultDelay   :: Int
    --, optInput      :: IO String
    --, optOutput     :: String -> IO ()
    } deriving (Eq, Show, Read)

startOptions :: Options
startOptions = 
    Options 
    { optVerbose     = False
    , optHTMLOutput  = False
    , optBranchDelay = 1
    , optMultDelay   = 2
--     , optInput      = getContents
--     , optOutput     = putStr
    }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ 
-- Option "i" ["input"]
--         (ReqArg
--             (\arg opt -> return opt { optInput = readFile arg })
--             "FILE")
--         "Input file"
 
--     , Option "o" ["output"]
--         (ReqArg
--             (\arg opt -> return opt { optOutput = writeFile arg })
--             "FILE")
--         "Output file"
 
--     , Option "s" ["string"]
--         (ReqArg
--             (\arg opt -> return opt { optInput = return arg })
--             "FILE")
--         "Input string"
      Option "" ["html"]
        (NoArg 
            (\opt -> return opt { optHTMLOutput = True }))
        "Generate HTML output summary"

    , Option "" ["mult-delay"]
        (ReqArg (\arg opt -> return opt { optMultDelay = read arg })
                "N")
        "Delay of the multiplier in cycles (default: 2)"

    , Option "" ["branch-delay"]
        (ReqArg (\arg opt -> return opt { optBranchDelay = read arg })
                "N")
        "Number of branch delay slots (default: 1)"
 
    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "Enable verbose messages"
 
    , Option "V" ["version"]
        (NoArg
            (\_ -> do
                hPutStrLn stderr "Version 0.01"
                exitWith ExitSuccess))
        "Print version"
 
    , Option "h" ["help"]
        (NoArg
            (\_ -> do
    	        prg <- getProgName
                hPutStrLn stderr (usageInfo prg options)
                exitWith ExitSuccess))
        "Show help"
    ]
