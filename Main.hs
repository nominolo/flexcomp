module Main where

import Prelude hiding ( mapM )
--import Data.Map ( Map )
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad ( when )
import System.Cmd ( system )
import System.FilePath
import System.Exit
import System.Environment
import System.IO
import Data.List ( nub )
import Data.Traversable ( mapM )
import System.Console.GetOpt

import PPrint

import Types ( Label(..) )
import Mips
import MipsCFG
import CFG
import Schedule4
import Liveness
import Mips2Mops
import Dependency
import GraphMap
import DrawSchedule
import Options
import qualified FlexsocTypes as FS
------------------------------------------------------------------------------


main :: IO ()
main = do
    args <- getArgs
 
    -- Parse options, getting a list of option actions
    let (actions, nonOptions, errors) = getOpt Permute options args
 
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return startOptions) actions
 
    let files = nonOptions
    if (Prelude.null files) then do
      prg <- getProgName
      hPutStrLn stderr "No Arguments given"
      hPutStrLn stderr (usageInfo prg options)
      exitWith (ExitFailure 1)
     else
      mapM_ (compile opts) files

{-    let Options { optVerbose = verbose
                , optInput = input
                , optOutput = output   } = opts
 
    when verbose (hPutStrLn stderr "Hello!")
 
    input >>= output -}

compile :: Options -> String -> IO ()
compile opts file = do
  mips <- parseMipsFile file
  withEither mips (error . show) $ \ds -> do
    let ps = parseMips $ simplify ds
    let mipsBlks = psBlocks ps
    let blks = mipsToMops (psBlocks ps) (psGlobals ps)
    let coreLoop = blks M.! (AutoLabel 6)--(NamedLabel "$L50") --Block l (reverse rmops) ex
    let depinfo = depInfo coreLoop
    let tmpl = reverse $ psTemplate ps
          
    flexBlocks <- mapM (\b -> schedule opts b) blks
    
    when (optHTMLOutput opts) $ do 
      htmlIndex "fft" (M.elems blks)

    writeFile (file <.> "flexcompiled") $ do
      unlines $ fillTemplate tmpl (rtnBlock flexBlocks)
    putStrLn $ "Written file: " ++ file <.> "flexcompiled"
    
    --sched <- schedule (blks Map.! NamedLabel "fxpifft")

    --dotty (genDepGraph depinfo) "foo" "pdf" (Just "open")
    --dotty (dottyFlowGraph mipsBlks) "bar" "pdf" (Just "open")
    return ()

rtnBlock :: M.Map Label [[FS.MicroOp]] -> Label -> [String]
rtnBlock bs l = 
    case M.lookup l bs of
      Nothing -> []
      Just is -> map (\l -> "\trtn\t[" ++ show l ++ "]") (init is)

dotty :: String -> String -> String -> Maybe String -> IO ()
dotty dotspec fname ext mopencmd = do
  writeFile (fname <.> "dot") dotspec
  putStrLn "Runnig Dotty ..."
  let cmdline = "dot -T" ++ ext ++ " " ++ fname <.> "dot" ++ " > " ++ fname <.> ext
  putStrLn cmdline
  system $ "dot -T" ++ ext ++ " " ++ fname <.> "dot" ++ " > " ++ fname <.> ext
  case mopencmd of
    Nothing -> return ()
    Just open ->
        system (open ++ " " ++ fname <.> ext) >> return ()
  
htmlIndex fname blks = writeFile ("schedule_" ++ fname ++ ".html") $ unlines $
  [ "<html><body>"
  , "<h1>Scheduled Basic Blocks for " ++ fname ++ "</h1>"
  , "<ul>"
  , concatMap (\b -> "<li><a href=\"schedule" ++ pretty (bbLabel b) ++ ".html\">"
                  ++ pretty (bbLabel b) ++ "</a></li>")
              blks
  , "</ul>"
  , "</body></html>"
  ]


tmp_path = "tmp.s"

withEither (Left a)  k _ = k a
withEither (Right b) _ k = k b
