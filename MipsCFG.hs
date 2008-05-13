{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards #-}
module MipsCFG 
{-  ( CFG
  , BasicBlock(..)
  )  -}
where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Set ( Set )
import Data.Map ( Map )
import Data.List ( foldl', intercalate )
import Data.Maybe ( fromMaybe )

import Control.Monad ( when, mapM_ )
import Control.Monad.State ( State, modify, get, gets, put, execState,
                             MonadState )

import GraphMap as GM
import IDGen

import CFG
import Text.PrettyPrint.HughesPJ as PP
import PPrint

import Mips
import Types ( Instr(..), Reg(..), ALUOp(..), LSWidth(..), 
               BCond(..), Sign(..), Label(..) )
import GPP ( ASMInstr, GPPInstr, Const(..), Instruction, uses, defs, isBranch
           , branchTarget, BranchTarget(..), BranchType(..) )

import Debug.Trace

import qualified WriteDotGraph as Dot

import Test.QuickCheck
import Control.Monad


------------------------------------------------------------------------------
-- Parses a GNU MIPS Assembly program according to the rules from:
--
-- http://www.gnu.org/software/binutils/manual/gas-2.9.1/html_chapter/as_7.html#SEC67
--

data MipsTemplate = MipsTemplate
    { mtBlocks  :: Map Label (BasicBlock ASMInstr)
    , mtGlobals :: Set Label
    , mtTemplate :: [([MipsDirective],Label)]
    }


data Program i n = Program
    { progData :: [Data]
    , progCode :: CFG i n
    }

data Data = DLabel String
	  | DAscii String
	  | DWord Const
	  | DHalfWord Const
	  | DByte Const
	  | DChar Char
	  | DStruct [Data]
	  | DArray Data Int
	  | DAlign Int
	  | DAbsPos Int
   deriving (Eq, Show)

{-
liveIn :: Instruction i r => Set r -> BasicBlock i -> Set r
liveIn out0 bb =
    foldl' accum out0 (reverse (bbInstrs bb))
  where
    accum out i = (out S.\\ defs i) `S.union` uses i
-}
mkBasicBlock l instrs ex = BB l instrs ex

-- | Live-out registers for a block that exits a function.  These are all the
-- callee-save registers and the function result.
exitLiveOuts :: Set Reg 
exitLiveOuts = 
    S.fromList $ map Reg [2,3,16,17,18,19,20,21,22,23,28,29,30,31]
{-
data Program i r = Program 
    { globals :: Map Label (CFG i r) }
-}
--isReturn :: Instr Reg a -> Bool
isReturn (JR (Reg 31)) = True
isReturn _ = False


------------------------------------------------------------------------------

-- | Map with lookahead
mapM'_ :: Monad m => (a -> Maybe a -> m ()) -> [a] -> m ()
mapM'_ f [] = return ()
mapM'_ f [x] = f x Nothing
mapM'_ f (x:xs@(x':_)) = f x (Just x') >> mapM'_ f xs

data ParseMode = CodeMode | DataMode deriving (Eq, Show)

data ParseState = PS { --psFile    :: String
--    psIds     :: [Int], 
    psMode    :: ParseMode,
    psCode    :: GraphMap Label (),
    psBlocks  :: Map Label (BasicBlock ASMInstr),
    psGlobals :: Set Label,
    currLabel :: Maybe Label,
    currBlock :: [ASMInstr],
    psTemplate :: [([MipsDirective],Label)],
    currTemplate :: Maybe [MipsDirective]
  }

type PS a = IDGenT (State ParseState) a

emptyParseState = 
    PS { psMode = CodeMode
       , psCode = GM.empty
       , psBlocks = M.empty
       , psGlobals = S.empty
       , currLabel = Nothing --Just (NamedLabel "__entry__")
       --, currBlockIndex
       , currBlock = []
       , psTemplate = []
       , currTemplate = Nothing
       }

--genId :: PS Int
--genId = do i <- gets (head . psIds 

whenCode :: PS () -> PS ()
whenCode m = do
  pm <- gets psMode
  when (pm == CodeMode) m

whenData :: PS () -> PS ()
whenData m = do
  pm <- gets psMode
  when (pm == DataMode) m

setMode :: ParseMode -> PS ()
setMode m = modify $ \ps -> ps { psMode = m }

addGlobal l = 
    modify $ \s -> s { psGlobals = S.insert l (psGlobals s) }

genLabel :: PS Label
genLabel = genID >>= return . AutoLabel

-- XXX: this probably shouldn't finish the current block
setLabel :: Label -> PS ()
setLabel l = do --trace ("setting label: " ++ show l)$  do
  mcurr <- gets currLabel
  case mcurr of
    Just l' -> do
      -- we are currently in a block, so finish this one first and add an
      -- implicit jump to this new label
      finishBlock (ImplicitJump l)
    Nothing -> return ()
  modify $ \ps -> ps { currLabel = Just l }

finishBlock :: Exit (ASMInstr) -> PS ()
finishBlock ex = do
    gets currLabel >>= (maybe (return ()) $ \l ->
        modify $ \ps -> 
          let bb = mkBasicBlock l (currBlock ps) ex S.empty () in
          ps { psBlocks = M.insert l bb (psBlocks ps)
             , currBlock = []
             , currLabel = Nothing
             })


addInstr :: ASMInstr -> PS ()
addInstr i | isBranch i = error "Branches must be treated specially."
addInstr i = modify $ \ps -> ps { currBlock = i : currBlock ps }

addBranch :: ASMInstr -> Maybe String -> PS ()
addBranch i nextl = do
    
    case branchTarget i of
      Unconditional t -> do
          finishBlock (ExBranch (target t) i)
          noBlock 
          maybe (return ()) (setLabel . NamedLabel) nextl
      Conditional t -> do
          l <- maybe genLabel (return . NamedLabel) nextl
          finishBlock (ExCBranch (target t) l i)
          noBlock >> setLabel l
      -- for now, calls end the current basic block
      Call t -> do
          l <- maybe genLabel (return . NamedLabel) nextl
          finishBlock (ExCall (target t) l i)
          noBlock >> setLabel l
  where 
    noBlock = modify $ \ps -> ps { currLabel = Nothing }
    target (Direct (Addr n)) = Just (NamedLabel n)
    target _ = Nothing
    
branchToExit :: Monad m => ASMInstr -> m Label -> m (Exit (ASMInstr))
branchToExit i genLab = 
    case branchTarget i of
      Unconditional t -> return (ExBranch (target t) i)
      Conditional t -> genLab >>= return . \l -> (ExCBranch (target t) l i)
      Call t -> genLab >>= return . \l -> (ExCall (target t) l i)
  where
    target (Direct (Addr n)) = Just (NamedLabel n)
    target _ = Nothing

reorderBranches :: [MipsDirective] -> [MipsDirective]
reorderBranches ds = f (zip [1..] ds)
  where 
    f ((_,Instr j) : (_,Instr i) : r) 
      | isBranch j = Instr i : Instr j : f r
    f ((n,Instr j) : r) 
      | isBranch j = error $ "Line: " ++ show n ++ "Missing instruction in branch delay slot."
    f ((_,d) : r) = d : f r
    f [] = []

finishTemplate :: PS ()
finishTemplate = 
    modify $ \s -> 
        s { currTemplate = Nothing
          , psTemplate = (fromMaybe id $ do l <- currLabel s
                                            t <- (currTemplate s `mplus` Just [])
                                            return ((reverse t,l):))
                         $ psTemplate s }

parseMips :: [MipsDirective] -> ParseState
parseMips dirs = execState (runIDGenT 1 $ w (reorderBranches dirs)) emptyParseState
  where
    walk ds@(p@(Pragma _ _) : _) = do
        modify $ \s -> s { currTemplate = (p:) `fmap` (currTemplate s `mplus` Just [])}
        w ds
    walk ds@(l@(MDLabel _) : _) = do
        modify $ \s -> s { currTemplate = (l:) `fmap` (currTemplate s `mplus` Just [])}
        w ds
   
    walk ds@(i@(Instr j) : r) 
         | isBranch j = finishTemplate >> w ds
         | (MDLabel _:r) <- r = finishTemplate >> w ds
         | otherwise = w ds
    walk [] = do
        finishTemplate
        w []

    w [] = finishBlock Exit >> return []
    w (Pragma pragma args : r) = do
      case pragma of
        datasect | datasect `elem` ["section", "data", "rdata", "kdata"] 
               -> setMode DataMode
        "text" -> setMode CodeMode
        glob | glob `elem` ["global", "globl"] 
               -> mapM_ (addGlobal . NamedLabel) args
        _      -> return ()
      walk r
    w (MDLabel l : r) = do whenCode $ setLabel (NamedLabel l)
                           walk r
    w (Instr j : r@(MDLabel l : _r))
      | isBranch j  = addBranch j (Just l) >> walk r
    w (Instr j : r)
      | isBranch j  = addBranch j Nothing >> walk r
    w (Instr i : r) = addInstr i >> walk r
    --w (_:r) = w r

fillTemplate :: [([MipsDirective],Label)] -> (Label -> [String]) -> [String]
fillTemplate tmpl f = 
    [ line | (dirs, lbl) <- tmpl
           , codelines <- [f lbl]
           , lines <- [map prettyDirective dirs, codelines]
           , line <- lines ]
  where
    prettyDirective (Pragma n args) =
        "\t." ++ n ++ "\t" ++ intercalate ", " args
    prettyDirective (MDLabel l) =
        l ++ ":"
    prettyDirective d = error $ "bad template directive: " ++ show d

instance Arbitrary (BasicBlock ASMInstr) where
  arbitrary = do
    (body, branch:_) <- break isBranch `fmap` sequence (repeat arbitrary)
    exit <- branchToExit branch (NamedLabel `fmap` listOf1 (choose ('A','Z')))
    BB `fmap` (NamedLabel `fmap` listOf1 (choose ('A','Z')))
       `ap` return (cleanupMults body)
       `ap` return exit
       `ap` ((S.fromList . filter (/=Reg 0)) `fmap` arbitrary)
       `ap` return ()
   where
     -- make sure that mult always appears before mflo and that
     -- there's at most one mflo and mfhi per mult
     cleanupMults = reverse . cleanupMults' Nothing Nothing (False,False)
     cleanupMults' l h _ (i@(MultMIPS _ _):rs) = 
       i : cleanupMults' l h (True,True) rs
     cleanupMults' (Just i) h (True,hh) rs = 
       i : cleanupMults' Nothing h (False,hh) rs
     cleanupMults' l (Just i) (hl,True) rs =
       i : cleanupMults' l Nothing (hl,False) rs
     cleanupMults' l h (hl,hh) (i@(MFLo _):rs)
         | hl = i : cleanupMults' l h (False,hh) rs
         | otherwise = cleanupMults' (Just i) h (hl,hh) rs
     cleanupMults' l h (hl,hh) (i@(MFHi _):rs)
         | hh = i : cleanupMults' l h (hl,False) rs
         | otherwise = cleanupMults' l (Just i) (hl,hh) rs
     cleanupMults' l h m (i:rs) = i : cleanupMults' l h m rs
     cleanupMults' _ _ _ [] = []
                             
                             