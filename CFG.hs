{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module CFG 
  ( CFG(..), BasicBlock'(..), BasicBlock, Exit(..)
  , exitInst, bbInstsFwd
  , successors'
  , flowGraph, dottyFlowGraph
  , dfs
  ) 
where

import Types ( Label(..), Reg, Instr(..) )
import GPP ( Const(..), GPPInstr, isBranch )

import GraphMap as GM
import PPrint
import Text.PrettyPrint.HughesPJ as PP
import WriteDotGraph as Dot

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Set ( Set )
import Data.Map ( Map )
import Data.Maybe ( maybeToList )


------------------------------------------------------------------------------

-- | A Control Flow Graph parametrized over the instruction type.
data CFG i n = CFG
    { cfgBBs     :: Map Label (BasicBlock' i n)  
    -- ^ All basic blocks from the original description (MIPS source file).
    , cfgEntries :: Set Label  
    -- ^ All entry points. (Typically one per function.)
    } deriving (Eq, Show)

type BasicBlock i = BasicBlock' i ()

data BasicBlock' instr info = BB 
    { bbLabel  :: Label
    , bbInstrs :: [instr]    -- ^ Instructions in reversed order.
    , bbExit   :: Exit instr
    , bbLiveOut :: Set Reg
    , bbInfo   :: info       -- ^ 
    } deriving (Show)

-- | Return all instructions in ascending order (according to orignal source).
--  Will include the final jump instruction, if any.
bbInstsFwd :: BasicBlock' i n -> [i]
bbInstsFwd BB{ bbInstrs=is, bbExit=ex } = 
    reverse $ maybeToList (exitInst ex) ++ is

instance (PPrint i, PPrint n) => PPrint (BasicBlock' i n) where
    pshow b = nest 8 $ (vcat $ 
                map pshow (reverse (bbInstrs b))) 
              $+$ pshow (bbExit b)
              $+$ text "liveOut:" <+> pshow (bbLiveOut b)
              $+$ pshow (bbInfo b)

               

-- | Describes how the basic block is ended.  Jump targets may be unknown if
--   the jump target is determined dynamically.
data Exit i
    = ExBranch (Maybe Label) i
      -- ^ Unconditional branch to the label.
    | ExCBranch (Maybe Label) Label i
      -- ^ Conditional branch.  If the condition is true the first label is
      -- chosen.
    | ImplicitJump Label
      -- ^ Control flow continues at block with the given label.  This is
      -- semantically equivalent to 'ExBranch' but captures the fact that in
      -- the original code the target block was following the given block
      -- directly.
    | ExCall (Maybe Label) Label i
      -- ^ Control flow continues at the called procedure and has call
      -- semantics (i.e., has to respect calling convention.)  If the called
      -- function returns properly, control flow continues at the second
      -- label.  [XXX: could also be an instruction]
    | Exit
      -- ^ Control flow ends by returning from the current function.
      deriving (Eq, Read, Show)

-- | Return the instruction associated with this exit node, if any.
exitInst :: Exit i -> Maybe i
exitInst x = case x of
  ExBranch _ i    -> Just i
  ExCBranch _ _ i -> Just i
  ExCall _ _ i    -> Just i 
  _               -> Nothing

instance PPrint i => PPrint (Exit i) where
    pshow (ExBranch _ s) = pshow s
    pshow (ExCBranch _ l s) = pshow s <+> text " else goto " <+> pshow l
    pshow (ImplicitJump l) = PP.parens $ text "goto" <+> pshow l
    pshow (ExCall _ _ i) = pshow i
    pshow Exit = text "---EXIT---"


instance Eq (BasicBlock' i n) where bb1 == bb2 = bbLabel bb1 == bbLabel bb2
instance Ord (BasicBlock' i n) where
    compare bb1 bb2 = bbLabel bb1 `compare` bbLabel bb2

-- | Return the labels of known successors of the given block.
successors' :: BasicBlock' i n -> [Label]
successors' BB{ bbExit=x } = case x of
    Exit             -> []
    ExBranch ml _    -> m2l ml
    ExCBranch ml l _ -> l : m2l ml
    ImplicitJump l   -> [l]
    ExCall ml l _    ->  [l] -- calls targets are not part of the CFG
  where m2l Nothing = []
        m2l (Just x) = [x] 
       
--
flowGraph :: Map Label (BasicBlock a) -> GraphMap Label ()
flowGraph bbs = GM.fromEdges $
    [ (n, (), n') | n <- M.keys bbs
                  , n' <- successors' (bbs M.! n)
    ]

-- | Generate a /Dotty/ file description from the given flow graph.
dottyFlowGraph :: Map Label (BasicBlock a) -> String
dottyFlowGraph bbs = Dot.writeDotGraph nodes edges
  where
    nodes = [ (n, [Dot.Label (render (pshow n))]) | n <- M.keys bbs ]
    edges = [ (n, [], n') | n <- M.keys bbs
                          , n' <- successors' (bbs M.! n)
            ]
-- | Traverse the graph in depth-first order starting at label.
dfs :: Map Label (BasicBlock a) -> Label
    -> (BasicBlock a -> b -> b) -> b -> b
dfs bbs start f r0 = snd $ dfs' start (S.empty, r0)
  where
    dfs' l (ls, r)
      | l `S.member` ls = (ls, r)
      | otherwise = foldr dfs' (S.insert l ls, f (bb l) r)
                          (successors' (bb l))
    bb l = M.findWithDefault (error $ "Unknown block: " ++ show l) l bbs
    
tst_dfs bs l0 = dfs bs l0 (\BB{ bbLabel=l } ls -> (l:ls)) []
