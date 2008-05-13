module Liveness where

import Types ( Label, Reg(..), Instr(JR) )
import GPP ( ASMInstr, uses, defs )
import MipsCFG
import CFG

import Utils

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Set ( Set )
import Data.Map ( Map )
import Data.List ( foldl' )
import Data.Maybe ( maybeToList )

import Debug.Trace

------------------------------------------------------------------------------

lives :: Map Label (BasicBlock ASMInstr) -- ^ Basic blocks
      -> Set Label                       -- ^ Graph entry points
      -> Map Label (Set Reg, Set Reg)   -- ^ live-ins and live-outs per block 
lives bbs entries =
    S.fold lives' M.empty entries 
  where
    lives' entry lvs = findFixPoint (calcLives (dfs bbs entry (:) [])) lvs

    -- The use/def map contains a set of all registers that are being defined
    -- ("killed") by a basic block and those that are used (i.e., those
    -- registers that are expected to be defined outside of this basic block).
    usedefs = M.map usedef bbs
    usedef :: BasicBlock ASMInstr -> (Set Reg, Set Reg)
    usedef BB{ bbInstrs=is, bbExit=ex } = rslt
      where 
        rslt = second (S.filter (/= Reg 0)) $
               foldl' usedef' (S.empty, S.empty) is'
        -- Note, that @is@ is reversed, ie., we start with the last
        -- instruction.
        is' = maybeToList (exitInst ex) ++ is
        -- If an instruction defines a value, that means it no longer needs to
        -- be defined outside this block.  
        usedef' (kill, use) inst =
            let kill' = kill `S.union` defs inst
                use'  = (use `S.difference` defs inst) `S.union` uses inst
            in (kill', use') 
            
    calcLives :: [BasicBlock ASMInstr]  -- ^ Order in which to traverse the BBs.
              -> Map Label (Set Reg, Set Reg) -- ^ current knowledge
              -> (Map Label (Set Reg, Set Reg), Bool) 
                 -- ^ new knowledge and whether it is different
    calcLives bbs lvs0 = foldl' calcLives' (lvs0, False) bbs
     where
      -- the lives-in of a basic block are the lives-out adjusted by the kills
      -- and uses
      calcLives' :: (Map Label (Set Reg, Set Reg), Bool) -> BasicBlock ASMInstr
                -> (Map Label (Set Reg, Set Reg), Bool) 
      calcLives' (lvs,changed) bb@BB{ bbLabel=l, bbExit=ex} = 
          let (kill, use) = M.findWithDefault 
                              (error $ "Liveness.lives: no kill/use for "++show l)
                              l usedefs --usedefs M.! l
              succs = successors' bb
              blkLiveIns l = fst $ M.findWithDefault (S.empty, S.empty) l lvs
              (oldLiveIns, oldLiveOuts) = M.findWithDefault (S.empty,S.empty) l lvs
              liveOuts =
                  case ex of
                    Exit -> exitLiveOuts
                    ExBranch _ (JR (Reg 31)) -> exitLiveOuts
                    ExCall _ _ _ -> S.fromList (map Reg [4,5,6,7,16,17,18,19,20,21,22,23,28,29,30,31])
                    
                    _ -> foldl' S.union S.empty $ map blkLiveIns succs
              liveIns' = (liveOuts `S.difference` kill) `S.union` use
          in ( M.insert l (liveIns', liveOuts) lvs
             , changed || liveIns' /= oldLiveIns )
               
    findFixPoint :: (a -> (a, Bool)) -> a -> a
    findFixPoint f a =
        case f a of
          (a', False) -> a'
          (a', True)  -> findFixPoint f a'