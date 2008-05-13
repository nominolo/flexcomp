{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards #-}
module Dependency (
  -- * Dependency testing
  dependsOn,
  depType,
  Dependency(..), DepType(..), DepCarrier(..), 
  DepInfo(..), depInfo, --, diDeps, diIDeps --, DepMap(..), InvDepMap(..)
  dependents, dependees, foldDependents, isImmDep, uses, defs,
  genDepGraph,
  --findDeps
) where

------------------------------------------------------------------------------

import qualified Types as F
import qualified GPP   as F
import Types ( Label(..) )
import MicroOp
import Mips2Mops ( BasicMopBlock )
import MipsCFG
import CFG

import Utils
import PPrint
import GraphMap as GM

import Data.Map ( Map )
import Data.Set ( Set )
import qualified Data.Map as Map
import qualified Data.Set as S
import Control.Monad ( liftM )
import Data.List ( foldl', find )
import Data.Maybe ( maybeToList, isJust, fromMaybe )
import Text.PrettyPrint.HughesPJ as PP

import qualified WriteDotGraph as Dot

import Debug.Trace 
import Test.Example1

------------------------------------------------------------------------------

data DepInfo = DI { 
      diDeps  :: DepMap,
      -- ^ Dependencies in usual order.  Maps from use site to definition site.
      diIDeps :: DepMap
      -- ^ Inverted dependencies.  Maps from definition site to use site(s).
    } deriving Show

instance PPrint DepInfo where pshow = pshow . diDeps

dependsOn :: DepInfo -> MicroOp -> MicroOp -> Bool
dependsOn inf a b = isJust $ depType inf a b

-- | Return all micro-ops that depend on the given instruction.
dependents :: DepInfo -> MicroOp -> Set MicroOp
dependents DI{ diIDeps=invd } mop = successors mop invd

-- | Return all micro-ops that the given instruction depends on.
dependees :: DepInfo -> MicroOp -> Set MicroOp
dependees DI{ diDeps=d } mop = successors mop d

depType :: DepInfo -> MicroOp -> MicroOp -> Maybe Dependency
depType (DI deps ideps) a b = Nothing

foldDependents :: DepInfo -> MicroOp 
               -> (MicroOp -> Set Dependency -> a -> a) 
               -> a
               -> a
foldDependents DI{ diIDeps=invd } mop f a0 =
    let succ'deps :: Map MicroOp (Set Dependency)
        succ'deps = outEdges mop invd
    in Map.foldWithKey f a0 succ'deps

------------------------------------------------------------------------------



-- | Maps each path variable to its defining instruction.
type DefMap = Map PathVar (MicroOp)

mkDefMap :: BasicMopBlock -> DefMap
mkDefMap BB{ bbInstrs=ops } = foldl' f Map.empty ops
  where f m i@(Mop _ (Just pv) _) = Map.insert pv i m
        f m _ = m

-- | Return operations that this operation directly depends on. 
--dirDepInsts :: DefMap -> MicroOp -> Set (MicroOp)
--dirDepInsts dm mop = S.map (dm Map.!) (uses mop)

-- | Type of possible dependencies between two micro-ops.
--
-- Dependencies due to register usage:
--
-- * True dependency, \"@i1@ truly depends on @i2@\" 
-- 
-- > i1:  $r <- ...
-- > i2:  ... <- $r
--
--   We certainly need to keep track of this.
--
-- * Write dependency, \"@i2@ is write-dependent on @i3@\"
--
-- > i1:  $r <- ...
-- > i2:  $r <- ...
--
--   This is actually quite nice, since we can completely drop the first
--   instruction, /if it doesn't give rise to any true dependencies/.
--   However, we could keep the information that @$r@ is free until the second
--   instruction is scheduled, so we can use it as a keep-alive location, if
--   needed.
--
-- * Anti dependency, \"@i2@ is anti-dependent on @i1@\"
--
-- > i1:  p <- $r
-- > i2:  $r <- ...
--
--   We have to consider this.  Of course, if any of the instructions gets
--   deleted, the dependency disappears.
-- 
-- * Read dependencies can be ignored.
-- 
-- We keep the original order of memory accesses, since each memory reference
-- may potentially alias a previous one.  [XXX: We could recognize spill
-- locations and optionally remove them if we find that we freed a register.]
-- Doing full dependency analysis is the task of an earlier pass.
--
-- Dependencies due to path variable usage can only be true dependencies,
-- since a path variable can only be used after it is defined and can only be
-- defined once.
-- 
data Dependency 
    = DataDep DepType DepCarrier  -- ^ data dependency via given carrier
    | ImmediateDependency PathVar -- ^ direct dependency due to use of given
                                  -- path variable
    | TimingDependency Int -- ^ instruction must be executed @n@ cycles after  
      deriving (Ord, Show, Eq)

data DepCarrier 
    = Reg F.Reg   -- ^ dependency is due to use of register @r@
    | Buf Int   -- ^ dependency is due to use of buffer @b@
    | Mem       -- ^ dependency is due to memory accesses
      deriving (Ord, Show, Eq)

data DepType = DTAnti 
             | DTTrue 
             | DTOutput deriving (Ord, Show, Eq)

instance PPrint Dependency where
    pshow (DataDep DTTrue r) = text "Dt" <> parens (pshow r)
    pshow (DataDep DTAnti r) = text "Da" <> parens (pshow r)
    pshow (DataDep DTOutput r) = text "Do" <> parens (pshow r)
    pshow (ImmediateDependency p) = text "Di" <> parens (pshow p)
    pshow (TimingDependency n) = text "D(t>=" <> int n <> char ')'

instance PPrint DepCarrier where
    pshow (Reg r) = text $ '$' : show r
    pshow (Buf b) = text $ 'B' : show b
    pshow Mem     = text "Mem"

-- | Maps a micro op to all the micro ops it depends on (i.e., those that have
-- to be scheduled first) and annotates it with the type(s) of dependency.
type DepMap = GraphMap (MicroOp) (Set Dependency)
{-
MkDepMap 
    { depGraph :: GraphMap (MicroOp) (Set Dependency) } 
    deriving PPrint
newtype InvDepMap = MkInvDepMap
    { idepGraph :: GraphMap (MicroOp) (Set Dependency) } 
    deriving PPrint
-}

depInfo :: BasicMopBlock -> DepInfo
depInfo blk@BB{ bbInstrs=blkmops0, bbExit=ex, bbInfo=wbs } =
    let blkmops = maybeToList (exitInst ex) ++ blkmops0
        depMap = findDeps' (reverse blkmops) 
                           wbs 
                           Map.empty
                           (Map.empty, Map.empty)
                           (Nothing, [])
                           GM.empty
        invDepMap = transpose depMap
    in DI depMap invDepMap

findDeps' :: [MicroOp]
         -> Set PathVar
         -> Map PathVar MicroOp -- instruction where PV is defined
         -> (Map F.Reg MicroOp, Map F.Reg [MicroOp])
         -- ^ last instructions where Reg is read or instruction where
         -- Reg is written
         -> (Maybe MicroOp, [MicroOp])
         -- ^ last mem write or all last mem reads
         -> DepMap
         -> DepMap
findDeps' _ a b c d e | a `seq` b `seq` c `seq` d `seq` e `seq` False = undefined
findDeps' [] _ _ _ _ d = d
findDeps' (mop:r) writeBacks pvDefs regWriteReads memWriteReads deps =
    findDeps' r writeBacks pvDefs' regWriteReads' memWriteReads' deps'
 where
   -- if this instruction defines a new PV, add it to the map
   pvDefs' | Mop _ (Just pv) _ <- mop = Map.insert pv mop pvDefs
           | otherwise = pvDefs

   deps' = addDirectDeps . addRegDeps . addMemDeps $ deps
   
   addDirectDeps =
       let edges = S.toList $ directDep mop `mapMaybe` uses mop
       in addDepEdges edges

   directDep m pv = do m' <- Map.lookup pv pvDefs
                       return (m, ImmediateDependency pv, m')
       
   -- A memory read depends on (read: "must be executed after") the
   -- most recent write.  A memory write depends on all most recent
   -- writes or the most recent write.
   (addMemDeps, memWriteReads') 
     | Mop _ _ (Load _ _ _) <- mop =
        case memWriteReads of
          (Just w, rs)  -> ( addDepEdge (mop, DataDep DTTrue Mem, w),
                             (Just w, mop:rs) )
          (Nothing, rs) -> (id, (Nothing, mop:rs) )
     | Mop _ _ (Store _ _ _) <- mop =
        case memWriteReads of
          (mw, rs) ->
              ( maybe id (\w -> addDepEdge (mop, DataDep DTOutput Mem, w)) mw
                . addDepEdges (map (\r -> (mop, DataDep DTAnti Mem, r)) rs)
              , (Just mop, []) )
     | otherwise = (id, memWriteReads)
                   
   (regWrite, regReads) = regWriteReads
   -- Register dependencies only need to be considered for registers
   -- that are in the write back list.  That is, if a micro-op needs
   -- to write back its result to a certain register, it must be
   -- scheduled after all reads from that register.  That is, it
   -- depends on all instructions that use a path var that has origin
   -- @(OrigRead r)@ where @r@ matches the register needing to be
   -- written back
   --
   -- (We are actually a little too strict here.  If the same register
   -- is read from twice, the code generator will re-use the same path
   -- var.  In principle the micro-op needing the write-back could
   -- therefore be scheduled after the first read has been scheduled
   -- and the value of the var could be somehow routed to its second
   -- usage.  It would require some validation see if that would
   -- actually lead to better results.)
   -- 
   -- All other dependencies are covered by direct
   -- dependencies. 
   (addRegDeps, regWriteReads')
     -- this instruction defines a path variable that needs to be
     -- written back (
     | Mop _ (Just pv@(PV _ (Just (OrigWrite r)))) _ <- mop,
       pv `S.member` writeBacks =
         case (Map.lookup r regWrite, Map.lookup r regReads) of
           (Nothing, mrs) ->
               let rs = fromMaybe [] mrs in
               ( addDepEdges (map (\m -> (mop, DataDep DTAnti (Reg r), m)) rs)
                 -- XXX: actually, two writes of the same reg in the
                 -- same basic block cannot happen
               -- . maybe id (\m -> addDepEdge (mop, DataDep DTOutput (Reg r), m)) mw
               , ( Map.insert r mop regWrite
                 , Map.delete r regReads )) 
     -- We cannot have a read after write dependency
     | otherwise = ( id, (regWrite, addReads mop regReads) )

   addReads m rds =
       let us = S.toList $ uses m
           addIt (PV _ (Just (OrigRead r))) rs = 
               Map.alter (maybe (Just [m]) (Just . (m:))) r rs
           addIt _ rs = rs
       in foldr addIt rds us

                                   
addDepEdges es ds = foldr addDepEdge ds es 

addDepEdge (from, dep, to) dm = --trace (show (from,to)) $
    modifyLabel from to addDep dm
  where addDep Nothing = Just (S.singleton dep)
        addDep (Just ds) = Just (S.insert dep ds)


{-
isMemAccess :: MicroOp -> Bool
isMemAccess x = isMemRead x || isMemWrite x
isMemRead (Mop _ _ (Load _ _ _)) = True;  isMemRead _ = False
isMemWrite (Mop _ _ (Store _ _ _)) = True;  isMemWrite _ = False

depInfo :: BasicMopBlock -> DepInfo
depInfo blk@BB{ bbInstrs=blkmops0, bbExit=ex, bbLiveOut=lvOut } = 
    DI (depMap) (invDepMap)
  where
    depMap :: GraphMap (MicroOp) (Set Dependency)
    depMap = fromEdgesWith add2set $ concatMap mopDeps blkmops
    blkmops = maybeToList (exitInst ex) ++ blkmops0

    add2set Nothing x = S.singleton x
    add2set (Just s) x = S.insert x s

    invDepMap = transpose depMap
                
    -- all the micro-ops as a set
    mopsSet = S.fromList blkmops
              
    mults = S.filter isMult mopsSet
      where isMult (Mop _ _ (MultMIPS _ _)) = True
            isMult _ = False
    memAccesses = S.filter isMemAccess mopsSet


    mopDeps :: MicroOp -> [(MicroOp, Dependency, MicroOp)]
    mopDeps mop =
        [ (mop, dt, dst) | (dt, dst) <- S.toList (directDeps `S.union` otherDeps) ] 
      where
        -- a micro-op depends on all the instructions that define the
        -- path variable
        directDeps = S.map (\p -> (ImmediateDependency p, 
                                  pvDefMap Map.! p)) $ uses mop
        otherDeps = 
          case mop of
            Mop _ _ (ReadReg r) -> regDeps mop Read r
            Mop _ _ (WriteReg r _) -> regDeps mop Write r

            Mop _ _ (ReadBuf 3) ->
              -- if we read from Buf3, it means we want to retrieve the result
              -- of a multiplication.  Let @D@ be the delay of the multiplier.
              -- Then the buffer read has to be scheduled
              -- 
              --  * @D@ or more cycles after the corresponding mult
              --  
              --  * at most @D - 1@ cycles after the next scheduled @mult@
              --    instruction
              --  
              -- XXX: we simplify and assume there is only one mult per basic
              -- block, ATM
              let bef = fst $ S.split mop mults in
              if S.null bef then S.empty
              else S.singleton (TimingDependency 2, S.findMax bef)

            -- Memory operations:
            --
            -- We have the same rules for memory as we have for registers,
            -- except that we assume that the address always is the same, to
            -- avoid doing alias analysis.
            -- 
            Mop _ _ (Load _ _ _) -> memDeps mop Read
            Mop _ _ (Store _ _ _)  -> memDeps mop Write

            _ -> S.empty

    --mopsBefore :: MicroOp -> Set (MicroOp) -> Set (MicroOp)
    --mopsBefore mop = fst . S.split mop

    -- a write depends on the next read or write
    -- a read depends on the next write
    memDeps mop acc =
        withMaybe (dependents mop memAccesses isMemRead isMemWrite acc)
           S.empty $ \targets ->
         mapMaybe (\m -> do dt <- depType acc (accType m) Mem
                            return (dt, m))
                  targets
      where
        accType x | isMemRead x  = Read
        accType x | isMemWrite x = Write
        accType _ = error "wrong micro op type"

    regDeps mop acc reg = 
        withMaybe (dependents (mop,acc) prevMops ((== Read) . snd)
                              ((== Write) . snd) acc)
            S.empty $ \targets ->
          mapMaybe (\(m, a) -> do dt <- depType acc a (Reg (F.Reg reg))
                                  return (dt, m)) 
                   targets
      where
        prevMops = fst $ S.split (mop, acc) $
                     Map.findWithDefault S.empty reg regMap
        
    depType :: AccessType -> AccessType -> DepCarrier -> Maybe Dependency
    depType Read Write c  = Just $ DataDep DTTrue c
    depType Write Write c = Just $ DataDep DTOutput c
    depType Write Read c  = Just $ DataDep DTAnti c
    depType Read Read _   = Nothing

    
    --isRegAccess r x = isRegRead r x || isRegRead r x
    --isRegRead r (Def _ _ (ReadReg r')) | r == r' = True;   isRegRead _ _ = False
    --isRegWrite r (Eff _ (WriteReg r' _)) | r == r' = True; isRegWrite _ _ = False

    dependents mop mops isRead isWrite acc =
        case acc of 
          Read  -> recentWrite isWrite prevMops
          Write -> recentReadsOrWrite isRead isWrite prevMops
      where prevMops = fst $ S.split mop mops --mopsBefore mop mops

    recentWrite :: Ord a => (a -> Bool) -> Set a -> Maybe (Set a)
    recentWrite isWrite mops = 
        liftM S.singleton $ findMaxBy isWrite mops

    -- return either all recent reads up to the most recent write or, the most
    -- recent write
    recentReadsOrWrite :: Ord a => (a -> Bool) -> (a -> Bool) -> Set a 
                       -> Maybe (Set a)
    recentReadsOrWrite isRead isWrite mops = rsorw
      where
        reads = S.filter isRead mops
        writes = S.filter isWrite mops
        rsorw
          | S.null reads  = liftM S.singleton (safeFindMax writes)
          | S.null writes = Just $ reads
          | S.findMax writes > S.findMax reads =
                              Just $ S.singleton $ S.findMax writes
          | otherwise =
              Just $ snd $ S.split (S.findMax writes) reads

    safeFindMax set | S.null set = Nothing
                    | otherwise    = Just $ S.findMax set

    pvDefMap = mkDefMap blk
    regMap = mkRegMap blk

{-
    stripTransitiveDeps :: Map (MicroOp) (Set (Dependency, MicroOp))
                        -> Map (MicroOp) (Set (Dependency, MicroOp))
    stripTransitiveDeps m =
        let (changed, m') = strip1 m in
        if changed then stripTransitiveDeps m' else m'
      where
        strip1 dm =
            let ((m,deps), dm') = Map.deleteFindMin dm in
-}          

{-
mopDeps :: DepMap -> MicroOp -> Set (Dependency, MicroOp)
mopDeps (DepMap dm) mop = Map.findWithDefault S.empty mop dm

mopInvDeps :: InvDepMap -> MicroOp -> Set (Dependency, MicroOp)
mopInvDeps (InvDepMap idm) mop = Map.findWithDefault S.empty mop idm

filterInvDeps :: InvDepMap -> (Set (Dependency, MicroOp) -> Bool)
              -> Set (MicroOp)
filterInvDeps (InvDepMap idm) p =
    Map.k
-}
data AccessType = Read | Write deriving (Eq, Ord, Show)
instance PPrint AccessType where pshow = text . show

-- | Maps each register to all the micro ops that use it and how they are used
-- at that micro op.
type RegMap = Map Int (Set (MicroOp, AccessType))

mkRegMap :: BasicMopBlock -> RegMap
mkRegMap BB{ bbInstrs=mops, bbLiveOut=lvOut } = foldl' gather Map.empty mops
  where
    gather m i@(Mop _ _ (ReadReg r)) = addToSetMap r (i, Read) m
    gather m i@(Mop _ _ (WriteReg r _)) = addToSetMap r (i, Write) m
    gather m _ = m


addToSetMap :: (Ord k, Ord a) => k -> a -> Map k (Set a) -> Map k (Set a) 
addToSetMap k e m = Map.alter add' k m
  where add' Nothing = Just $ S.singleton e
        add' (Just m') = Just $ S.insert e m'

--immDeps :: DepMap -> MicroOp -> Set (MicroOp)
--immDeps (MkDepMap gm) mop = depsOfType gm mop isImmDep

--immIDeps :: InvDepMap -> MicroOp -> Set (MicroOp)
--immIDeps (MkInvDepMap gm) mop = depsOfType gm mop isImmDep

depsOfType :: GraphMap (MicroOp) (Set Dependency) 
           -> MicroOp
           -> (Dependency -> Bool)
           -> Set (MicroOp)
depsOfType g mop f = Map.keysSet $ Map.filter hasDepType $ outEdges mop g
  where hasDepType depTypes = not $ S.null $ S.filter f depTypes
-}
isImmDep :: Dependency -> Bool
isImmDep (ImmediateDependency _) = True
isImmDep _ = False

------------------------------------------------------------------------------

data Lit = Lit String deriving Eq
instance Show Lit where show (Lit s) = s

genDepGraph :: DepInfo -> String
genDepGraph DI{ diDeps=dm } = Dot.writeDotGraph (nodes'++regNodes) 
                                                (edges'++regEdges)
  where
    nodes' = [ (Left n, [Dot.Label (ppMop n)]) 
                   | n <- nodes dm ]
    edges' = [ (Left sink, ppLab l, Left src) 
                   | (src, ls, sink) <- edges dm
                   , l <- S.toList ls ]
    ps :: PPrint a => a -> String
    ps = render . pshow
    regReads = [ (m, r) | m <- nodes dm
                        , (PV _ (Just (OrigRead r))) <- S.toList (uses m) ]
    regNodes = [ (Right r, [Dot.Label (render $ pshow r)] ) | (_,r) <- regReads ]
    regEdges = [ (Right r, [], Left m) | (m,r) <- regReads ]
     
    ppLab dep = case dep of 
      ImmediateDependency x -> [ Dot.Label (ps x) ]
      DataDep DTAnti r@(Reg _) -> [Dot.Label (ps r), Dot.Color "green" ]
      DataDep _ r -> [Dot.Label (ps r), Dot.Color "red" ]
      --DataDep _ r -> [Dot.Label (ps r), Dot.Color "blue" ]
      TimingDependency n -> [Dot.Label (show n)]
      --x -> [Dot.Label (ps x)]
    ppMop (Mop i _ x) = show i ++ ": " ++ case x of
      Imm c -> show c
      ALU o _ _ -> render $ pshow o
      Load w _ _ -> "Load M" ++ render (pshow w) ++ "[]"
      ReadReg r -> '$' : show r
      ReadBuf b -> 'B' : show b
      ReadPC    -> "PC"
      WriteReg r _ ->  '$' : show r
      WriteBuf b _ -> 'B' : show b
      Store w _ _  -> "Store M"++ render (pshow w) ++ "[]"
      MultMIPS _ _ -> "*"
      Branch _ _ _ -> "br"
      Jump _ _     -> "j"


uses :: MicroOp' a -> Set PathVar
uses (Mop _ _ op) = case op of
    ALU _ x y    -> twoElem x y
    Load _ _ x   -> S.singleton x
    WriteReg _ x -> S.singleton x
    Store _ x y  -> twoElem x y
    MultMIPS x y -> twoElem x y
    Branch _ x y -> S.singleton x --twoElem x y
--    Jump True x  -> S.fromList [  ]
    Jump _ x -> S.singleton x
    WriteBuf _ x -> S.singleton x
    _            -> S.empty
--uses (ParPair x y) = S.union (uses x) (uses y)

defs :: MicroOp -> Set PathVar
defs (Mop _ (Just x) _) = S.singleton x
defs _                  = S.empty

twoElem :: Ord a => a -> a -> Set a
twoElem x y = S.insert x (S.singleton y)
