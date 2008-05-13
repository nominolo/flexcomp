{-# OPTIONS_GHC -fglasgow-exts -Wall #-}
module Schedule4 ( schedule ) where

import Options
import qualified MultiSet as MS
import qualified SAT
import qualified Types as F
import qualified GPP as F
import qualified FlexsocTypes as FS
import MicroOp as O hiding ( def )
import CFG
import Utils
import PPrint
import Dependency
import Mips2Mops ( BasicMopBlock )

import Prelude hiding ( all, any, foldr, cycle )
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Foldable    ( Foldable, toList, all, fold, foldMap, foldr, find, 
                          foldl' )
import Data.Traversable ( for, Traversable )
import Data.Set ( Set, isSubsetOf )
import Data.Map ( Map )
import Data.Maybe ( catMaybes, isJust, isNothing )
import Data.Ord   ( comparing )
import Data.List  ( sortBy, intercalate, sort )
import Control.Applicative  ( WrappedMonad(..) )
import Control.Arrow        ( (&&&) )
import Control.Monad.Reader
import Control.Monad.State  -- Don't use State.Strict
import Control.Monad.Writer ( execWriter, tell )
import Data.Monoid
import Data.Array.ST      hiding ( range )
import Data.Array.Unboxed hiding ( range )
import Text.PrettyPrint.HughesPJ as PP

import System.IO ( stderr, hPutStrLn )
import Debug.Trace

--import Test.Example1
--import Control.Exception (assert)

------------------------------------------------------------------------

type Cycle = Int
type Schedule = Map Cycle [(MicroOp,OpResource)]
type Routes   = Map Cycle (Map DataResource (Set PathVar))
type RegAlloc = Map PathVar (Set (Cycle,F.Reg))

schedule :: Options -> BasicMopBlock -> IO [[FS.MicroOp]]
schedule gopts blk = runSchedM gopts opts blk $ do 
     dbgNotify $ "Scheduling: " ++ pretty (bbLabel blk)
     st0   <- startState blk
     sched <- schedule' opts 1 mempty st0 mempty mempty
     withMaybe sched schedError $ \s -> do
       rvs <- getRouteVars
       withMaybe (allocateRegisters (bbLiveOut blk) rvs)
                 (error "Failed to allocate registers.") $
         \regAlloc -> do
             when (optHTMLOutput gopts) $ do
               liftIO (writeFile ("schedule" ++ pretty (bbLabel blk) ++ ".html")
                      (htmlSchedule gopts blk (s, rvs, regAlloc)))
             when (not $ sanityCheckSchedule (s, rvs)) $
               liftIO . putStrLn $ 
                   "WARNING: Sanity check failed for " ++ pretty (bbLabel blk)
             return $ flexSchedule gopts blk (s, rvs, regAlloc)
  where
    schedError = error $ "Scheduling failed.  This should not happen.\n"
                 ++ "This is a bug in the scheduler."
    opts = mempty :: ScheduleOptions

data ScheduleOptions = ScheduleOptions
    { allowTrailingWritebacks :: Bool }

instance Monoid ScheduleOptions where
    mempty = ScheduleOptions { allowTrailingWritebacks = False }
    a `mappend` b = 
        ScheduleOptions 
        { allowTrailingWritebacks = 
              allowTrailingWritebacks a || allowTrailingWritebacks b }

------------------------------------------------------------------------

-- | Return all the path vars that got assigned "true" 
getRouteVars :: SchedM Routes
getRouteVars = do
  rvls <- gets siLit2RV
  asgn <- filterM (liftSAT . SAT.getModelValue . fst) (M.assocs rvls)
  return $ 
    foldl' (\m (_,RV pv c loc) -> 
                M.insertWith (M.unionWith mappend) c (M.singleton loc (S.singleton pv)) m)
          mempty
          asgn 

------------------------------------------------------------------------
type Priority = Int

type SchedM a = ReaderT SchedEnv (StateT SATInfo SAT.MiniSatM) a

runSchedM :: Options -> ScheduleOptions -> BasicMopBlock -> SchedM a -> IO a
runSchedM gopts opts bb m = SAT.run $ evalStateT (runReaderT m (mkSchedEnv gopts opts bb)) s0
  where s0 = SI mempty mempty [] mempty

liftSAT :: SAT.MiniSatM a -> SchedM a
liftSAT = lift . lift

data SATInfo = SI
    { siRV2Lit :: Map RouteVar SAT.Lit
    , siLit2RV :: Map SAT.Lit RouteVar
    , siUndos  :: [SAT.Lit]
    , siPressure :: Map (Cycle, DataResource) [SAT.Lit]
    }

-- | We know the resource and the cycle a route var refers to.  This is used
-- to automatically add mutual exclusivity constraints for all variables that
-- refer to the same resource.  
--
-- Due to the need for incrementally adding new variables, we use a simple
-- mechanism to ensure mutual exclusivity.  For each new variable we add a
-- pair of "not both" constraints, that is a clause of the form @[-a, -b]@.
-- This means that if we have N variables we need (N*(N-1))/2 such clauses, or
-- O(N^2) complexity.  Some of those variables might actually not be needed,
-- since they represented a failed attempt to schedule a certain instruction
-- in that cycle.  We rely on the SAT-solver's garbage collection ability to
-- remove those unnecessary (trivially satisfiable) clauses.  (It does,
-- however, mean adding some superfluous clauses since we do not detect if a
-- variable is still needed or not.)
newLit :: RouteVar -> SchedM SAT.Lit
newLit rv@(RV _ cyc loc) = do
  l <- liftSAT SAT.newLit
  let limitedResource = loc /= DrRegFile
  when limitedResource $ do
    others <- liftM (M.findWithDefault [] (cyc, loc)) $ gets siPressure
    when (not (null others)) $ do
      liftSAT . sequence_ $ [ SAT.addClause [SAT.neg l, SAT.neg o] | o <- others ]
  modify $ \si -> si { siRV2Lit = M.insert rv l (siRV2Lit si)
                     , siLit2RV = M.insert l rv (siLit2RV si)
                     , siPressure = (if limitedResource then
                                        M.insertWith (++) (cyc, loc) [l]
                                      else id) (siPressure si)
                     }
  return l
newLit _ = error "newLit: cannot generate negated literal."

getLit :: RouteVar -> SchedM SAT.Lit
getLit rv = do
  aliases <- gets siRV2Lit 
  case M.lookup (the rv) aliases of
    Just l  -> return $ sign rv l
    Nothing -> newLit (the rv) >>= return . sign rv

newtype UndoToken = UT { undoToken :: SAT.Lit }

-- | Generates an undo-token.  We need to be able to remove certain
-- constraints when we back-track, but miniSAT doesn't support this directly.
-- Adding a contradicting clause will put the SAT-solver into a state where we
-- can no longer trust it.  We therefore use a trick shown by Koen Claessen.
--
-- For each clause we might want to undo later we add an additional negated
-- literal @neg p@.  We then try to satisfy the constraints (prove the
-- formula) under the assumption, that this additional literal is /true/.
-- This means that all constraints that contain @neg p@ must be satisfied by
-- some other propositions in that clause, since @neg p@ is false.  The
-- SAT-solver will not try to alter the assignment of @p@.
--
-- If we later want to undo the addition of those clauses, we simply add the
-- clause @[neg p]@.  This is a unit assignment and will make all clauses that
-- contained @neg p@ trivially satisfiable.  This is also efficient since the
-- SAT solver automatically garbage-collects all clauses that it determined to
-- be true.
--
newUndoToken :: SchedM UndoToken
newUndoToken = do 
  l <- liftSAT SAT.newLit
  modify $ \si -> si { siUndos = l : siUndos si }
  return $ UT l

addUClause :: UndoToken -> [RouteVar] -> SchedM ()
addUClause u c = do
    logP' 5 ("  +clause[u"++show (undoToken u)++"]") (Filled c)
    lits <- mapM getLit c
    ok <- liftSAT $ SAT.addClause (SAT.neg (undoToken u) : lits)
    when (not ok) $ error "Inconsistent SAT-solver state!  Argh!"
    return ()

undo :: UndoToken -> SchedM Bool
undo (UT l) = 
    do modify $ \si -> si { siUndos = filter (/= l) $ siUndos si }
       logS 4 (" - Undoing [" ++ "u"++show l++"]")
       liftSAT $  SAT.addClause [SAT.neg l]
       -- XXX: Also remove stuff from pressure

isRoutable :: SchedM Bool
isRoutable = do
  assumpts <- gets siUndos
  liftSAT $ SAT.solve assumpts


data RouteVar = RV !PathVar !Cycle !DataResource
              | Neg !RouteVar
                deriving (Ord, Eq, Show)

instance PPrint RouteVar where
  pshow (Neg rv) = char '-' <> pshow rv
  pshow (RV pv c pl) = pshow pv <> char '@' <> pshow pl <> char ':' <> pshow (-c)

the :: RouteVar -> RouteVar
the r@(RV _ _ _) = r
the (Neg r) = the r

neg :: RouteVar -> RouteVar
neg (Neg rv) = rv
neg rv = Neg rv

sign :: RouteVar -> SAT.Lit -> SAT.Lit
sign (Neg x) = SAT.neg . sign x
sign _ = id

data SchedEnv = SE 
    { seAsap       :: UArray Int Int
    , seAlap       :: UArray Int Int
    , seDepInfo    :: DepInfo
    , seLogLevel   :: LogLevel
    , seWriteBacks :: Set PathVar
    , seOrigBB     :: BasicMopBlock
    -- XXX: add interconnect and unit info
    , seCutoff     :: Int
    , seOptions    :: Options
    } deriving Show


instance PPrint SchedEnv where
  pshow se =
      text "asaps:" <+> fsep (punctuate comma $ map pshow $ filter ((>=0).snd) 
                                              $ assocs $ seAsap se) $+$
      text "alaps:" <+> fsep (punctuate comma $ map pshow $ filter ((<=0).snd)
                                              $ assocs $ seAlap se)

mkSchedEnv :: Options -> ScheduleOptions -> BasicMopBlock -> SchedEnv
mkSchedEnv gopts opts bb = SE { seAsap = asaps
                   , seAlap = alaps
                   , seDepInfo = deps
                   , seLogLevel = 0
                   , seWriteBacks = bbInfo bb
                   , seOrigBB = bb
                   , seCutoff = 2 * length (bbInstsFwd bb)
                   , seOptions = gopts
                   }
  where deps = depInfo bb
        (asaps, alaps) = mobilities opts needsWriteback deps (bbInstsFwd bb)
        needsWriteback (Mop _ (Just p) _) = p `S.member` (bbInfo bb)
        needsWriteback _ = False
            

mobilities :: ScheduleOptions 
           -> (MicroOp -> Bool) -- ^ needs write-back
           -> DepInfo -> [MicroOp]
	   -> (UArray Int Int, UArray Int Int)
mobilities opts writeBack deps mops = (asaps, alaps)
  where
    asaps = asap deps mops (minMop, maxMop)
    alaps = alap opts writeBack deps mops (minMop, maxMop)
    (minMop, maxMop) = foldl' (\(a,b) (a',b') -> (min a a', max b b')) (maxBound,0) 
                      $ map (\x -> (x,x)) $ map mopInfo mops

-- | Calculate the as-soon-as-possible (ASAP) thresholds for the given
--   instructions.
asap :: DepInfo -> [MicroOp] -> (Int,Int) -> UArray Int Int
asap deps mops range = runSTUArray $
    do asaps <- newArray range (-1)
       forM mops $ (\mop -> do
         let preds = dependees deps mop
         depAsaps <- mapM (readArray asaps . mopInfo) $ S.toList preds
         let mopAsap | null depAsaps = 0
                     | otherwise     = 1 + maximum depAsaps
         writeArray asaps (mopInfo mop) mopAsap)
       return asaps

-- | Calculate the as-late-as-possible (ALAP) threshold.  The result is an
--   array of negative numbers, where @0@ means "can be the last instruction",
--   @-1@ "can be the second to last instruction", etc.  By using negative
--   numbers we avoid recalculation of these numbers, when we find a better
--   approximation for the upper bound of the schedule length.
alap :: ScheduleOptions -> (MicroOp -> Bool) 
     -> DepInfo -> [MicroOp] -> (Int, Int) -> UArray Int Int
alap opts writeBack deps mops range = runSTUArray $
    do alaps <- newArray range 1
       forM (reverse mops) $ \mop -> do
         let succs = dependents deps mop
         depAlaps <- mapM (readArray alaps . mopInfo) (S.toList succs)
         let mopAlap | isBranch mop  = -branchDelaySlots
                     | null depAlaps = if writeBack mop && not (allowTrailingWritebacks opts)
                                       then (-1) else 0
                     | otherwise     = minimum depAlaps - 1
         writeArray alaps (mopInfo mop) (mopAlap :: Int)
       return alaps

branchDelaySlots :: Int
branchDelaySlots = 2

------------------------------------------------------------------------

-- | The scheduler State.
data SchedState = SS
    { ssSchedule    :: Schedule
    , ssScheduled   :: Map MicroOp (Cycle, OpResource)
    --, ssReady     :: Set MicroOp -- this would be an optimization
    --, ssLiveVars    :: Map PathVar (Set DataResource)
    , ssUnscheduled :: Set MicroOp
    } deriving Show

instance PPrint SchedState where
  pshow sch = text "schedule:" $+$ nest 4 (pshow (ssSchedule sch))
              

startState :: BasicMopBlock -> SchedM SchedState
startState bb = 
    -- XXX: monadic -> for optimizations we might later want to read the
    -- environment
    return $
      SS { ssSchedule  = mempty
         , ssScheduled = mempty
         , ssUnscheduled = S.fromList (bbInstsFwd bb)
         }

scheduleMop :: Cycle -> (MicroOp, OpResource)
            -> SchedState -> SchedState
scheduleMop c mp@(m,r) st =
    st { ssSchedule = M.insertWith (++) c [mp] (ssSchedule st)
       , ssScheduled = M.insert m (c,r) (ssScheduled st)
       , ssUnscheduled = S.delete m (ssUnscheduled st)
       }

-- addLive :: PathVar -> Set DataResource -> SchedState -> SchedState
-- addLive pv locs st = st { ssLiveVars = M.insert pv locs (ssLiveVars st) }

schedule' ::
     ScheduleOptions
  -> Cycle 
  -> [(MicroOp,OpResource)] 
  -> SchedState 
  -> [[(PathVar, Set DataResource)]] 
    -- ^ Variables which might become live in this cycle.
  -> [(PathVar, Set DataResource)]
    -- ^ Lives.  PVs known to be live in this cycle.
  -> SchedM (Maybe Schedule)
--schedule' cycle _ _ _ _ | cycle < (-40) = 
schedule' opts cycle insts state newLives lives 
  | S.null (ssUnscheduled state) && null newLives = do
      logS 2 "Scheduling successful!"
      return $ Just (ssSchedule state)
  | otherwise = do 
    env <- ask

    when (cycle > seCutoff env) $ error "scheduling ran too long"

    logP' 2 "Scheduling cycle" cycle
    logP' 3 "Instructions remaining" (S.size (ssUnscheduled state))
    logP 3 "Schedule so far:" (ssSchedule state)
    logP 5 "> Lives" lives
    logP 5 "> New Lives" newLives

    if isJust (exitInst (bbExit (seOrigBB env))) 
       && cycle == -branchDelaySlots
       && isNothing (find (isBranch . fst) insts)
     then return Nothing
     else do
       let state' = foldr (scheduleMop cycle) state insts
       readyByPrio' <- readyInstsByPrio (seOptions env) (cycle-1) (mobilityPrio env (cycle-1)) state'
       let insts' = pickWithResources readyByPrio' coreOpResources

       scheduleCycle state' insts'
    
 where
--   whenRoutable u state' insts' k = do
--     ok <- isRoutable
--     if ok then k
--       else do 
--         undo u 
--         if null insts' then
--           return Nothing
--          else
--           scheduleCycle state' (init insts')  

  scheduleCycle state' insts' = do
    let rsrcs = MS.fromList $ map snd insts
--     dbgNotify $ "Scheduling cycle " ++ replicate (-cycle) ' '
--               ++ show (-cycle) ++ " " ++ show (length insts) 
--               ++ " " ++ show (length insts') ++ " " ++ pretty rsrcs 
    logP 2 "chosen instructions" insts
    logP 3 "Assume prev cycle" insts'

    -- 1.  Check which vars are actually live
    let defs = fst `S.map` foldMap (uncurry srcPlaces) insts
    let (currNewLives:nextNewLives) | null newLives = [[]]
                                    | otherwise = newLives
    let currNewLives' = filter (\(pv, _places) -> not (pv `S.member` defs)) 
                               currNewLives
    u <- newUndoToken
    lives' <- forM currNewLives' $ \(pv, places) -> do
        -- if the PC Unit is used for something (PC is read or a jump is
        -- performed), we cannot schedule a read from the immediate.
        when (rsrcs `MS.members` RPC > 0) $
          addUClause u [neg (RV pv cycle DrImm)]
        [(_, places')] <- setPossiblePlaces u cycle [(pv, places)]
        return (pv, S.filter (not . (`elem` [DrRead1, DrRead2])) places')

    let keepAlive (pv, locs) 
         | not (needStayinAlive pv) = Nothing
         | otherwise = Just (pv, S.filter (not . (`elem` [DrRead1, DrRead2])) locs)
    
    let lives'' = catMaybes (map keepAlive lives') ++ lives
    
    -- do the routing stuff
    r <- handleWrites u cycle insts' lives''
    case r of
     Nothing -> do undo u
                   if null insts' 
                    then return Nothing -- we failed to route with no assumptions
                    else scheduleCycle state' (init insts') 
                            -- (MS.insert (snd (last insts')) rsrcs) 
     Just (lives''',_occPlaces,_) -> do
      --dbgNotify "Writebacks OK"
      gopts <- seOptions `fmap` ask
      let nextNewLives' = addNewNextLives gopts insts nextNewLives

      -- add the routing constraints, return possible places in next cycle 
      lives'''' <- extendLiveness u cycle lives'''
      
      ok <- isRoutable

      logP' 2 "Routable" ok
      if not ok 
        then -- XXX
          do undo u
             if null insts' then return Nothing
                            else scheduleCycle state' (init insts')
                                 --  (MS.insert (snd (last insts')) rsrcs) 
        else do
        let required' = insts' --map fst insts''
        s <- schedule' opts (cycle-1) required' state' nextNewLives' lives''''
        case s of
          Nothing -> do
              logP' 3 "*** Back in cycle" cycle
              -- if we needed to backtrack it's because we assumed to much for
              -- the previous cycle, let's try again with fewer assumptions
              undo u
--               when (fromMaybe 42 maxNext <= 0) $
--                 error $ "We didn't assume anything and still could not schedule\n"
--                      ++ "This is weird.  Let's go home."
              if null insts' 
                then return Nothing
                else scheduleCycle state' (init insts') --(MS.insert (snd (last insts')) rsrcs) 
          Just s' -> return $ Just s'

-- | For each path variable and its possible locations add the constraints to
--   the SAT-solver.  Return for each path var all the places where it could
--   be in the next cycle.
setPossiblePlaces :: UndoToken 
                  -> Cycle
                  -> [(PathVar, Set DataResource)]
                  -> SchedM [(PathVar, Set DataResource)]
setPossiblePlaces u cycle vars = do
    forM vars $ \(pv, locations) -> do
      logP' 4 "Add read-from info" pv
      let rf = RV pv cycle DrRegFile
      let places = [ RV pv cycle loc | loc <- toList locations ]  
      -- PV must be in either of these places
      addUClause u places
      -- if the variable could be read from a read-port this implies that it
      -- must have been in the register file before.
      couldBeInRF <- forM places $ \pl@(RV _ _ dr) ->
        if (dr `S.member` readPorts) then do
          when (needStayinAlive pv) $
            addUClause u [neg pl, rf]  -- pl ==> rf
          return True
         else
          return False
      -- XXX: add interconnect information.  For now: new places are all
      -- previous places + possibly register file
      return $ (pv, if or couldBeInRF 
                    then S.insert DrRegFile locations
                    else locations)

extendLiveness :: 
       UndoToken 
    -> Cycle
    -> [(PathVar, Set DataResource)] 
       -- ^ Live path vars and possible places in current cycle.
    -> SchedM [(PathVar, Set DataResource)] 
extendLiveness u cycle lives' = do
    forM lives' $ \(pv, locations) -> do
        addUClause u [neg (RV pv (cycle-1) DrDef)]  -- not defined in next cycle
        let rf = RV pv (cycle-1) DrRegFile
        locss' <- forM (toList locations) $ \loc -> do
          let pl = RV pv cycle loc
          case () of { _ 
            | loc `elem` [DrRead1, DrRead2] -> 
                error "Read port is not admissible permanent location."
            | loc `elem` [DrBuf1, DrBuf2] -> do
                let places = [DrBuf1, DrBuf2, DrRead1, DrRead2]
                let rvs = [ RV pv (cycle-1) p | p <- places ]
                addUClause u (neg pl:rvs) -- pl ==> rvs
                return places
            | loc == DrRegFile -> do
                addUClause u [neg pl, RV pv cycle DrWrite, rf]
                addUClause u [neg (RV pv cycle DrWrite),RV pv (cycle-1) DrDef
                             ,RV pv (cycle-1) DrBuf1, RV pv (cycle-1) DrBuf1]
                return [DrRegFile]
            | otherwise -> 
                error $ "Unknown permanent location" ++ show loc
          }
        let allPlaces = foldMap S.fromList locss'
        let (regReads, places') = S.partition (`elem` [DrRead1, DrRead2]) 
                                  allPlaces
        forM_ (toList regReads) $ \pl ->
          addUClause u [ neg (RV pv (cycle-1) pl), rf ] -- pl ==> rf
          
        return (pv, places')

readyInsts :: Options -> Cycle -> SchedState -> SchedM (Set MicroOp)
readyInsts gopts cycle st = do
    deps <- asks seDepInfo
    let scheduled = M.filterWithKey (\m (c,r) -> c - instDelay gopts m r >= cycle)
                                    (ssScheduled st)
    let scheduledMops = M.keysSet scheduled
        allPredsScheduled i = dependents deps i `isSubsetOf` scheduledMops
    return $ S.filter allPredsScheduled (ssUnscheduled st)

readyInstsByPrio :: Options -> Cycle -> (MicroOp -> Priority) -> SchedState -> SchedM [MicroOp]
readyInstsByPrio gopts cycle prio st = do
  ready <- readyInsts gopts cycle st
  return $ map fst $ takeWhile ((< maxBound) . snd) $ sortBy (comparing snd)
         $ map (id &&& prio) $ toList ready

mobilityPrio :: SchedEnv -> Cycle -> MicroOp -> Priority
mobilityPrio env cycle mop
    | cycle > seAlap env ! mopid = maxBound
    | isBranch mop && cycle == (-branchDelaySlots) = minBound
    | otherwise = -(seAsap env ! mopid - seAlap env ! mopid)
  where mopid = mopInfo mop


-- | Given a list of instructions (sorted by priority) and a set of available
--   resources pick as many instructions as possible.
pickWithResources :: [MicroOp] -> MS.MultiSet OpResource 
                  -> [(MicroOp,OpResource)]
pickWithResources [] _rs = []
pickWithResources (mop:mops) rs =
  case opPlaces mop of
    p | p `MS.isSubsetOf` rs ->
        (mop, head (MS.toList p)) : pickWithResources mops (rs MS.\\ p)
    _ | otherwise ->
        pickWithResources mops rs

-- | Return all PVs that need to be routed by scheduling the given
--   instructions.

addNewNextLives :: 
     Options 
  -> [(MicroOp,OpResource)]  
  -> [[(PathVar, (Set DataResource))]]
  -> [[(PathVar, (Set DataResource))]]
addNewNextLives gopts is0 = addNewNextLives' (newLives is0)
  where
    addNewNextLives' [] ls = ls
    addNewNextLives' ((pv, n, places):r) lvs = addNewNextLives' r (ins lvs)
      where ins = modifyAt (n-1) ((pv, places):) mempty  
    newLives :: [(MicroOp,OpResource)]  
             -- ^ The instructions that need to be scheduled, ordered by
             -- priority (highest first).
             -> [(PathVar, Int, Set DataResource)]
    newLives is =
      let use = map (uncurry (dstPlaces gopts)) is
      in toList $ flip foldMap use $ \(delay,pvs) ->
           S.map (\(pv, _port) -> (pv, delay, readableFrom pv)) pvs
    


-- | This evaluates the effects of 
-- 
-- Given the set of live variables in this cycle and a list of instructions
-- assumed to be scheduled in the next cycle, returns the list of instructions
-- are still alive in the next cycle.  It also adds the information for the
-- routing part.
--
-- XXX: add information about required places.  Make sure that we have both
-- all possible places according to the previous routes and all places to be
-- able to route it to a possible direct read.
--
-- XXX: do the write-back magic
handleWrites ::        
       UndoToken
    -> Cycle
    -> [(MicroOp,OpResource)]  
    -> [(PathVar, (Set DataResource))]
    -> SchedM (Maybe ([(PathVar, Set DataResource)] -- remaining lives
                    ,[(PathVar, Set DataResource)] -- places used (needed?)
                    ,[MicroOp]))
handleWrites u currCycle assumedPrev currLives = do
    let def = fst `S.map` foldMap (uncurry srcPlaces) assumedPrev
    -- If a value is defined in the prev cycle it must be in any of the
    -- writable places in this cycle.  This also means that the variable
    -- cannot have been live in the previous cycle and it cannot have been in
    -- the register file.
    --
    -- We collect and return the places we defined, so that we can impose
    -- mutual exclusivity on those later on.
    writeBacks <- asks seWriteBacks
    let currLives' = M.fromList currLives 
    let writePlaces = S.fromList [DrBuf1, DrBuf2, DrWrite] 
    places <- forM (toList def) $ \pv -> do
      logP' 4 "Adding write-back info" (pv, currCycle)
      let rvs = [ RV pv currCycle pl | pl <- toList writePlaces ] 
      pl1 <- if (pv `M.member` currLives') 
        then do addUClause u rvs
                -- make sure it cannot be in the reg-file
                addUClause u [ neg $ RV pv (currCycle-1) DrRegFile ]
                addUClause u [ neg $ RV pv currCycle DrRegFile
                             , RV pv currCycle DrWrite ]
                return writePlaces
        else return mempty
      -- If this PV needs a write-back it *must* be in DrWrite.  But it may
      -- also be written to a buffer if that could be useful, so we just add
      -- both constraints.
      pl2 <- if (pv `S.member` writeBacks) 
             then do addUClause u [ RV pv currCycle DrWrite ]
                     return (S.singleton DrWrite)
             else return mempty
      return (pv, pl1 `mappend` pl2)
    
    ok <- isRoutable
    if ok then
        return $ Just $ 
           ( filter (\(k, _) -> k `S.notMember` def) currLives
           , places
           , [] )
     else do logP' 4 "Adding write-backs failed" currCycle
             return Nothing

needStayinAlive :: PathVar -> Bool
needStayinAlive (PV _ (Just (OrigRead _))) = False
needStayinAlive (PV _ (Just (OrigImm _))) = False
needStayinAlive _ = True

-- XXX: implement function that maps from set of current places to set of next
-- possible places


readableFrom :: PathVar -> Set DataResource
readableFrom (PV _ (Just (OrigRead _))) = readPorts
readableFrom (PV _ (Just (OrigImm _))) = S.singleton DrImm
readableFrom _ = tempPlaces

data OpResource
    = RALU | RMem | RPC | RMult | RWB --  RBuf
      deriving (Eq, Ord, Show)
instance PPrint OpResource where pshow = text . show

data DataResource
    = DrRead1 | DrRead2 | DrWrite | DrBuf1 | DrBuf2 | DrImm | DrRegFile | DrDef
      deriving (Eq, Ord, Show, Bounded, Enum)
instance PPrint DataResource where pshow = text . show

tempPlaces :: Set DataResource
tempPlaces = S.fromList [ DrRead1, DrRead2, DrBuf1, DrBuf2 ]

readPorts :: Set DataResource
readPorts = S.fromList [ DrRead1, DrRead2 ]
-- writePorts = S.fromList [ DrBuf1, DrBuf2, DrWrite ]

-- data PortName
--     = StoreP DataResource | UnitInP UnitInPort | UnitOutP UnitOutPort
--       deriving (Eq, Ord, Show)
-- instance PPrint PortName where pshow = text . show

data UnitInPort 
    = UipAluA | UipAluB | UipMultA | UipMultB | UipMemAddr | UipMemVal
    | UipPC | UipRegWrite
      deriving (Eq, Ord, Show)

data UnitOutPort
    = UopAluRslt | UopMultLo | UopMultHi | UopMemRead | UopPCFP
    | UopDummy
      deriving (Eq, Ord, Show)

coreOpResources :: MS.MultiSet OpResource
coreOpResources = MS.fromList [ RALU, RMem, RPC, RMult, RWB ]

-- coreDataResources :: MS.MultiSet DataResource
-- coreDataResources = 
--     MS.fromList [ DrRead1, DrRead2, DrWrite, DrBuf1, DrBuf2 ]

-- XXX: This is what it should do one day:
--   Return a list (sorted by preference) of resources the given
--   micro-op could be scheduled on.
opPlaces :: MicroOp -> MS.MultiSet OpResource
opPlaces (Mop _ _ op) = case op of
  ALU _ _ _  -> MS.singleton RALU
  Load _ _ _ -> MS.singleton RMem
  Store _ _ _  -> MS.singleton RMem
  MultMIPS _ _ -> MS.singleton RMult
  Branch _ _ _ -> MS.singleton RPC  -- do we need this?
  Jump _ _     -> MS.singleton RPC  -- or this?
  ReadPC       -> MS.singleton RPC
  WriteReg _ _ -> MS.singleton RWB
  _ -> error "opPlaces: Only valid for operations"

-- | Return the path vars and port names of all the input operands
--   that need to be routed to be able to schedule this unit.
--
-- XXX: ports should be a function of the unit.  Note, *not* a unit
-- *type* but of a particular unit.  (This matters if we have more
-- than one unit of any type.)
dstPlaces :: Options -> MicroOp -> OpResource -> (Int, Set (PathVar, UnitInPort))
dstPlaces gopts mop@(Mop _ _ op) r = 
   (instDelay gopts mop r, case (op, r) of
    (ALU _ a b, RALU)       -> S.fromList [ (a, UipAluA), (b, UipAluB) ]
    (Load _ _ a, RMem)      -> S.singleton (a, UipMemAddr)
    (Store _ a v, RMem)     -> S.fromList [ (a, UipMemAddr), (v, UipMemVal) ]
    (MultMIPS a b, RMult)   -> S.fromList [ (a, UipMultA), (b, UipMultB) ]
    (Branch _ c _addr, RPC) -> S.singleton (c, UipPC)
                             -- @_addr@ blocks the immediate port, but
                             -- it's encoded directly
    (Jump _ a, RPC)         -> S.singleton (a, UipPC)
    (ReadPC, RPC)           -> S.empty
    (WriteReg _ a, RWB)     -> S.singleton (a, UipRegWrite)
    _ -> error $ "Don't know dst-places for: " ++ show op)

multDelay :: Options -> Int
multDelay = optMultDelay

srcPlaces :: MicroOp -> OpResource -> Set (PathVar, UnitOutPort)
srcPlaces (Mop _ (Just x) op) r = case (op, r) of
  (ALU _ _ _, RALU)      -> S.singleton (x, UopAluRslt)
  (Load _ _ _, RMem)     -> S.singleton (x, UopMemRead)
  (Store _ _ _, RMem)    -> S.singleton (x, UopMemRead)
  (MultMIPS _ _, RMult)  -> S.singleton (x, UopMultLo)
  (ReadPC, RPC)          -> S.singleton (x, UopPCFP)
  (WriteReg _ _, RWB)    -> S.singleton (x, UopDummy)
  _ -> error $ "srcPlaces: unknown source places for " ++ pretty op
srcPlaces _ _ = mempty

instDelay :: Options -> MicroOp -> OpResource -> Int
instDelay gopts (Mop _ _ op) r = case (op, r) of
  (MultMIPS _ _, RMult) -> multDelay gopts
  (WriteReg _ _, RWB) -> 0
  _ -> 1

------------------------------------------------------------------------

type LogLevel = Int

logS :: LogLevel -> String -> SchedM ()
logS lvl msg = do
    l <- asks seLogLevel
    when (lvl <= l) $ 
      liftIO $ putStrLn msg
    
logP :: PPrint a => LogLevel -> String -> a -> SchedM ()
logP lvl label msg = do
    l <- asks seLogLevel
    when (lvl <= l) $ 
      liftIO $ putStrLn $ render $ text label <> char ':' $+$ nest 2 (pshow msg)

logP' :: PPrint a => LogLevel -> String -> a -> SchedM ()
logP' lvl label msg = do
    l <- asks seLogLevel
    when (lvl <= l) $ 
      liftIO $ putStrLn $ render $ text label <> char ':' <+> pshow msg

dbgNotify :: String -> SchedM ()
dbgNotify str = liftIO $ hPutStrLn stderr str

------------------------------------------------------------------------
      
sanityCheckSchedule :: (Schedule, Routes) -> Bool
sanityCheckSchedule (sched, routes) = 
    M.null sched ||
    and 
       
     [ forall [minCycle .. maxCycle] $ \c ->
         (forall (usesC c) $ \u -> 
 --          trace (pretty (readables (c-1)), pretty (defsC c))
           u `S.member` readables (c-1) || u `S.member` defsC (c-1))
       && (forall (defsC c) $ \d ->
             --trace (pretty d ++ pretty (usesC (c+1)) ++ pretty (writables (c+1))) 
             d `S.member` usesC (c+1) || d `S.member` writables (c+1)
             || c == maxCycle  -- XXX: HACK!
             )
       -- add:  path is credible and complete
     ]
  where  
    instsC c = fst `fmap` M.findWithDefault [] c sched
    usesC c = foldMap uses (instsC c)
    defsC c = foldMap defs (instsC c)
--     livesC c = M.findWithDefault mempty c lives
    readables c = S.map fst 
                . S.filter (not . (`elem` [DrWrite, DrDef, DrRegFile]) . snd)
                $ M.findWithDefault mempty c liveLocs
    writables c = S.map fst 
                . S.filter ((`elem` [DrWrite, DrBuf1, DrBuf2]) . snd)
                $ M.findWithDefault mempty c liveLocs
    cycles = toList (M.keysSet sched `mappend` M.keysSet routes)
    liveLocs :: Map Cycle (Set (PathVar, DataResource))
    liveLocs = (`fmap` routes) $ \m -> 
                 fold $ M.mapWithKey (\x s -> S.map (\y -> (y,x)) s) m
--     lives :: Map Cycle (Set PathVar)
--     lives = fmap (S.map fst) liveLocs
--     assertTrue str x = if (not x) then error $ "Failed to ensure: " ++ str else True
    minCycle = head cycles
    maxCycle = last cycles
--     forallCycles :: (Cycle,Cycle) -> Map Cycle a -> (Cycle -> a -> Bool) -> Bool
--     forallCycles (from,to) m pred =
--         all (uncurry pred) $ takeWhile ((<= to).fst) 
--                            $ dropWhile ((<= from).fst) $ M.assocs m
    forall :: Foldable f => f a -> (a -> Bool) -> Bool
    forall x f = all f x 


------------------------------------------------------------------------

--main = schedule l50



htmlSchedule :: Options -> BasicMopBlock -> (Schedule, Routes, RegAlloc) -> String
htmlSchedule gopts blk (sched, routes, regAlloc) =
    let cycs = toList $ M.keysSet sched `mappend` M.keysSet routes
    in unlines $ 
         [ "<html><head>"
         , "<style type=\"text/css\">table.sample {"
         , "border-width: 1px;"
         , "border-spacing: 0px;"
         , "border-style: solid;"
         , "border-color: #ddd;"
         , "border-collapse: collapse;"
         , "background-color: white;"
         , "}"
         , "table.sample th {"
         , "border-width: 1px;"
         , "padding: 1px;"
         , "border-style: inset;"
         , "border-color: #ddd;"
         , "background-color: white;"
         , "-moz-border-radius: 0px;"
         , "}"
         , "table.sample td {"
         , "border-width: 1px;"
         , "padding: 1px;"
         , "border-style: inset;"
         , "border-color: #ddd;"
         , "background-color: white;"
         , "-moz-border-radius: 0px;"
         , "}"
         , "</style>"
         , "</head><body>"
         , "<h1>Schedule for Basic Block " ++ pretty (bbLabel blk) ++ "</h1>"
         , "<table class=\"sample\">"
         , "<tr><th>C</th><th>Instrs</th>"
         , concatMap (\p -> "<th>" ++ show p ++ "</th>") [minBound .. (maxBound :: DataResource)]
         , "</tr>"
         , (unlines $ (`fmap` cycs) $ \c ->
             concat [ "<tr>"
                    , "<td>",  show c, "</td>"
                    , "<td>"
                    , intercalate "<br/>" (map (html . fst) (M.findWithDefault [] c sched))
                    , "</td>"
                    , let rs = M.findWithDefault mempty c routes in
                      concat $
                        (`fmap` [minBound .. maxBound]) $ \r ->
                          "<td>" ++ concatMap html (toList (M.findWithDefault mempty r rs)) ++ "</td>"
                    , "</tr>"
                    ]
           )
         , "</table>"
         , "WriteBack: " ++ intercalate ", " (map html (toList (bbInfo blk)))
         , "<br/>LiveOuts: " ++ intercalate ", " (map pretty (toList (bbLiveOut blk)))
         , "<pre>"
         , unlines $ map show $ flexSchedule gopts blk (sched, routes, regAlloc)
         , "</pre>"
         , "Last Register Reads: "
         , pretty (lastRegReads routes)
         , "<hr>Allocated Registers: "
         , pretty (allocateRegisters (bbLiveOut blk) routes)
         , "</body></html>"
         ]

class HTML a where html :: a -> String

instance HTML PathVar where
    html (PV i orig) = "<span style=\"color:#" ++ r ++ g ++ b ++ ";\">p" 
                      ++ show i ++ maybe "" html orig ++ "</span> "
      where r = col (i `mod` 5)
            g = col ((i `div` 5) `mod` 5)
            b = col ((i `div` 25) `mod` 5)
            col c = ["00", "33", "66", "99", "cc"] !! c
instance HTML PVOrigin where
    html (OrigRead r) = "<sub>=" ++ pretty r ++ "</sub>"
    html (OrigWrite r) = "<sub>" ++ pretty r ++ "=</sub>"
    html (OrigImm i) = "<sub>" ++ pretty i ++ "</sub>"

instance HTML MicroOp where
    html (Mop _ (Just pv) op) = html pv ++ " &larr; " ++ html op
    html (Mop _ Nothing ef)   = html ef

            
instance HTML Oper where
    html op = case op of
      Imm i -> show i
      ALU o s1 s2 -> html s1 +++ html o +++ html s2
      Load w s a  -> ch 'M' ++ html w ++ html s ++ "[" ++ (html a) ++ "]"
      ReadReg r   -> ch '$' ++ show r
      ReadBuf b   -> ch 'B' ++ show b
      ReadPC      ->  "PC"
      WriteReg _r p -> html p
      WriteBuf b p -> ch 'B' ++ show b +++  "&larr;" +++ html p
      Store w a s  -> ch 'M' ++ html w ++ "[" ++ (html a) ++ "]" +++
                         "&larr;" +++ html s
      MultMIPS x y -> html x +++ ch '*' +++ html y
      Branch z c a ->  "if" +++ html c +++ cmp z +++ 
                         "then goto" +++ html a
      Jump True a  ->  "call" +++ html a
      Jump False a ->  "goto" +++ html a
     where cmp F.Z  =  "== 0"
           cmp F.NZ =  "/= 0" 
           -- x +++ y = x ++ " " ++ y
           ch c = c:[]

instance HTML F.LSWidth where
    html w = "<sub>" ++ show (1 + fromEnum w) ++ "</sub>"

instance HTML F.Sign where
    html F.Signed = "<sub>,s</sub>"
    html F.Unsigned = "<sub>,u</sub>"

instance HTML F.ALUOp where
    html op = case op of
                F.AO_ADD -> "+"
                F.AO_ADDU -> "+<sub>u</sub>"
                F.AO_SUB -> "-"
                F.AO_SUBU -> "-<sub>u</sub>"
                F.AO_AND -> "&and;"
                F.AO_OR -> "&or;"
                F.AO_NOR -> "&#8891;"
                F.AO_XOR -> "&oplus;"
                F.AO_SLL -> "&#8810;"
                F.AO_SRL -> "&#8811;"
                F.AO_SHR -> "&#8811;<sub>S</sub>"
                F.AO_SLT -> "&lt;"
                F.AO_SLE -> "&le;"
                F.AO_SEQ -> "="
                F.AO_SNE -> "&ne;"
                F.AO_TEST -> "const"

------------------------------------------------------------------------

-- | Returns a list of "rtn" instructions for the FlexSoC simulator.
flexSchedule :: Options -> BasicMopBlock -> (Schedule, Routes, RegAlloc) -> [[FS.MicroOp]]
flexSchedule gopts _bb (ops, routes, regAlloc) =
   asRTN . processRoutes . processOps $ fops0
   
  where
    cyc0 = maybe 2 fst $ S.minView
           $ M.keysSet ops `mappend` M.keysSet routes

    asRTN fops = (`fmap` [cyc0..1]) $ \c ->
                   M.findWithDefault [] c fops

    fops0 :: Map Int [FS.MicroOp]
    fops0 = mempty

    processOps = M.unionWith (++) (M.fromListWith (++) $
        [ (c', [pr m c' lookupPV]) | (c,mops) <- M.assocs ops
                                   , (m,r)    <- mops
                                   , c'       <- [c + 1 - instDelay gopts m r]])
    
    processRoutes = M.unionWith (++) (M.fromListWith (++) $
        [ (c,[s]) | (c, dr2pv) <- M.assocs routes
                  , (dr, pvs)  <- M.assocs dr2pv
                  , [pv] <- [S.toList pvs] -- pick only 1-elem sets
                  , Just s <- [prD dr c pv lookupPV lookupReg lookupOcc] ])

    dataLocs :: Map Cycle (Map PathVar (Set DataResource))
    dataLocs = fmap invertSetMap routes

    defLocs = M.map (\insts -> 
                         M.fromAscList . S.toList $ foldMap (uncurry srcPlaces) insts)
                    ops

    -- Look up the register allocated to given PV during given cycle
    lookupReg :: PathVar -> Cycle -> FS.Register
    lookupReg p c = FS.R $
        let alloc = S.filter ((<=c) . fst) $
                    M.findWithDefault (error $ "no register defined for: " ++ pretty p
                                             ++ " in cycle " ++ show c)
                                      p regAlloc
        in if S.null alloc then error $ "No alloc: " ++ pretty (p,c) ++ "\n" ++ pretty regAlloc
                           else snd $ S.findMax alloc

    -- Look up the PV assigned to given data resource in given cycle.  If none
    -- is present a dummy is generated that is guaranteed not to be equal to
    -- any PV from the block.
    lookupOcc :: DataResource -> Cycle -> PathVar
    lookupOcc dr c = 
        case lookup2 c dr routes of
          Nothing -> dummyPV
          Just s -> if S.null s then dummyPV
                    else head . S.toList $ s
                        
    dummyPV = PV (-1) Nothing
              
    -- Return name of port at which given PV can be read from in given cycle.
    lookupPV :: PathVar -> Cycle -> FS.DataOutPortName
    lookupPV pv c = --pretty pv ++ 
        case lookup2 c pv dataLocs of
          Just s | isJust (find readablePlace s) -> 
              showDP . head . filter readablePlace $ S.toList s
          _ -> case lookup2 c pv defLocs of
                 Just s -> showUP s -- . head $ S.toList s
                 Nothing -> error "unknown out port name" --"***" ++ pretty dataLocs
    
    showDP :: DataResource -> FS.DataOutPortName
    showDP dr = case dr of
                 DrRead1 -> FS.Regbank_Out1
                 DrRead2 -> FS.Regbank_Out2
                 DrBuf1  -> FS.Buf1_Read
                 DrBuf2  -> FS.Buf2_Read
                 DrImm   -> FS.PC_ImmPC
                 _ -> error $ "not a readable place: " ++ show dr
    readablePlace :: DataResource -> Bool
    readablePlace dr = dr `elem` [DrRead1, DrRead2, DrBuf1, DrBuf2, DrImm]
                       
    showUP :: UnitOutPort -> FS.DataOutPortName
    showUP uop = case uop of
                   UopAluRslt -> FS.Alu_Rslt
                   UopMultLo  -> FS.Mult_LSW
                   UopMultHi  -> FS.Mult_MSW
                   UopMemRead -> FS.Ls_Read
                   UopPCFP    -> FS.PC_ImmPC
                   _ -> error $ "flexSchedule: unsupported unit out port: "
                              ++ show uop           

    
--t1 = M.fromList [(0, M.fromList [])] :: Map Cycle (Map DataResource (Set PathVar))
    

pr :: MicroOp -> Cycle -> (PathVar -> Cycle -> FS.DataOutPortName) -> FS.MicroOp
pr (Mop _ _dst op) c pv = case op of
  ALU aop a b           -> FS.ALUOp aop (pv a (c-1)) (pv b (c-1))
  Load w F.Signed a     -> FS.LSRead w (pv a (c-1))
  Load w F.Unsigned a   -> FS.LSReadU w (pv a (c-1))
  Store w a v           -> FS.LSWrite w (pv a (c-1)) (pv v (c-1))
  MultMIPS a b          -> FS.Mult (pv a (c-1)) (pv b (c-1))
  Jump _ (PV _ (Just (OrigImm c))) -> FS.PCJumpSA (FS.C c)
  Jump _ p              -> FS.PCJumpDA (pv p (c-1))
  Branch F.Z cond addr  -> FS.PCBEQZA (pvImm addr) (pv cond (c-1))
  Branch F.NZ cond addr -> FS.PCBNEZA (pvImm addr) (pv cond (c-1))
  ReadPC                -> FS.PCGetPC
  _ -> error $ "Don't know how to map '" ++ show op ++ "' into FlexSoC instructions." 

(+++) :: String -> String -> String
x +++ y = x ++ " " ++ y

pvImm :: PathVar -> FS.Const
pvImm (PV _ (Just (OrigImm c))) = FS.C c
pvImm _ = FS.C $ F.Addr "***NIRVANA***"

-- Print a data resource.
prD :: DataResource -> Cycle -> PathVar 
    -> (PathVar -> Cycle -> FS.DataOutPortName) -- Lookup location of PV in given cycle
    -> (PathVar -> Cycle -> FS.Register)  -- lookup register assigned to PV in given cycle
    -> (DataResource -> Cycle -> PathVar)
    -> Maybe FS.MicroOp
prD rsrc cyc p pv reg occ = case rsrc of
  DrRead1   -> Just $ FS.RegRead1 (readLoc p cyc)
  DrRead2   -> Just $ FS.RegRead2 (readLoc p cyc)
  DrWrite   -> Just $ FS.RegWrite (writeLoc p cyc) (pv p (cyc-1))
  DrImm     -> Just $ FS.PCImm (immVal p)
  DrBuf1 | occ DrBuf1 (cyc-1) /= p -> Just $ FS.Buf1 (pv p (cyc-1))
  DrBuf2 | occ DrBuf2 (cyc-1) /= p -> Just $ FS.Buf2 (pv p (cyc-1))
  _ -> Nothing
--   DrRegFile -> Nothing
--   DrDef     -> Nothing
 where immVal (PV _ (Just (OrigImm v))) = FS.C v
       immVal _ = error "PathVar does not represent an immediate"
       readLoc (PV _ (Just (OrigRead r))) _ = FS.R r
       readLoc p' cycle = reg p' cycle
       writeLoc (PV _ (Just (OrigWrite r))) _ = FS.R r
       writeLoc p' cycle = reg p' cycle
  
prReg :: F.Reg -> String
prReg (F.Reg n) = "R" ++ show n
    
--allocRegs = 

-- | For each register maps to the last cycle it is being read.  If the
--   register is not in the map it is never read (in the given basic block).
lastRegReads :: Routes -> Map F.Reg Cycle
lastRegReads routes = 
    M.fromListWith max $
      [ (r, cycle) | (cycle, dr2pvs) <- M.assocs routes
                   , (dr, pvs) <- M.assocs dr2pvs
                   , dr `elem` [DrRead1, DrRead2] 
                   , [PV _ (Just (OrigRead r))] <- [toList pvs]
      ]
{-
    unMaxMap $ execWriter $ unwrapMonad $
    for (M.assocs routes) $ \(cycle,dr2pvs) ->
      for (M.assocs dr2pvs) $ \(dr, pvs) -> WrapMonad $ 
        if dr `elem` [DrRead1, DrRead2] then
          case toList pvs of 
           [PV _ (Just (OrigRead r))] -> 
               tell $ MaxMap $ M.singleton r cycle
           [_] -> return ()
           [] -> return ()
           _ -> error $ "lastRegReads: too many elems on read port: " ++ show pvs
        else return ()

newtype MaxMap k a = MaxMap { unMaxMap :: Map k a }
    deriving (Eq, Show, Ord, Traversable, Functor, Foldable)
instance (Ord k, Ord a) => Monoid (MaxMap k a) where
    mempty = MaxMap M.empty
    mappend (MaxMap m1) (MaxMap m2) = MaxMap $ M.unionWith max m1 m2
-}
-- simple linear scan register allocator.  May fail if we don't have enough
-- free registers.
allocateRegisters :: Set F.Reg -> Routes -> Maybe (Map PathVar (Set (Cycle,F.Reg)))
allocateRegisters liveOuts routes = 
  (\(x,_,_) -> x) `fmap`
    (foldlMOver (M.assocs routes) (mempty,mempty,freeRegs0) $ 
      \(cycle,dr2pv) (ranges,oldInRegs,freeRegs) ->
        let inRegs     = M.findWithDefault mempty DrRegFile dr2pv
            newInRegs0 = inRegs `S.difference` oldInRegs
            -- some registers are live-out but might still be used after they
            -- have been written to the register file.  In this case we just
            -- reuse the actual write-back register and don't need to assign
            -- any of the free regs.
            (preAllocs, newInRegs) = S.partition isWriteBack newInRegs0
            noLongerInRegs         = toList $ oldInRegs `S.difference` inRegs
            newFreeRegs            = map lastReg noLongerInRegs
            lastReg pv             = snd $ S.findMax $ M.findWithDefault 
                                     (error "regAlloc: oops. Bug!") pv ranges
        in do (usedRegs, freeRegs') <- splitAtExactly (S.size newInRegs) 
                                           (filter (regUsable cycle) $ newFreeRegs ++ freeRegs)
              let ranges' = addPreAllocs $ foldr addPVReg ranges (zip (toList newInRegs) usedRegs)
                  addPVReg (pv,r) = M.insertWith mappend pv (S.singleton (cycle,r))
                  addPreAllocs = M.unionWith mappend preAllocMap 
                  preAllocMap = M.fromList $ map (\p@(PV _ (Just (OrigWrite r))) -> 
                                                      (p, S.singleton (cycle, r))) 
                                                 (toList preAllocs)
              return (ranges', inRegs, freeRegs'))
  where
    freeRegs0 = toList (S.fromList (map F.Reg [1..31]) `S.difference` liveOuts)
    lastReads = lastRegReads routes
    regUsable cycle r = M.findWithDefault minBound r lastReads < cycle
    isWriteBack (PV _ (Just (OrigWrite r))) | r `S.member` liveOuts = True
    isWriteBack _ = False
    

