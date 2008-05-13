--  Translate from MIPS instructions to internal MicroOps.
--
-- The difficulty is that in MIPS registers are used for every temporary
-- result, while we can route values directly or through buffers.  In
-- principle we could also route values across basic block boundaries, but for
-- simplicity we will chose not to (at least not for now).  Our translation
-- theme therefore works like this.
--
--  * Whenever we encounter a MIPS operation storing a result in a register,
--    we generate a new @PathVar@ and record this as a synonym for the
--    register used in the MIPS code.
--
--  * Whenever we encounter a read from a register, we check whether we have
--    recorded a synonym for that variable.  If we find one, we use the path
--    variable as operand.  If not, we generate a new path variable and record
--    it as a synonym for the register.
--
--  * For immediates we always generate a new path variable.  (This could be
--    changed in order to avoid loading the same immediate twice at the
--    expense of having to keep the value alive longer, thus putting more
--    pressure on the register file and buffers.)
--
--  * We annotate each path variable with the location it came from or was
--    supposed to go to (register or immediate).
--
--  * Some variables are read outside the current block (ie., are live-out).
--    We have to make sure that those values are actually written back to the
--    register at some point.  We therefore tag all the pathvars that are the
--    synonyms for a live-out variable at the end of the code.  The scheduler
--    will then ensure to insert write-backs for each of those @PathVar@s.
--
-- For example, the following mips code
--
-- @
--   add $1, $2, $3
--   sub $4, $1, $2
--   ld $1, 2($4)    ; $1 live out
-- @
--
-- gets translated into:
--
-- @
--   p3 <- p1 + p2   ; p1(read, $2), p2(read, $3), p3(rslt, $1), p4(rslt, $4),    
--   p4 <- p3 - p1   ; p5(=Imm 1), p6(-)
--   p6 <- p4 + p5   ; p7(rslt, $1),  needs writeback:  p7
--   p7 <- M[p6]
-- @
--
-- There are some additional things we need to take care of.
--
--  * For the MIPS architecture it doesn't make sense to store an address once
--    calculated since the it cannot use the ALU for other things when a
--    memory operation is scheduled.  For FlexSoC, however, it can be
--    beneficial to save a calculated address and reuse that value whenever
--    the same base and offset are used.  This puts some larger constraints on
--    the data path but it saves an ALU operation for each subsequent use of
--    the address, thus reducing pressure on an important bottleneck.
--
--  * MIPS only supports 16 bit immediates.  Therefore, whenever a 32 bit
--    value needs to be loaded into a register two instructions need to be
--    generated:
--
--      > lui $4,%hi($symbol)
--      > addiu $4,$4,%lo($symbol)
--
--    Note the use of the special functions "%hi" and "%lo".  This special
--    combination leads to problems with our approach of aliasing registers to
--    path variables, since now we will get an addition of two path vars which
--    both come from the immediate port.  However, we actually don't need
--    that.  FlexSoC supports 32 bit immediates, therefore we could just alias
--    the final $4 to the value (address) of "$symbol".
--
--  * Still, just aliasing is not always enough.  We use registers to route
--    values between basic blocks, hence if just want to call a function with
--    an immediate argument (or initialize a loop counter) we must write a
--    value without performing an operation with it.  We therefore add special
--    'RegWrite' instructions for all registers that are live-out and are not
--    the result of a computation.
--
--  * In MIPS multiplication consists of two operations, one to start the
--    multiplication and one to to get each result.  If they are paired
--    properly this is no problem, since we can associate the last read
--    (@mflo@) with the most recent multiplication.  However, sometimes the
--    multiplication was started in a previous basic block.  In this case we
--    use a special register ($42) to mark that the corresponding PV actually
--    refers to the output of the multiplier which we assume has been defined
--    earlier.
--
--  * XXX: ATM we don't correctly translate a @mlhi@
--  
module Mips2Mops
  ( mipsToMops
  , BasicMopBlock
  )  where

import Types as F
import GPP   as F
import MicroOp as O hiding ( eff, def )
import MipsCFG
import CFG
import Liveness

import Utils
import IDGen
import PPrint

import Data.Set ( Set )
import Data.Map ( Map )
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe ( maybeToList, catMaybes )

import Control.Monad ( mapM_, liftM )
import Control.Monad.State
import Control.Applicative ( (<$>), (<*>), pure )

import Debug.Trace

------------------------------------------------------------------------------

--  Transform a set of blocks of (MIPS) assembly instructions into blocks of
--  micro-ops.
mipsToMops :: Map Label (BasicBlock ASMInstr) 
           -> Set Label
           -> Map Label BasicMopBlock
mipsToMops bbs entries = M.mapWithKey convBlock bbs
  where
    convBlock label blk = mips2mops' blk lvIn lvOut
      where (lvIn, lvOut) = M.findWithDefault (S.empty, S.empty) label lvs
    lvs = lives bbs entries

----------------------------------------------------------------------

-- Some properties.

-- 1) We cannot have a read of a register after a write to the register
-- and not use the same PV for both.
--
-- 2) The same register can only need a write-back once.
-- 

-- XXX: We have the invariant that the PV id is always the same as the MicroOp
-- id.  This might seem a little hackish.
def :: MonadIDGen m => Oper -> Maybe PVOrigin -> m (PathVar, MicroOp)
def op orig = do iid <- genID
                 let p = PV iid orig
                 return (p, Mop iid (Just p) op)

-- checks the above mentioned invariant (incomplete)
prop_pv_id BB{ bbInstrs = instrs } = all isOk instrs
  -- XXX: the input BB should be a MIPS block, not a Mops block.
  where 
    isOk (Mop i (Just (PV j _)) _) = i == j
    isOk _ = True
              
-- | Generate an instruction that does not return any result.
eff :: MonadIDGen m => Oper -> m MicroOp
eff e = do iid <- genID; return $ Mop iid Nothing e

type LiveIn = Set Reg
type LiveOut = Set Reg

data TransState = TS
    { tsMops       :: [MicroOp]
    , tsRegAliases :: Map Reg (PathVar, Bool)
    , tsMultAlias  :: Maybe PathVar   
    -- ^ PV to use for next "mflo" instruction
    , tsAddrAliases :: Map (PathVar, Int) PathVar
--    , tsCopyWrites :: Set PathVar 
    -- ^ PVs that are writes to registers which are not the result of an
    -- operation, thus need to be written back.
    } deriving Show

type TransM a = StateT TransState IDGen a

type BasicMopBlock = BasicBlock' MicroOp (Set PathVar)

mips2mops' :: BasicBlock ASMInstr 
           -> LiveIn 
           -> LiveOut 
           -> BasicMopBlock
mips2mops' BB{ bbLabel=l, bbInstrs=asms, bbExit=ex } liveIn liveOut =
    worker (reverse asms)
  where 
    worker insts = (runIDGen 1 . (`evalStateT` state0)) 
                 $ translate insts

    state0 = TS { tsMops = []
                , tsRegAliases = M.empty
                , tsMultAlias = Nothing
                , tsAddrAliases = M.empty
                }
    
    translate :: [ASMInstr] -> TransM BasicMopBlock
    translate insts = do 
        -- First, translate all instructions into micro-ops.  Don't try to
        -- write back registers, because that might not be needed.
        mapM_ m2m insts
        -- Finally, we have to translate the branch instruction into
        -- micro-ops.  One instruction may be translated to multiple
        -- micro-ops, so we add an arbitrary number of micro-ops here, but
        -- assume that the actual branch will be the last instruction.
        withMaybe (exitInst ex) (return ()) m2m 
        wbs <- determineWriteBacks
        s <- get
        trace (pretty (tsRegAliases s)) $ return ()
        insts' <- gets tsMops
        case ex of
          Exit              -> return $ BB l insts' Exit liveOut wbs
          ImplicitJump l2   -> return $ BB l insts' (ImplicitJump l2) liveOut wbs
          _ -> do let (xi:is') = insts' 
                  case ex of 
                    ExBranch ml _     -> 
                        return $ BB l is' (ExBranch ml xi) liveOut wbs
                    ExCBranch ml l2 _ -> 
                        return $ BB l is' (ExCBranch ml l2 xi) liveOut wbs
                    ExCall ml l2 _    -> 
                        return $ BB l is' (ExCall ml l2 xi) liveOut wbs


    determineWriteBacks :: TransM (Set PathVar)
    determineWriteBacks = do
        s <- gets tsRegAliases
        -- PVs that need to be written back are all those that have
        -- been written to in this basic block
        let reg2pv r@(Reg n) = 
              do let mp =  M.lookup r s 
                 withMaybe mp (return Nothing) $ \(pv@(PV _ orig),_) ->
                   case orig of
                     -- this PV is an alias for an actual write-back: the
                     -- write-back will be scheduled with the operation
                     (Just (OrigWrite _)) -> return (Just pv)
                     -- the PV was not the result of some write-back, we need
                     -- to add an actual write-back instruction, otherwise the
                     -- new value for the register will never be written back
                     (Just (OrigRead r')) | r == r' -> return Nothing
                     _ -> --def' (WriteReg n pv) (Just (OrigWrite r)) >>= return . Just 
                          return Nothing
        pvs <- mapM reg2pv (S.toList liveOut)
        return $ S.fromList $ catMaybes pvs
--        let pvs'acts = reg2pv `mapMaybe` liveOut
        

-- Adds an instruction to the front of the list
addInst :: MicroOp -> TransM ()
addInst i = modify $ \s -> s { tsMops = i : tsMops s }

def' op orig = do (p, i) <- def op orig; addInst i; return p
eff' e = eff e >>= addInst
aliasReg = aliasReg' False
aliasReg' b r pv = modify $ \s -> s { tsRegAliases = M.insert r (pv, b) (tsRegAliases s) }
--aliasRegFo r pv = modify $ \s -> s { tsRegAliases = M.insert r (pv, True) (tsRegAliases s) }

-- newPV :: MonadIDGen m => m PathVar
-- newPV = do i <- genID; return $ PV i Nothing
            
-- Read a register.  If the register already has an aliased PV, return
-- this.  Otherwise, load the register, assign it a PV and add the alias
-- definition.
readR :: Reg -> TransM PathVar
readR = readR' False

readR' :: Bool -> Reg -> TransM PathVar
readR' b r@(Reg rn) = do 
        reg2pv <- gets tsRegAliases
        let rpv = M.lookup r reg2pv
        case rpv of
          Just (pv,_) -> return pv
          Nothing -> do pv <- liftM2 PV genID (return (Just $ OrigRead r))
                        --pv <- def' (ReadReg rn) (Just $ OrigRead r)
                        aliasReg' b r pv
                        return pv

imm i = liftM2 PV genID (return (Just $ OrigImm i))
         --def' (O.Imm i) (Just $ OrigImm i)

alu :: F.ALUOp -> PathVar -> PathVar -> Maybe F.Reg -> TransM PathVar
alu op x y dst = def' (O.ALU op x y) (OrigWrite `fmap` dst)
                 
addr base (Int 0) = readR base
addr (Reg 0) c = imm c
addr base offs = 
    do pBase <- readR base
       case offs of
         Int n -> 
             do addrAlias <- gets tsAddrAliases
                case M.lookup (pBase, n) addrAlias of
                  Just p -> return p
                  Nothing -> do
                    pImm <- imm offs
                    p <- def' (O.ALU AO_ADD pBase pImm) Nothing
                    modify $ \st -> 
                        st { tsAddrAliases = M.insert (pBase, n) p (tsAddrAliases st)}
                    return p

writeR r@(Reg rn) pv = do aliasReg r pv
--                          eff' $ O.WriteReg rn pv

writeRegOnly r@(Reg rn) pv = do aliasReg' True r pv

br cond val dest = eff' $ O.Branch cond val dest

jmp c dest = eff' $ O.Jump c dest
    
getPC = def' ReadPC Nothing

multMIPS a b = do p <- def' (O.MultMIPS a b) Nothing
                  modify $ \s -> s { tsMultAlias = Just p }
    
getMFLo :: TransM PathVar
getMFLo = do mpv <- gets tsMultAlias
             case mpv of
               -- XXX: we use some special register here, which stands for the
               -- output of the multiplier
               Nothing -> readR (Reg 42)
               --Nothing -> error "Buf3 is being read from without a prior multiplication."
               Just pv -> return pv



m2m inst = case inst of 
      F.Load width sign dst adr offs ->
          do pAddr <- addr adr offs
             pRes <- def' (O.Load width sign pAddr) (Just $ OrigWrite dst)
             aliasReg dst pRes
      F.Store width val base offs ->
          do pAddr <- addr base offs
             pVal <- readR val
             eff' $ O.Store width pAddr pVal
{-      F.ALUr F.AO_ADD dst (F.Reg 0) s2 ->
          do t <- readR s2
             writeRegOnly dst t
      F.ALUr F.AO_ADD dst s1 (F.Reg 0) ->
          do t <- readR s1
             writeRegOnly dst t -}
      F.ALUr op dst s1 s2 ->
          do t1 <- readR s1; t2 <- readR s2
             d <- alu op t1 t2 (Just dst)
             writeR dst d
{-      F.ALUi F.AO_ADD dst s1 (F.Int 0) ->
        -- in case @s1 == Reg 0@ we actually have two variants of expressing the
        -- same operation.  We could use this flexibility to shift pressure from
        -- the register file to the immediate or vice versa
          do t <- readR s1
             writeRegOnly dst t -}
      F.ALUi F.AO_ADD dst (F.Reg 0) c ->
          do t <- imm c
             writeR dst t
      F.ALUi op dst s1 c ->
          do t1 <- readR s1  
             -- recognize the pattern:
             --   > lui $r,hi(X)
             --   > addui $r,$r,lo(X)
             -- and translate it to direct register move or aliasing
             case (op, t1, c) of
               ( F.AO_ADDU
                , PV _ (Just (OrigImm (ConstFun "toHi" [ConstFun "hi" [s]])))
                , (ConstFun "lo" [s'])) | s == s' && s1 == dst -> 
                    do t2 <- imm s
                       t0 <- readR (F.Reg 0)
                       t <- alu op t2 t0 (Just dst)
                       writeR dst t
                       --writeRegOnly dst t2
               _ -> do t2 <- imm c;  
                       t <- alu op t1 t2 (Just dst)
                       writeR dst t
      F.Br cond v l ->
          do addr <- imm l
             val <- readR v
             br cond val addr
      F.BrALUr cond op s1 s2 l ->
          do t1 <- readR s1;       t2 <- readR s2
             val <- alu op t1 t2 Nothing;  addr <- imm l
             br cond val addr
      F.BrALUi cond op s1 s2 l ->
          do t1 <- readR s1;       t2 <- imm s2
             val <- alu op t1 t2 Nothing;  addr <- imm l
             br cond val addr
      F.J c ->
          do t <- imm c
             jmp False t
      F.JR r ->
          do t <- readR r
             jmp False t
      F.JAL lr c ->
          do t <- getPC;  a <- imm c
             r0 <- readR (F.Reg 0)
             pc' <- alu AO_ADDU t r0 (Just (F.Reg 31))
             writeR (F.Reg 31) pc'
             --writeRegOnly lr t;  
             jmp True a
      F.JALR r dst ->
          do t <- getPC;  a <- readR dst
             writeRegOnly r t;  jmp True a
      -- 
      F.MultMIPS s1 s2 ->
          do t1 <- readR s1;  t2 <- readR s2
             multMIPS t1 t2
      F.MFLo dst ->
          do t <- getMFLo
             writeRegOnly dst t

      F.UserNOP _ -> return ()
      x -> error $ "not implemented: " ++ show x


tst = pprint $ --runIDGen 1 $ 
        mips2mops' bb S.empty S.empty 
  where bb = BB (NamedLabel "foo")
                (reverse $ fftL50 )
                  {-[ F.Load F.LSW_2 F.Signed (Reg 2) (Reg 1) (Int 42)
                  , F.Store F.LSW_2 (Reg 3) (Reg 2) (Int 0)
                  ]) -- -}
                Exit
                S.empty
                ()

tst1 = pprint $ --runIDGen 1 $ 
        mips2mops' bb S.empty (S.singleton (Reg 1))
  where bb = BB (NamedLabel "x")
                (reverse tst1_block)
                Exit
                S.empty
                ()

tst1_block = [ ALUr AO_ADD (Reg 1) (Reg 2) (Reg 3)
             , ALUr AO_SUB (Reg 4) (Reg 1) (Reg 2)
             , F.Load LSW_4 Unsigned (Reg 1) (Reg 4) (Int 2)
             ]

fftL50 = [ ALUi AO_SLL (Reg 6) (Reg 8) (Int 1)
         , ALUr AO_ADDU (Reg 6) (Reg 6) (Reg 17)
         , F.Load LSW_2 Signed (Reg 4) (Reg 6) (Int 0)
         , F.Load LSW_2 Signed (Reg 10) (Reg 6) (Int 512)
         , F.MultMIPS (Reg 25) (Reg 4)
         , ALUr AO_ADDU (Reg 14) (Reg 14) (Reg 15)
         , ALUi AO_SLL (Reg 5) (Reg 13) (Int 1)
         , ALUr AO_ADDU (Reg 5) (Reg 5) (Reg 17)
         , F.Load LSW_2 Unsigned (Reg 7) (Reg 5) (Int 512)
         , F.Load LSW_2 Unsigned (Reg 11) (Reg 5) (Int 0)
         , ALUr AO_ADDU (Reg 13) (Reg 15) (Reg 13)
         , ALUr AO_SUBU (Reg 9) (Reg 14) (Reg 15)
         , MFLo (Reg 12)
         , ALUr AO_ADDU (Reg 8) (Reg 8) (Reg 15)
         , ALUr AO_SLT (Reg 9) (Reg 9) (Reg 16)
         , F.MultMIPS (Reg 24) (Reg 10)
         , MFLo (Reg 2)
         , ALUr AO_SUBU (Reg 2) (Reg 2) (Reg 12)
         , ALUi AO_SHR (Reg 2) (Reg 2) (Int 15)
         , F.MultMIPS (Reg 24) (Reg 4)
         , ALUr AO_SUBU (Reg 7) (Reg 7) (Reg 2)
         , F.Store LSW_2 (Reg 7) (Reg 6) (Int 512)
         , F.Load LSW_2 Unsigned (Reg 3) (Reg 5) (Int 512)
         , MFLo (Reg 4)
         , ALUr AO_ADDU (Reg 2) (Reg 2) (Reg 3)
         , F.Store LSW_2 (Reg 2) (Reg 5) (Int 512)
         , F.MultMIPS (Reg 25) (Reg 10)
         , MFLo (Reg 10)
         , ALUr AO_ADDU (Reg 4) (Reg 4) (Reg 10)
         , ALUi AO_SHR (Reg 4) (Reg 4) (Int 15)
         , ALUr AO_SUBU (Reg 11) (Reg 11) (Reg 4)
         , F.Store LSW_2 (Reg 11) (Reg 6) (Int 0)
         , F.Load LSW_2 Unsigned (Reg 2) (Reg 5) (Int 0)
         , UserNOP ""
         , ALUr AO_ADDU (Reg 4) (Reg 4) (Reg 2)
         , F.Store LSW_2 (Reg 4) (Reg 5) (Int 0)
         , BrALUr NZ AO_SNE (Reg 9) (Reg 0) (Addr "$L50")
         ]

-- -}