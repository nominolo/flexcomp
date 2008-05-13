{-# OPTIONS -fglasgow-exts #-}
module GPP where

import Types
import PPrint
import Text.PrettyPrint.HughesPJ as PP

import Data.Set as S ( Set, singleton, fromList, empty )

import Control.Monad -- ( ap )
import Test.QuickCheck
------------------------------------------------------------------------------

type ASMInstr = Instr Reg Const
type GPPInstr = Instr Reg Int

-- | Symbolic data type for numerical constants.  A constant can be an integer,
-- a data- or code address, or a sum, difference, or product of other
-- constants.
--
-- It implements the Num class, so it is possible to write e.g. DataAddr "foo"
-- + 3*8 to refer to element number 3 in the array foo where elements have
-- size 8.  Warning: equality means syntactic equality
data Const = Int Int
	   | Addr String
	   | Const :+: Const
	   | Const :-: Const
	   | Const :*: Const
	   | ConstFun String [Const]
  deriving (Eq, Show, Read)

instance PPrint Const where
    pshow (Int i) = int i
    pshow (Addr n) = text n
    pshow (c1 :+: c2) = parens (pshow c1 <+> text "+" <+> pshow c2)
    pshow (c1 :-: c2) = parens (pshow c1 <+> text "-" <+> pshow c2)
    pshow (c1 :*: c2) = parens (pshow c1 <+> text "*" <+> pshow c2)
    pshow (ConstFun n as) = char '%' <> text n <+> 
                            parens (fsep $ punctuate comma $ map pshow as) 

-- Valid functions:
--   ConstFun "hi" [c]    returns  c >> 16
--   ConstFun "lo" [c]    returns  c && 0xffff
--   ConstFun "toHi" [c]  returns  (c && 0xffff) << 16
--------------------------------------------------------------------------------

instance Num Const where
  (+) = (:+:)
  (-) = (:-:)
  (*) = (:*:)
  negate (Int n) = Int (negate n)
  negate c = Int 0 :-: c
  abs = error ("abs not defined for Const")
  signum = error ("signum not defined for Const")
  fromInteger = Int . fromInteger


------------------------------------------------------------------------------

class Register r where
    physReg :: Int -> r

instance Register Reg where
    physReg = Reg

class Ord r => Instruction i r | i -> r where
    defs :: i -> Set r
    uses :: i -> Set r

instance Ord r => Instruction (Instr r c) r where
--defs :: Ord r => Instr r c -> Set r
 defs i = case i of
  Load _ _ d _ _ -> singleton d
  ALUr _ d _ _   -> singleton d
  ALUi _ d _ _   -> singleton d
 -- LUI d _        -> singleton d
  JAL d _        -> singleton d
  JALR d _       -> singleton d
  Mult3 d _ _    -> singleton d
  MFHi d         -> singleton d
  MFLo d         -> singleton d
  _              -> S.empty


 --uses :: Ord r => Instr r c -> Set r
 uses i = case i of
  Load _ _ _ b _     -> singleton b
  Store _ v b _      -> fromList [v, b]
  ALUr _ _ s1 s2     -> fromList [s1, s2]
  ALUi _ _ s _       -> singleton s
  Br _ s _           -> singleton s
  BrALUr _ _ s1 s2 _ -> fromList [s1, s2]
  BrALUi _ _ s _ _   -> singleton s
  JR a               -> singleton a
  JALR _ a           -> singleton a
  Mult3 _ s1 s2      -> fromList [s1, s2]
  MultMIPS s1 s2     -> fromList [s1, s2]
  _                  -> S.empty


isBranch :: Instr r c -> Bool
isBranch i = case i of
  Br _ _ _         -> True
  BrALUr _ _ _ _ _ -> True
  BrALUi _ _ _ _ _ -> True
  J _              -> True
  JR _             -> True
  JAL _ _          -> True
  JALR _ _         -> True
  _ -> False

data BranchTarget r c
    = Unconditional (BranchType r c)
    | Conditional (BranchType r c)
    | Call (BranchType r c)

data BranchType r c = Direct c | Indirect r

branchTarget :: Instr r c -> BranchTarget r c
branchTarget i = case i of
  J c      -> Unconditional (Direct c)
  JR r     -> Unconditional (Indirect r)
  Br _ _ c         -> Conditional (Direct c)
  BrALUr _ _ _ _ c -> Conditional (Direct c)
  BrALUi _ _ _ _ c -> Conditional (Direct c)
  JAL _ c  -> Call (Direct c)
  JALR _ r -> Call (Indirect r)
  _ -> error "Branch instruction expected"
  
instance Arbitrary Const where
  arbitrary = frequency 
                [ (4, Int `fmap` arbitrary)
                , (1, Addr `fmap` listOf1 (choose ('a','z')))
                ]
                
instance Arbitrary LSWidth where
  arbitrary = elements [LSW_1, LSW_2, LSW_4]
  
instance Arbitrary ALUOp where
  arbitrary = elements [minBound .. maxBound]
  
instance Arbitrary Reg where
  arbitrary = sized $ \s -> Reg `fmap` choose (0, max 3 (min 32 s))
  
instance Arbitrary Sign where
  arbitrary = elements [Signed, Unsigned]
  
instance Arbitrary BCond where
  arbitrary = elements [ Z, NZ ]

instance (Arbitrary r, Arbitrary c) => Arbitrary (Instr r c) where
  arbitrary = sized $ \s -> frequency $
    [(s, frequency $
       [ (5, Load `fmap` arbitrary `ap` arbitrary `ap` arbitrary
                  `ap` arbitrary `ap` arbitrary)
       , (5, Store `fmap` arbitrary `ap` arbitrary `ap` arbitrary
                   `ap` arbitrary)
       , (5, ALUr `fmap` arbitrary `ap` arbitrary `ap` arbitrary
                  `ap` arbitrary)
       , (5, ALUi `fmap` arbitrary `ap` arbitrary `ap` arbitrary
                  `ap` arbitrary)
       --, (1, Mult3 `fmap` arbitrary `ap` arbitrary `ap` arbitrary)
       , (2, MultMIPS  `fmap` arbitrary `ap` arbitrary)
       , (2, MFLo `fmap` arbitrary)
       , (1, MFHi `fmap` arbitrary)
       , (1, return $ AutoNOP "")
       ])
    ,(1, oneof $
       [ Br `fmap` arbitrary `ap` arbitrary `ap` arbitrary
       , BrALUr `fmap` arbitrary `ap` arbitrary `ap` arbitrary
                  `ap` arbitrary `ap` arbitrary
       , BrALUi `fmap` arbitrary `ap` arbitrary `ap` arbitrary
                  `ap` arbitrary `ap` arbitrary
       , J `fmap` arbitrary
       , JR `fmap` arbitrary
       , JAL `fmap` arbitrary `ap` arbitrary
       , JALR `fmap` arbitrary `ap` arbitrary
       ])
    ]
