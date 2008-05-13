module Types where

import Text.PrettyPrint.HughesPJ as PP
import PPrint

------------------------------------------------------------------------------
-- FlexCore Types

data Reg = Reg Int deriving (Show, Read, Eq, Ord)

instance PPrint Reg where
    pshow (Reg r) = char '$' <> int r

data LSWidth = LSW_1 | LSW_2 | LSW_3 | LSW_4 deriving (Eq, Read, Show, Enum)

data ALUOp 
    = AO_ADD     -- Add signed
    | AO_ADDU    -- Add unsigned
    | AO_SUB     -- Sub signed
    | AO_SUBU    -- Sub unsigned
    | AO_AND     -- Bitwise AND
    | AO_OR      -- Bitwise OR
    | AO_NOR     -- Bitwise NOR
    | AO_XOR     -- Bitwise XOR
    | AO_SLL     -- Shift left logical
    | AO_SRL     -- Shift right logical
    | AO_SHR     -- Shift right arithmetical signed
    | AO_SLT     -- Set on less than
    | AO_SLE     -- Set on less than or equal
    | AO_SEQ     -- Set on equal
    | AO_SNE     -- Set on not equal
    | AO_TEST    -- Return operand 1                
      deriving (Read, Show, Eq, Ord, Enum, Bounded)


------------------------------------------------------------------------------
-- MIPS Types and Compiler Types

data Label 
    = AutoLabel Int 
    | NamedLabel String
    deriving (Ord, Eq, Read, Show)

instance PPrint Label where
    pshow (AutoLabel n) = text "$_" <> int n
    pshow (NamedLabel n) = text n


data Instr r c
    -- = --Label String
    = Load LSWidth Sign r r c  -- width sign dest base offset
    | Store LSWidth r r c      -- width value base offset
    | ALUr ALUOp r r r         -- op dest op1 op2
    | ALUi ALUOp r r c         -- op dest op1 op2
    --  LUI r c                  -- (load upper) dest val
    | Br BCond r c             -- cond val absDest
    | BrALUr BCond ALUOp r r c -- cond op op1 op2 absDest
    | BrALUi BCond ALUOp r c c -- cond op op1 op2 absDest
    | J c                      -- absDest
    | JR r                     -- absDest
    | JAL r c                  -- linkr absDest
    | JALR r r                 -- linkReg absDest
    | Mult3 r r r              -- dest src1 src2
    | MultMIPS r r     -- src1 src2 (low 32 bits of result placed in Buf3)
    | MFHi r 
    | MFLo r 
    | UserNOP String   -- the String argument is included for profiling
    | AutoNOP String   -- the String argument is included for profiling
    | Error String
      deriving (Eq, Read, Show)


data Sign = Signed | Unsigned deriving (Eq, Read, Show)

data BCond = Z | NZ deriving (Eq, Read, Show)
 
instance PPrint LSWidth where
    pshow LSW_1 = text "b"
    pshow LSW_2 = text "h"
    pshow LSW_3 = text "3"
    pshow LSW_4 = text "w"

instance PPrint Sign where
    pshow Signed = empty
    pshow Unsigned = text "u"

instance (Show r, Show c, PPrint r, PPrint c) => PPrint (Instr r c) where
    pshow inst = case inst of
      Load w s dst base offs -> 
          text "l" <> pshow w <> pshow s <+> 
            pshow dst <> comma <> pshow offs <> parens (pshow base)
      Store w val base offs ->
          text "s" <> pshow w  <+> 
            pshow val <> comma <> pshow offs <> parens (pshow base)
      x -> text (show x)