module FlexsocTypes where

import Types ( ALUOp(..), LSWidth(..), Reg(..) )
import Text.ParserCombinators.ReadP
import Data.Char ( isDigit )
import qualified GPP as G ( Const(..) )
import PPrint

newtype Register = R Reg deriving Eq
instance Show Register where
  show (R (Reg n)) = 'R' : show n
{-
instance Read Register where
  readsPrec _ = readP_to_S $ 
                  (R . Reg) `fmap` (char 'R' >> (read `fmap` munch1 isDigit))
  -}                                    
newtype Const = C G.Const deriving Eq
instance Show Const where
    show (C c) = pretty c

data DataOutPortName 
    = PC_ImmPC
    | Regbank_Out1 | Regbank_Out2
    | Alu_Rslt
    | Ls_Read
    | Buf1_Read | Buf2_Read | Buf3_Read 
    | Mult_LSW | Mult_MSW                 
    deriving (Show, Eq, Ord, Enum, Bounded)

data DataInPortName 
    = PC_FB
    | Regbank_Write 
    | Alu_OpA | Alu_OpB 
    | Ls_Address | Ls_Write
    | Buf1_Write | Buf2_Write | Buf3_Write 
    | Mult_OpA | Mult_OpB  
    deriving (Show, Eq, Ord, Enum, Bounded)

data MicroOp = -- PC unit
               PCImm Const
	     | PCGetPC
	     | PCJumpSA Const
	     | PCJumpSR Const
	     | PCJumpDA DataOutPortName
	     | PCBEQZR Const DataOutPortName  -- destination value 
	     | PCBNEZR Const DataOutPortName  -- destination value 
	     | PCBEQZA Const DataOutPortName  -- destination value
	     | PCBNEZA Const DataOutPortName
	     -- Register bank
	     | RegRead1 Register
	     | RegRead2 Register
	     | RegWrite Register DataOutPortName
	     -- ALU
	     | ALUOp ALUOp DataOutPortName DataOutPortName
	     -- Load/store
	     | LSWrite LSWidth DataOutPortName DataOutPortName --size addr value
	     | LSRead LSWidth DataOutPortName --size addr
	     | LSReadU LSWidth DataOutPortName --size addr
	     -- Buffers
	     | Buf1 DataOutPortName
	     | Buf2 DataOutPortName
	     | Buf3 DataOutPortName
	     -- Multiplier
	     | Mult DataOutPortName DataOutPortName
             | MultRegWrite
	     -- Stalls
	     | StallReg1
	     | StallReg2
	     | StallALU
	     | StallLS
  deriving (Eq, Show)
