module Test.Example1 ( l50 ) where

import Data.Set ( fromList )
import Types ( Reg(..), ALUOp(..), Label(..), Sign(..), BCond(..), LSWidth(..) )
import GPP 
import MicroOp -- ( Oper(..) )
import CFG
import Mips2Mops ( BasicMopBlock )

l50 :: BasicMopBlock
l50 = 
  BB { bbLabel = NamedLabel "$L50", 
       bbInstrs = 
           [Mop 53 Nothing (Store LSW_2 (PV 18 (Just (OrigWrite (Reg 5)))) (PV 52 (Just (OrigWrite (Reg 4)))))
           ,Mop 52 (Just (PV 52 (Just (OrigWrite (Reg 4))))) (ALU AO_ADDU (PV 48 (Just (OrigWrite (Reg 4)))) (PV 51 (Just (OrigWrite (Reg 2)))))
           ,Mop 51 (Just (PV 51 (Just (OrigWrite (Reg 2))))) (Load LSW_2 Unsigned (PV 18 (Just (OrigWrite (Reg 5)))))
           ,Mop 50 Nothing (Store LSW_2 (PV 5 (Just (OrigWrite (Reg 6)))) (PV 49 (Just (OrigWrite (Reg 11)))))
           ,Mop 49 (Just (PV 49 (Just (OrigWrite (Reg 11))))) (ALU AO_SUBU (PV 22 (Just (OrigWrite (Reg 11)))) (PV 48 (Just (OrigWrite (Reg 4)))))
           ,Mop 48 (Just (PV 48 (Just (OrigWrite (Reg 4))))) (ALU AO_SHR (PV 46 (Just (OrigWrite (Reg 4)))) (PV 47 (Just (OrigImm (Int 15)))))
           ,Mop 46 (Just (PV 46 (Just (OrigWrite (Reg 4))))) (ALU AO_ADDU (PV 33 Nothing) (PV 45 Nothing))
           ,Mop 45 (Just (PV 45 Nothing)) (MultMIPS (PV 10 (Just (OrigRead (Reg 25)))) (PV 9 (Just (OrigWrite (Reg 10)))))
           ,Mop 44 Nothing (Store LSW_2 (PV 43 Nothing) (PV 41 (Just (OrigWrite (Reg 2)))))
           ,Mop 43 (Just (PV 43 Nothing)) (ALU AO_ADD (PV 18 (Just (OrigWrite (Reg 5)))) (PV 42 (Just (OrigImm (Int 512)))))
           ,Mop 41 (Just (PV 41 (Just (OrigWrite (Reg 2))))) (ALU AO_ADDU (PV 32 (Just (OrigWrite (Reg 2)))) (PV 40 (Just (OrigWrite (Reg 3)))))
           ,Mop 40 (Just (PV 40 (Just (OrigWrite (Reg 3))))) (Load LSW_2 Unsigned (PV 39 Nothing))
           ,Mop 39 (Just (PV 39 Nothing)) (ALU AO_ADD (PV 18 (Just (OrigWrite (Reg 5)))) (PV 38 (Just (OrigImm (Int 512)))))
           ,Mop 37 Nothing (Store LSW_2 (PV 36 Nothing) (PV 34 (Just (OrigWrite (Reg 7)))))
           ,Mop 36 (Just (PV 36 Nothing)) (ALU AO_ADD (PV 5 (Just (OrigWrite (Reg 6)))) (PV 35 (Just (OrigImm (Int 512)))))
           ,Mop 34 (Just (PV 34 (Just (OrigWrite (Reg 7))))) (ALU AO_SUBU (PV 21 (Just (OrigWrite (Reg 7)))) (PV 32 (Just (OrigWrite (Reg 2)))))
           ,Mop 33 (Just (PV 33 Nothing)) (MultMIPS (PV 28 (Just (OrigRead (Reg 24)))) (PV 6 (Just (OrigWrite (Reg 4)))))
           ,Mop 32 (Just (PV 32 (Just (OrigWrite (Reg 2))))) (ALU AO_SHR (PV 30 (Just (OrigWrite (Reg 2)))) (PV 31 (Just (OrigImm (Int 15)))))
           ,Mop 30 (Just (PV 30 (Just (OrigWrite (Reg 2))))) (ALU AO_SUBU (PV 29 Nothing) (PV 11 Nothing))
           ,Mop 29 (Just (PV 29 Nothing)) (MultMIPS (PV 28 (Just (OrigRead (Reg 24)))) (PV 9 (Just (OrigWrite (Reg 10)))))
           ,Mop 27 (Just (PV 27 (Just (OrigWrite (Reg 9))))) (ALU AO_SLT (PV 24 (Just (OrigWrite (Reg 9)))) (PV 26 (Just (OrigRead (Reg 16)))))
           ,Mop 25 (Just (PV 25 (Just (OrigWrite (Reg 8))))) (ALU AO_ADDU (PV 2 (Just (OrigRead (Reg 8)))) (PV 13 (Just (OrigRead (Reg 15)))))
           ,Mop 24 (Just (PV 24 (Just (OrigWrite (Reg 9))))) (ALU AO_SUBU (PV 14 (Just (OrigWrite (Reg 14)))) (PV 13 (Just (OrigRead (Reg 15)))))
           ,Mop 23 (Just (PV 23 (Just (OrigWrite (Reg 13))))) (ALU AO_ADDU (PV 13 (Just (OrigRead (Reg 15)))) (PV 16 (Just (OrigRead (Reg 13)))))
           ,Mop 22 (Just (PV 22 (Just (OrigWrite (Reg 11))))) (Load LSW_2 Unsigned (PV 18 (Just (OrigWrite (Reg 5)))))
           ,Mop 21 (Just (PV 21 (Just (OrigWrite (Reg 7))))) (Load LSW_2 Unsigned (PV 20 Nothing))
           ,Mop 20 (Just (PV 20 Nothing)) (ALU AO_ADD (PV 18 (Just (OrigWrite (Reg 5)))) (PV 19 (Just (OrigImm (Int 512)))))
           ,Mop 18 (Just (PV 18 (Just (OrigWrite (Reg 5))))) (ALU AO_ADDU (PV 17 (Just (OrigWrite (Reg 5)))) (PV 4 (Just (OrigRead (Reg 17)))))
           ,Mop 17 (Just (PV 17 (Just (OrigWrite (Reg 5))))) (ALU AO_SLL (PV 16 (Just (OrigRead (Reg 13)))) (PV 15 (Just (OrigImm (Int 1)))))
           ,Mop 14 (Just (PV 14 (Just (OrigWrite (Reg 14))))) (ALU AO_ADDU (PV 12 (Just (OrigRead (Reg 14)))) (PV 13 (Just (OrigRead (Reg 15)))))
           ,Mop 11 (Just (PV 11 Nothing)) (MultMIPS (PV 10 (Just (OrigRead (Reg 25)))) (PV 6 (Just (OrigWrite (Reg 4)))))
           ,Mop 9 (Just (PV 9 (Just (OrigWrite (Reg 10))))) (Load LSW_2 Signed (PV 8 Nothing))
           ,Mop 8 (Just (PV 8 Nothing)) (ALU AO_ADD (PV 5 (Just (OrigWrite (Reg 6)))) (PV 7 (Just (OrigImm (Int 512)))))
           ,Mop 6 (Just (PV 6 (Just (OrigWrite (Reg 4))))) (Load LSW_2 Signed (PV 5 (Just (OrigWrite (Reg 6)))))
           ,Mop 5 (Just (PV 5 (Just (OrigWrite (Reg 6))))) (ALU AO_ADDU (PV 3 (Just (OrigWrite (Reg 6)))) (PV 4 (Just (OrigRead (Reg 17)))))
           ,Mop 3 (Just (PV 3 (Just (OrigWrite (Reg 6))))) (ALU AO_SLL (PV 2 (Just (OrigRead (Reg 8)))) (PV 1 (Just (OrigImm (Int 1)))))]
     , bbExit = ExCBranch (Just (NamedLabel "$L50")) (NamedLabel "$L48") (Mop 55 Nothing (Branch NZ (PV 27 (Just (OrigWrite (Reg 9)))) (PV 54 (Just (OrigImm (Addr "$L50"))))))
     , bbLiveOut = fromList [Reg 8,Reg 13,Reg 14,Reg 15,Reg 16,Reg 17,Reg 18,Reg 19,Reg 20,Reg 21,Reg 22,Reg 23,Reg 24,Reg 25,Reg 29,Reg 30]
     , bbInfo = fromList [PV 14 (Just (OrigWrite (Reg 14))),PV 23 (Just (OrigWrite (Reg 13))),PV 25 (Just (OrigWrite (Reg 8)))]}
