{-# OPTIONS -Wall #-}
-- | Parsers for MIPS assembly files (GNU format).
module Mips 
  ( MipsDirective(..)
  , mipsFile
  , parseMipsFile
  , simplify
  ) where

import Text.ParserCombinators.Parsec hiding ( label, token )
import Control.Applicative  ( Applicative, pure, (<$>), (<*>), (<*), (*>), (<$) )
import Control.Monad ( ap )
import qualified Data.Map as M
import Data.Map ( Map )
import Data.Char ( chr )

import Types ( Instr(..), Reg(..), ALUOp(..), LSWidth(..), 
               BCond(..), Sign(..) )
import GPP ( ASMInstr, Const(..) )

--import Debug.Trace

------------------------------------------------------------------------------
-- Applicative functors are a structuring mechanism more lightweight than
-- monads and are particularly nice for parsers.  Every monad automatically is
-- an applicative functor, and therefore supports the following nice
-- operators:
--
-- > f <$> p = do x <- p
-- >              return (f x)
--
-- > p <*> q = do f <- p
-- >              x <- q
-- >              return (f x)
--
-- > p <* q = do x <- p
-- >             q
-- >             return x
--
-- > p *> q = p >> q
-- 
-- Think of the ">" and "<" point to the parser whose result is returned. 
--
-- > a <$ q = do q; return a
--
-- For example, the following parses an expression like "123+45" and
-- returns 168.
--
-- > plusExpr = (+) <$> (read <$> many1 digit)
-- >                <*  char '+'  
-- >                <*> (read <$> many1 digit)

instance Applicative (GenParser c s) where
    pure = return
    (<*>) = ap


type MipsP a = CharParser MipsState a

type MipsState = ()

-- | A MIPS assembly file consists of only three kinds of directives.
data MipsDirective 
    = MDLabel String
    | Pragma String [String]
    | Instr ASMInstr
      deriving (Eq, Show)

------------------------------------------------------------------------------
-- Tokens

-- | Transform a parser into a token parser.  A token parser just skips all
--  remaining whitespace (including comments.)
token :: MipsP a -> MipsP a
token p = p <* skipWhitespace

-- | Like 'token' but also gives the token a name, that will be used in error
--   messages.
token' :: String -> MipsP a -> MipsP a
token' name p = token p <?> name

-- | Skips any whitespace except newlines.  Use 'eol' to parse those.
skipWhitespace :: MipsP ()
skipWhitespace = --return ()
    skipMany (oneOf " \t") >>
    optional (char '#' >> skipMany (satisfy (/='\n')) <?> "comment")


ident :: MipsP String
ident = token' "identifier" $
        (:) <$> (letter <|> special) <*> many (letter <|> digit <|> special <|> char '-')
  where special = oneOf "$@_."

-- number :: (Read a, Num a) => MipsP a
-- number = token' "number" numberStr

numberStr :: MipsP String
numberStr = do 
  sign <- option id (char '-' >> return ('-':))
  sign <$>
    (((:) <$> char '0' 
          <*> option [] ((:) <$> char 'x' <*> many1 hexDigit
                         <|> (option "" $ many1 octDigit)))
     <|> ((:) <$> oneOf "123456789" <*> many digit))
--     sign <- option '+' (char '-')
--     (sign:) <$> 
--              (((:) <$> char '0' 
--                    <*> option [] ((:) <$> char 'x' <*> many1 hexDigit
--                                   <|> ('o':) <$> (option "0" $ many1 octDigit)))
--              <|> ((:) <$> oneOf "123456789" <*> many digit))

-- | A string literal.  Interprets some escape sequences. [XXX: make it all]
stringLit :: MipsP String
stringLit = token' "string" $
    do char '"'
       content <- many stringChar 
       char '"'
       return $ '"' : (concat content ++ "\"")
  where 
    stringChar = (do c <- noneOf ['"']
                     return [c])
                 <|> (char '\\' >> char '"' >> return "\\\"")
--     stringChar = char '\\' *> (mkSpecial <$> oneOf "ntr\"" 
--                            <|> numChar <$> many1 digit) 
--                  <|> satisfy (/='"')
--     mkSpecial 'n' = '\n'
--     mkSpecial 't' = '\t'
--     mkSpecial 'r' = '\r'
--     --mkSpecial '"' = '"'
--     mkSpecial '0' = '\0'
--     mkSpecial c = c
--     numChar "0" = chr 0
--     numChar ('0':rst) = chr (read $ '0':'o':rst)
--     numChar rst = chr (read rst)

-- | Parses a newline.  If a newline is followed by empty lines, those are
--   ignored (ie, only treated as one.)
eol :: MipsP ()
eol = skipMany1 (satisfy (=='\n') >> skipWhitespace)

-- | A single-character token.
oper :: Char -> MipsP Char
oper c = token $ char c

-- | Parses a label, i.e., an identifier ending with ":".  Returns the label
--   name, without the ":".
labelDecl :: MipsP String
labelDecl = 
    token' "label declaration" $ 
    try $ -- if the ':' is missing, then it's an instruction name
    ((:) <$> (letter <|> special) 
         <*> many (letter <|> digit <|> special <|> char '.'))
         <* char ':'
  where special = oneOf "$@_"

------------------------------------------------------------------------------

-- | The parser that parses a complete MIPS assembly file.
mipsFile :: MipsP [MipsDirective]
mipsFile = skipWhitespace >> sepEndBy mipsDirective eol <* eof

mipsDirective :: MipsP MipsDirective
mipsDirective = 
    mipsLabel <|> mipsPragma <|> mipsInstr

mipsPragma :: MipsP MipsDirective
mipsPragma = (Pragma <$> pragmaName <*> pragmaArgs) <?> "pragma"

pragmaName :: MipsP String
pragmaName = token' "pragma name" $ (:) <$> char '.' *> ident

pragmaArgs :: MipsP [String]
pragmaArgs = arg `sepBy` (optional (oper ','))
  where arg = ident <|> stringLit <|> token numberStr

mipsLabel :: MipsP MipsDirective
mipsLabel = MDLabel <$> labelDecl

-- for our purposes a label consists of at least two alphanumericals this is
-- so that registers cannot be confused as labels (This would make parsing
-- depend on the order in which alternatives are being tried--we'd first have
-- to try if we can parse it as a register and then fall back to trying to
-- read it as a label.  )
label :: MipsP String
label = do f <- oneOf "$_." <|> letter
           f2 <- oneOf "$_." <|> letter
           fs <- many (oneOf "_$." <|> alphaNum)
           return $ f : f2 : fs

mipsInstr :: MipsP MipsDirective
mipsInstr = Instr <$> instr

maxRegs :: Int
maxRegs = 32

-- | Parses a MIPS register.  Almost all registers have two names, the actual
-- register number as well as a name according to the MIPS calling convention.
--
-- > $0       ($zero)
-- > $1       $at      ?? 
-- > $2,$3    $v0,$v1  function result
-- > $4-$7    $a0-$a3  first four function arguments 
-- > $8-$15   $t0-$t7  temporary values not preserved by callee
-- > $24,$25  $t8,$t9  more temporary values
-- > $16-$23  $s0-$s7  saved values (preserved by callee)
-- > $26,$27  $k0,$k1  used by OS kernel
-- > $28      $gp      global pointer
-- > $29      $sp      stack pointer
-- > $30      $fp      frame pointer
-- > $31      $ra      return address
reg :: MipsP Reg
reg = do char '$'
         choice [ Reg <$> numWithin (0, maxRegs-1)
                , try $ do char 'a'; (Reg . (4+)) <$> numWithin (0,3)
                ,       do char 'k'; (Reg . (26+)) <$> numWithin (0,1)
                , try $ do char 's'; (Reg . (16+)) <$> numWithin (0,7)
                , try $ do char 't'; (Reg . (8+)) <$> numWithin (0,7)
                ,       do char 't'; (Reg . (16+)) <$> numWithin (8,9)
                ,       do char 'v'; (Reg . (2+)) <$> numWithin (0,1)
                , string "zero" >> return (Reg 0)
                , string "at" >> return (Reg 1)
                , string "gp" >> return (Reg 28)
                , string "sp" >> return (Reg 29)
                , string "fp" >> return (Reg 30)
                , string "ra" >> return (Reg 31)
                ]
  where
    numWithin range = num >>= within range
    num = read <$> many1 digit
    within (from,to) x
      | from <= x && x <= to = return x
      | otherwise = fail $ "Number between " ++ show from ++ " and " 
                           ++ show to ++ " required."

integer :: MipsP Int
integer =
    (try $ 
     do char '0'; char 'x'
        ds <- many1 (oneOf "0123456789abcdefABCDEF")
        return $ read ('0':'x':ds))
    <|>
    (do s <- option 1 (char '-' >> return (-1))
        ds <- many1 digit
        return $ s * read ds)

imm :: MipsP Const
imm = (Addr <$> label) 
  <|> (Int <$> integer)
  <|> (do char '%'; ConstFun <$> many1 letter 
                             <*> parens (imm `sepBy` comma)) 

comma :: MipsP ()
comma = () <$ oper ','

parens :: MipsP b -> MipsP b
parens p = oper '(' *> p <* oper ')' 
{-
args :: MipsP Args
args = choice $ map try $
    [ RRR <$> reg <*> (comma >> reg) <*> (comma >> reg)
    , RRI <$> reg <*> (comma >> reg) <*> (comma >> imm)
    , do (dst,offs,base) <- (,,) <$> reg <*> (comma >> integer) <*> parens reg
         return $ RRI dst base (Int offs)
    , RR <$> reg <*> (comma >> reg)
    , RI <$> reg <*> (comma >> imm)
    , R <$> reg
    , I <$> imm
    ]
-}
-- Various common combinations of register and immediate arguments
instRRR :: (Reg -> Reg -> Reg -> ASMInstr) -> MipsP ASMInstr
instRRR c = c <$> reg <*> (comma >> reg) <*> (comma >> reg)

instRRI :: (Reg -> Reg -> Const -> a) -> MipsP a
instRRI c = c <$> reg <*> (comma >> reg) <*> (comma >> imm)

instR0I :: (Reg -> Reg -> Const -> ASMInstr) -> MipsP ASMInstr
instR0I c = c <$> reg <*> (return (Reg 0)) <*> (comma >> imm) 

instRI :: (Reg -> Const -> ASMInstr) -> MipsP ASMInstr
instRI c  = c <$> reg <*> (comma >> imm)

instRR :: (Reg -> Reg -> ASMInstr) -> MipsP ASMInstr
instRR c  = c <$> reg <*> (comma >> reg)

instR :: (Reg -> ASMInstr) -> MipsP ASMInstr
instR c = c <$> reg

instI :: (Const -> ASMInstr) -> MipsP ASMInstr
instI c = c <$> imm

instA :: (Reg -> Reg -> Const -> ASMInstr) -> MipsP ASMInstr
instA c = do 
    dst <- reg <* comma
    (offs, base) <- ((,) <$> (integer >>= return . Int) 
                         <*> option (Reg 0) (parens reg))
                 <|> ((,) <$> imm <*> return (Reg 0))  
    return $ c dst base offs

second :: (b -> c) -> (a,b) -> (a,c)
second f (a,b) = (a, f b)

instrs :: Map String (MipsP ASMInstr)
instrs = M.fromListWith (flip (<|>)) $ map (second try) $ 
    [ ("add",   instRRR (ALUr AO_ADD))
    , ("add",   instRRI (ALUi AO_ADD))
    , ("addu",  instRRR (ALUr AO_ADDU)) 
    , ("addu",  instRRI (ALUi AO_ADDU))
    , ("addiu", instRRR (ALUr AO_ADDU))
    , ("addiu", instRRI (ALUi AO_ADDU))
    , ("and",   instRRR (ALUr AO_AND))
    , ("andi",  instRRI (ALUi AO_AND))
    , ("b",     instI   J)
    , ("beq",   instRRI (BrALUr NZ AO_SEQ))
    , ("beqz",  instRI  (Br Z))
    , ("bgez",  instRI  (BrALUr NZ AO_SLE (Reg 0)))
    , ("bgtz",  instRI  (BrALUr NZ AO_SLT (Reg 0)))
    , ("blez",  instR0I (BrALUr NZ AO_SLE))
    , ("bltz",  instR0I (BrALUr NZ AO_SLT))
    , ("bnez",  instRI  (Br NZ))
    , ("bne",   instRRI (BrALUr NZ AO_SNE))
    , ("j",     instI   J)
    , ("j",     instR   JR)
    , ("jr",    instR   JR)
    , ("jal",   instI   (JAL (Reg 31)))
    , ("jal",   instR   (JALR (Reg 31)))
    , ("la",    instR0I (ALUi AO_ADD))
    , ("li",    instR0I (ALUi AO_ADD))
    , ("lb",    instA   (Load LSW_1 Signed))
    , ("lbu",   instA   (Load LSW_1 Unsigned))
    , ("lh",    instA   (Load LSW_2 Signed))
    , ("lhu",   instA   (Load LSW_2 Unsigned))
    , ("lui",   instRI  (\d c -> ALUi AO_ADD d (Reg 0) (ConstFun "toHi" [c])))
    , ("lw",    instA   (Load LSW_4 Signed))
    , ("mflo",  instR   MFLo)
    , ("move",  instRR  (\d s -> ALUr AO_ADD d s (Reg 0)))
    , ("mul",   instRRR Mult3)
    , ("mult",  instRR  MultMIPS)
    , ("neg",   instRR  (\d s -> ALUr AO_SUB d (Reg 0) s))
    , ("nop",   return  (UserNOP ""))
    , ("nor",   instRRR (ALUr AO_NOR))
    , ("or",    instRRR (ALUr AO_OR))
    , ("ori",   instRRI (ALUi AO_OR))
    , ("sb",    instA   (Store LSW_1))
    , ("sh",    instA   (Store LSW_2))
    , ("seq",   instRRR (ALUr AO_SEQ))
    , ("seq",   instRRI (ALUi AO_SEQ))
    , ("sll",   instRRR (ALUr AO_SLL))
    , ("sll",   instRRI (ALUi AO_SLL))
    , ("sne",   instRRI (ALUi AO_SNE))
    , ("sne",   instRRR (ALUr AO_SNE))
    , ("sra",   instRRI (ALUi AO_SHR))
    , ("sra",   instRRR (ALUr AO_SHR))
    , ("srl",   instRRI (ALUi AO_SRL))
    , ("srl",   instRRR (ALUr AO_SRL))
    , ("slt",   instRRI (ALUi AO_SLT))
    , ("slt",   instRRR (ALUr AO_SLT))
    , ("sltu",  instRRI (ALUi AO_SLT))
    , ("sltu",  instRRR (ALUr AO_SLT))
    , ("sub",   instRRI (ALUi AO_SUB))
    , ("sub",   instRRR (ALUr AO_SUB))
    , ("subu",  instRRI (ALUi AO_SUBU))
    , ("subu",  instRRR (ALUr AO_SUBU))
    , ("sw",    instA   (Store LSW_4))
    , ("xor",   instRRR (ALUr AO_XOR))
    , ("xor",   instRRI (ALUi AO_XOR))
    , ("xori",   instRRI (ALUi AO_XOR))
    ]

instr :: MipsP ASMInstr
instr = do iname <- token (many1 letter)
           --trace (show iname) (return ())
           i <- M.findWithDefault (fail $ "unknown instruction: " ++ iname)
                                    iname instrs
           skipWhitespace
           -- optional sp >> newline
           return i

{-
testArgs = all  p tsts
  where
    p (t,r) = case parse args "" t of
                Right r' -> r == r'
                Left _ -> False
    tsts = [ ("$4 , $5 , -5", RRI (Reg 4) (Reg 5) (Int (-5)))
           , ("$4 , $5 , foo", RRI (Reg 4) (Reg 5) (Addr "foo"))
           , ("$4 , 42($3)", RRI (Reg 4) (Reg 3) (Int 42))
           
           ]
-}

------------------------------------------------------------------------------

-- | Performs some simplifications on the MIPS assembly.
--   [XXX: this should probably be done automatically.]
simplify :: [MipsDirective] -> [MipsDirective]
simplify = map simpl
  where
    simpl (Instr i) = Instr $ case i of
      BrALUr z AO_SNE (Reg r1) (Reg r2) dest
          | r1 == 0 -> Br z (Reg r2) dest
          | r2 == 0 -> Br z (Reg r1) dest
      x -> x
    simpl d = d

------------------------------------------------------------------------------
-- | Open and parse a MIPS assembly file.
parseMipsFile :: FilePath -> IO (Either ParseError [MipsDirective])
parseMipsFile f = 
  parse mipsFile f <$> readFile f

{-
main = do 
          f <- parseMipsFile "../test/primes.s"
          case f of
            Left e -> print e
            Right ds -> mapM_ print ds
-}     



{-
tst = mapM_ testit tests
  where
    testit (p,inp,exp) = 
        case (parse p "testing" inp, exp) of
          (Right a, Just b) 
            | a == b -> return ()
          (Left _, Nothing) -> return ()
          (res,_) -> reportError inp res exp
    reportError inp res exp = do
        putStrLn $ "Failure while parsing: " ++ show inp
        putStrLn $ "Expected: " ++ maybe "Failure" show exp
        putStrLn $ "Got:      " ++ either show show res

--tests :: [forall a. (MipsP a, String, Maybe a)]
tests = [ (pragmaName, ".foo", Just "foo")
        , (pragmaName, "bar", Nothing)
--        , (number, "0", Just 0)
        ]
-}
--parseTest (skipWhitespace *> mipsPragma) "  .file  1 \"foo.c\"\n"
-- => Pragma "file" ["1","foo.c"]
