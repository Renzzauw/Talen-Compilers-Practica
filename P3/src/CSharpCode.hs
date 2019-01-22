module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import Data.Map as M
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM


data ValueOrAddress = Value | Address
    deriving Show

-- Code Algebra
codeAlgebra :: CSharpAlgebra Code Code Code (ValueOrAddress -> Code)
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
    , (fExprCon, fExprVar, fExprOp)
    )

-- Class, translated by calling the label "main" and halting execution. Followed by the code for all the methods   
fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

-- Method declaration, provides information to the compiler, but does not generate code
fMembDecl :: Decl -> Code
fMembDecl d = []

-- Method definition, starts with a label and we insert the code for the body of the method and a return instruction
fMembMeth :: Type -> Token -> [Decl] -> Code -> Code
fMembMeth t (LowerId "print") ps s = s ++ [TRAP 0]
fMembMeth t (LowerId x) ps s = [LABEL x] ++ s ++ [RET]

-- Statement declaration, does not generate any code
fStatDecl :: Decl -> Code
fStatDecl d = []

-- statement is an expression, has a result but statements do not compute results so it is discarded (adjust stack pointer -1)
fStatExpr :: (ValueOrAddress -> Code) -> Code
fStatExpr e = e Value ++ [pop]

-- If statements, join different blocks and add jumps around them using codeSize
fStatIf :: (ValueOrAddress -> Code) -> Code -> Code -> Code
fStatIf e s1 s2 = c ++ [BRF (n1 + 2)] ++ s1 ++ [BRA n2] ++ s2
    where
        c        = e Value
        (n1, n2) = (codeSize s1, codeSize s2)

-- While statements, join different blocks and add jumps around them using codeSize
fStatWhile :: (ValueOrAddress -> Code) -> Code -> Code
fStatWhile e s1 = [BRA n] ++ s1 ++ c ++ [BRT (-(n + k + 2))]
    where
        c = e Value
        (n, k) = (codeSize s1, codeSize c)

-- Return statements, computes result, but then discards it (pop). Then return to caller     
fStatReturn :: (ValueOrAddress -> Code) -> Code
fStatReturn e = e Value ++ [pop] ++ [RET]

-- Whole block, flattens a list of Code by concattenating
fStatBlock :: [Code] -> Code
fStatBlock = concat

-- Expression constant, push the constant onto the stack
fExprCon :: Token -> ValueOrAddress -> Code
fExprCon (ConstInt n) va = [LDC n]

-- Expression variable, loads a local variable or its address (depending on argument va)
-- At the moment doesn't calculate location of local variables yet, so uses default location 37
fExprVar :: Token -> ValueOrAddress -> Code
fExprVar (LowerId x) va = let loc = 37 in case va of
                                              Value    ->  [LDL  loc]
                                              Address  ->  [LDLA loc]

-- Assignment operator, computes the value of the right operand and duplicates it (LDS 0) and assigns it (STA 0)
fExprOp :: Token -> (ValueOrAddress -> Code) -> (ValueOrAddress -> Code) -> ValueOrAddress -> Code
fExprOp (Operator "=") e1 e2 _ = e2 Value ++ [LDS 0] ++ e1 Address ++ [STA 0]
fExprOp (Operator op)  e1 e2 _ = case M.lookup op logicCodes of
                                 Nothing  ->  val1 ++ val2 ++ [opCodes ! op]
                                 Just XOR -> val1 ++ val2 ++ [opCodes ! op]
                                 Just AND -> val1 ++ checkTrue ++ [BRF (len2 + 2)] ++ val2 ++ [AND]
                                 Just OR  -> val1 ++ checkTrue ++ [BRT (len2 + 2)] ++ val2 ++ [OR]
                                 where val1 = e1 Value
                                       val2 = e2 Value
                                       checkTrue = [LDC 1] ++ [EQ]
                                       len2 = codeSize val2
                                     
opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE)]
    
logicCodes :: Map String Instr
logicCodes = fromList[("&&", AND), ("||", OR), ("^", XOR)]