module CSharpCode where

import Prelude as P hiding (LT, GT, EQ)
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
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatFor, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fExprMeth)
    )

-- Class, translated by calling the label "main" and halting execution. Followed by the code for all the methods   
fClas :: Env -> Token -> [Code] -> Code
fClas _ c ms = [Bsr "main", HALT] ++ concat ms

-- Method declaration, provides information to the compiler, but does not generate code
fMembDecl ::Env ->  Decl -> Code
fMembDecl _ d = []

-- Method definition, starts with a label and we insert the code for the body of the method and a return instruction
fMembMeth :: Env -> Type -> Token -> [Decl] -> Code -> Code
fMembMeth _ t (LowerId x) ps s = [LABEL x] ++ s ++ [RET]

-- Statement declaration, does not generate any code
fStatDecl :: Env -> Decl -> (Code,Env)
fStatDecl e (Decl _  (LowerId name)) = ([], insert name 0 env)
                      where env = M.map (\x -> x - 1) e

-- statement is an expression, has a result but statements do not compute results so it is discarded (adjust stack pointer -1)
fStatExpr :: Env -> (ValueOrAddress -> Code) -> (Code,Env)
fStatExpr env e = (e Value ++ [pop],env)

-- If statements, join different blocks and add jumps around them using codeSize
fStatIf :: Env -> (ValueOrAddress -> Code) -> Code -> Code -> (Code,Env)
fStatIf env e s1 s2 = (c ++ [BRF (n1 + 2)] ++ s1 ++ [BRA n2] ++ s2, env)
    where
        c        = e Value
        (n1, n2) = (codeSize s1, codeSize s2)

-- For statements: We desugar to a While, with an extra statement before and during the loop
fStatFor :: Env -> Code -> (ValueOrAddress -> Code) -> Code -> Code -> (Code,Env)
fStatFor env s1 e s2 s3 = (s1 ++ fst (fStatWhile env e (s2 ++ s3)), snd (fStatWhile env e (s2 ++ s3)))

-- While statements, join different blocks and add jumps around them using codeSize
fStatWhile :: Env -> (ValueOrAddress -> Code) -> Code -> (Code,Env)
fStatWhile env e s1 = ([BRA n] ++ s1 ++ c ++ [BRT (-(n + k + 2))],env)
    where
        c = e Value
        (n, k) = (codeSize s1, codeSize c)

-- Return statements, computes result, Then return to caller. Set a boolean value in a register, that tells whether the method returned something
fStatReturn :: Env -> (ValueOrAddress -> Code) -> (Code,Env)
fStatReturn env e = (e Value ++ [STR R3] ++ [LDC 1] ++ [STR R4] ++ [RET], env)

-- Whole block, flattens a list of Code by concattenating
fStatBlock :: Env -> [Code] -> (Code,Env)
fStatBlock e c = (concat c, e)

-- Expression constant, push the constant onto the stack
fExprCon :: Env -> Token -> ValueOrAddress -> Code
fExprCon _ (ConstInt n) va = [LDC n]

-- Expression variable, loads a local variable or its address (depending on argument va)
-- At the moment doesn't calculate location of local variables yet, so uses default location 37
fExprVar :: Env -> Token -> ValueOrAddress -> Code
fExprVar e (LowerId x) va = let loc = e ! x in case va of
                                          Value    ->  [LDL  loc]
                                          Address  ->  [LDLA loc]

-- Assignment operator, computes the value of the right operand and duplicates it (LDS 0) and assigns it (STA 0)
fExprOp :: Env -> Token -> (ValueOrAddress -> Code) -> (ValueOrAddress -> Code) -> ValueOrAddress -> Code
fExprOp e (Operator "=") e1 e2 _ = e2 Value ++ [LDS 0] ++ e1 Address ++ [STA 0]
fExprOp e (Operator op)  e1 e2 _ = case M.lookup op logicCodes of
                                 Nothing  ->  val1 ++ val2 ++ [opCodes ! op]
                                 Just XOR -> val1 ++ val2 ++ [opCodes ! op]
                                 Just AND -> val1 ++ [BRF (len2 + 2)] ++ val2 ++ [AND]
                                 Just OR  -> val1 ++ [BRT (len2 + 2)] ++ val2 ++ [OR]
                                 where val1 = e1 Value
                                       val2 = e2 Value
                                       len2 = codeSize val2
                                   
-- Method call, Check if something is print, if so, treat it differently. Then check whether the method has returned something                                        
fExprMeth :: Env -> Token -> [ValueOrAddress -> Code] -> ValueOrAddress -> Code
fExprMeth _ (LowerId "print") es _ = concatMap (\x -> x Value ++ [TRAP 0]) es
fExprMeth _ (LowerId id) [] _ = [Bsr id] ++ checkForReturn
fExprMeth _ (LowerId id) es _ = concat(P.map (\x -> x Value) es) ++ [Bsr id] ++ checkForReturn
 

checkForReturn :: Code
checkForReturn = [LDR R4] ++ [BRF 7] ++ [LDR R3] ++ [LDC 0] ++ [STR R4]

opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE)]
    
logicCodes :: Map String Instr
logicCodes = fromList[("&&", AND), ("||", OR), ("^", XOR)]