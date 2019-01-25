module CSharpAlgebra where

import CSharpLex
import CSharpGram
import Data.Map as M hiding (foldr, map)

-- CSharp Algebra
type CSharpAlgebra clas memb stat expr
    = (  Env -> Token -> [memb] -> clas

      ,  ( Env -> Decl                             -> memb
         , Env -> Type -> Token -> [Decl] -> stat  -> memb
         )

      ,  ( Env -> Decl                         -> (stat,Env)
         , Env -> expr                         -> (stat,Env)
         , Env -> expr -> stat -> stat         -> (stat,Env)
         , Env -> expr -> stat                 -> (stat,Env)
         , Env -> expr                         -> (stat,Env)
         , Env -> stat -> expr -> stat -> stat -> (stat,Env)
         , Env -> [stat]                       -> (stat,Env)                                                                                                                                                                          
         )

      ,  ( Env -> Token                  -> expr
         , Env -> Token                  -> expr
         , Env -> Token -> expr -> expr  -> expr
         , Env -> Token -> [expr]        -> expr
         )
      )


-- CSharp Fold
foldCSharp :: CSharpAlgebra clas memb stat expr -> Class -> clas
foldCSharp (c1, (m1,m2), (s1,s2,s3,s4,s5,s6,s7), (e1,e2,e3,e4)) = fClas M.empty
    where
        fClas env (Class     c ms)         = c1 env c (map (fMemb env) ms)
        fMemb env (MemberD   d)            = m1 env d
        fMemb env (MemberM   t m ps s)     = m2 (snd (fStat M.empty s)) t m ps (fst (fStat M.empty s))
        fStat env (StatDecl  d)            = s1 env d
        fStat env (StatExpr  e)            = s2 env (fExpr env e)
        fStat env (StatIf    e s1 s2)      = s3 (snd(fStat (snd(fStat env s1)) s2)) (fExpr env e) (fst (fStat env s1)) (fst (fStat (snd(fStat env s1)) s2))
        fStat env (StatWhile e s1)         = s4 (snd (fStat env s1)) (fExpr env e) (fst (fStat env s1))
        fStat env (StatReturn e)           = s5 env (fExpr env e)
        fStat env (StatFor   s1 e s2 s3)   = s6 (snd (fStat (snd (fStat (snd(fStat env s1)) s2)) s3)) (fst(fStat env s1)) (fExpr (snd(fStat env s1)) e)  (fst (fStat (snd(fStat env s1)) s2)) (fst (fStat (snd (fStat (snd(fStat env s1)) s2)) s3))
        fStat env (StatBlock ss)           = s7 (snd (helper env ss)) (fst(helper env ss))
        fExpr env (ExprConst con)          = e1 env con
        fExpr env (ExprVar   var)          = e2 env var
        fExpr env (ExprOper  op e1 e2)     = e3 env op (fExpr env e1) (fExpr env e2)
        fExpr env (ExprMeth  name es)      = e4 env name $ map (fExpr env) es
        helper e [] = ([],e)
        helper e (x:xs) = (code e x : fst (helper (envi e x) xs) , M.union (envi e x) (snd (helper (envi e x) xs)))
        code e x = fst (fStat e x)
        envi e x = snd (fStat e x)




