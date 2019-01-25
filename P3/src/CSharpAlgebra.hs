module CSharpAlgebra where

import CSharpLex
import CSharpGram

-- CSharp Algebra
type CSharpAlgebra clas memb stat expr
    = (  Env -> Token -> [memb] -> clas

      ,  ( Env -> Decl                             -> memb
         , Env -> Type -> Token -> [Decl] -> stat  -> memb
         )

      ,  ( Env -> Decl                         -> stat
         , Env -> expr                         -> stat
         , Env -> expr -> stat -> stat         -> stat
         , Env -> expr -> stat                 -> stat
         , Env -> expr                         -> stat
         , Env -> stat -> expr -> stat -> stat -> stat
         , Env -> [stat]                       -> stat                                                                                                                                                                          
         )

      ,  ( Env -> Token                  -> expr
         , Env -> Token                  -> expr
         , Env -> Token -> expr -> expr  -> expr
         , Env -> Token -> [expr]        -> expr
         )
      )


-- CSharp Fold
foldCSharp :: CSharpAlgebra clas memb stat expr -> Class -> clas
foldCSharp (c1, (m1,m2), (s1,s2,s3,s4,s5,s6,s7), (e1,e2,e3,e4)) = fClas empty
    where
        fClas env (Class     c ms)         = c1 env c (map (fMemb env) ms)
        fMemb env (MemberD   d)            = m1 env d
        fMemb env (MemberM   t m ps s)     = m2 env t m ps (fStat env s)
        fStat env (StatDecl  d)            = s1 env d
        fStat env (StatExpr  e)            = s2 env (fExpr env e)
        fStat env (StatIf    e s1 s2)      = s3 env (fExpr env e) (fStat env s1) (fStat env s2)
        fStat env (StatWhile e s1)         = s4 env (fExpr env e) (fStat env s1)
        fStat env (StatReturn e)            = s5 env (fExpr env e)
        fStat env (StatFor   s1 e s2 s3)   = s6 env (fStat env s1) (fExpr env e) (fStat env s2) (fStat env s3)
        fStat env (StatBlock ss)           = s7 env (map (fStat env) ss)
        fExpr env (ExprConst con)          = e1 env con
        fExpr env (ExprVar   var)          = e2 env var
        fExpr env (ExprOper  op e1 e2)     = e3 env op (fExpr env e1) (fExpr env e2)
        fExpr env (ExprMeth  name es)      = e4 env name $ map (fExpr env) es




