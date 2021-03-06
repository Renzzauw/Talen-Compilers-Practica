module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex
import Data.Map as M hiding (foldr)
import Prelude hiding ((<*),(*>),(<$))

data Class = Class Token [Member]
    deriving Show

data Member = MemberD Decl
            | MemberM Type Token [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatFor    Stat Expr Stat Stat
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConst  Token
          | ExprVar    Token
          | ExprOper   Token Expr Expr
          | ExprMeth   Token [Expr]
          deriving Show

-- Parser for *, / and % operators
sOperator1 :: Parser Token Token
sOperator1 = satisfy isOperator
    where isOperator (Operator "*") = True
          isOperator (Operator "/") = True
          isOperator (Operator "%") = True
          isOperator _              = False  

          
-- Parser for + and - operators
sOperator2 :: Parser Token Token
sOperator2 = satisfy isOperator
    where isOperator (Operator "+") = True
          isOperator (Operator "-") = True
          isOperator _              = False  

-- Parser for logical operators
sOperator3 :: Parser Token Token
sOperator3 = satisfy isOperator
    where isOperator (Operator "&&") = True
          isOperator (Operator "||") = True
          isOperator (Operator "^") = True
          isOperator (Operator "==") = True
          isOperator (Operator "<=") = True
          isOperator (Operator "<") = True
          isOperator (Operator ">") = True
          isOperator (Operator ">=") = True
          isOperator (Operator "!=") = True
          isOperator _              = False  
          
data Decl = Decl Type Token
    deriving Show

data Type = TypeVoid
          | TypePrim  Token
          | TypeObj   Token
          | TypeArray Type
          deriving (Eq,Show)


parenthesised p = pack (symbol POpen) p (symbol PClose)
bracketed     p = pack (symbol SOpen) p (symbol SClose)
braced        p = pack (symbol COpen) p (symbol CClose)

pExprSimple :: Parser Token Expr
pExprSimple =  ExprConst <$> sConst
           <|> ExprMeth  <$> sLowerId <*> parenthesised (option (listOf pExpr (satisfy (== Comma))) [])
           <|> ExprVar   <$> sLowerId
           <|> parenthesised pExpr


pExpr :: Parser Token Expr
pExpr = flip ExprOper <$> lower <*> satisfy (==(Operator "=")) <*> pExpr
        <|> pExpr3
        <|> pExprSimple
        where lower = parenthesised pExpr <|> pExpr3

        
pExpr3 :: Parser Token Expr
pExpr3 = flip ExprOper <$> lower <*> sOperator3 <*> pExpr3
        <|> pExpr2
        <|> pExprSimple
        where lower = parenthesised pExpr <|> pExpr2

        
pExpr2 :: Parser Token Expr
pExpr2 = flip ExprOper <$> lower <*> sOperator2 <*> pExpr2
        <|> pExpr1
        <|> pExprSimple
        where lower = parenthesised pExpr <|> pExpr1

pExpr1 :: Parser Token Expr
pExpr1 = flip ExprOper <$> pExprSimple <*> sOperator1 <*> pExpr1
        <|> pExprSimple



-- Member parser
pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

-- Declaration parser
pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

-- Declaration type parser         
pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr <* sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr     <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr     <*> pStat                   -- Actual while loop
     <|> StatFor    <$ symbol KeyFor    <*> (satisfy (== POpen) *> pStat) <*> (pExpr <* sSemi) <*> (exprNoSemi <* satisfy (== PClose)) <*> pStat                   -- for loop desugered to while loop
     <|> StatReturn <$ symbol KeyReturn <*> pExpr                   <*  sSemi
     <|> pBlock
     where optionalElse = option ((\_ x -> x) <$> symbol KeyElse <*> pStat) (StatBlock [])
           exprNoSemi = StatExpr <$> pExpr

-- Block parser
pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

-- Method parser
pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> (const TypeVoid <$> symbol KeyVoid)
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

-- Type0 parser      
pType0 :: Parser Token Type
pType0 =  TypePrim <$> sStdType
      <|> TypeObj  <$> sUpperId

-- Type parser       
pType :: Parser Token Type
pType = foldr (const TypeArray) <$> pType0 <*> many (bracketed (succeed ()))

-- Declaration parser
pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

-- Constant declaration parser
pDeclSemi :: Parser Token Decl
pDeclSemi = const <$> pDecl <*> sSemi

-- Class declaration parser
pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

type Env = Map String Int

