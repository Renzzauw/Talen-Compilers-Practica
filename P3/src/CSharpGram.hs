module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex
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
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConst Token
          | ExprVar   Token
          | ExprOper1 Operator1 Operator2 | Operator2 -- =
          | ExprOper2 Operator2 Operator3 | Operator3 -- && | || | <= | < | >= | > | == | != | ^
          | ExprOper3 Operator3 Operator4 | Operator4 -- + | -
          | ExprOper4 Operator4                       -- * | / | %
          deriving Show

data Operator1 = Opr1 Token Expr Expr 
        deriving Show
data Operator2 = Opr2 Token Expr Expr
        deriving Show
data Operator3 = Opr3 Token Expr Expr
        deriving Show
data Operator4 = Opr4 Token Expr Expr
        deriving Show

-- Parser for = operator
sOperator1 :: Parser Token Token
sOperator1 = satisfy isOperator
    where isOperator (Operator "=") = True
          isOperator _              = False

-- Parser for boolean operators          
sOperator2 :: Parser Token Token
sOperator2 = satisfy isOperator
    where isOperator (Operator "&&") = True
          isOperator (Operator "||") = True
          isOperator (Operator "<=") = True
          isOperator (Operator "<")  = True
          isOperator (Operator ">=") = True
          isOperator (Operator ">")  = True
          isOperator (Operator "==") = True
          isOperator (Operator "!=") = True
          isOperator (Operator "^")  = True
          isOperator _               = False

-- Parser for + and - operators
sOperator3 :: Parser Token Token
sOperator3 = satisfy isOperator
    where isOperator (Operator "+") = True
          isOperator (Operator "-") = True
          isOperator _              = False

-- Parser for *, / and % operators
sOperator4 :: Parser Token Token
sOperator4 = satisfy isOperator
    where isOperator (Operator "*") = True
          isOperator (Operator "/") = True
          isOperator (Operator "%") = True
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
           <|> ExprVar   <$> sLowerId
           <|> parenthesised pExpr1
           <|> parenthesised pExpr2
           <|> parenthesised pExpr3
           <|> parenthesised pExpr4

-- OUDE PARSER COMBINATOR EXPRESSIONS
--pExpr :: Parser Token Expr
--pExpr = chainr pExprSimple (ExprOper <$> sOperator)


-- Member parser
pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

-- Declaration parser
pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

-- Declaration type parser (???)         
pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr <*> pStat
     <|> StatReturn <$ symbol KeyReturn <*> pExpr               <*  sSemi
     <|> pBlock
     where optionalElse = option ((\_ x -> x) <$> symbol KeyElse <*> pStat) (StatBlock [])

-- Block parser
pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

-- Method parser
pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> (const TypeVoid <$> symbol KeyVoid)
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

-- Type0 parser (???)      
pType0 :: Parser Token Type
pType0 =  TypePrim <$> sStdType
      <|> TypeObj  <$> sUpperId

-- Type parser       
pType :: Parser Token Type
pType = foldr (const TypeArray) <$> pType0 <*> many (bracketed (succeed ()))

-- Declaration parser
pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

-- Constant declaration parser (???)
pDeclSemi :: Parser Token Decl
pDeclSemi = const <$> pDecl <*> sSemi

-- Class declaration parser (???)
pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

