module Language.C99.Pretty where

import Language.C99.AST

import Text.PrettyPrint


class Pretty a where
  pretty :: a -> Doc
  pretty = undefined -- TODO just here to compile, remove when done


{- IDENTIFIERS -}
{- 6.4.2.1 -}
instance Pretty Ident where

instance Pretty IdentNonDigit where

instance Pretty NonDigit where

instance Pretty Digit where


{- UNIVERSAL CHARACTER NAMES -}
{- 6.4.3 -}
instance Pretty UnivCharName where

instance Pretty HexQuad where


{- CONSTANTS -}
{- 6.4.4 -}
instance Pretty Const where

{- 6.4.4.1 -}
instance Pretty IntConst where

instance Pretty DecConst where

instance Pretty OcConst where

instance Pretty HexConst where

instance Pretty HexPrefix where

instance Pretty NonzeroDigit where

instance Pretty OcDigit where

instance Pretty HexDigit where

instance Pretty IntSuffix where

instance Pretty UnsignedSuffix where
instance Pretty LongSuffix     where
instance Pretty LongLongSuffix where

{- 6.4.4.2 -}
instance Pretty FloatConst where

instance Pretty DecFloatConst where

instance Pretty HexFloatConst where

instance Pretty FracConst where

instance Pretty ExpPart where

instance Pretty Sign where

instance Pretty DigitSeq where

instance Pretty HexFracConst where

instance Pretty BinExpPart where

instance Pretty HexDigitSeq where

instance Pretty FloatSuffix where

{- 6.4.4.3 -}
instance Pretty EnumConst where

{- 6.4.4.4 -}
instance Pretty CharConst where

instance Pretty CCharSeq where

instance Pretty CChar where

instance Pretty EscSeq where

instance Pretty SimpleEscSeq where

instance Pretty OcEscSeq where

instance Pretty HexEscSeq where


{- STRING LITERALS -}
{- 6.4.5 -}
instance Pretty StringLit where

instance Pretty SCharSeq where

instance Pretty SChar where


{- EXPRESSIONS -}
{- 6.5.1 -}
instance Pretty PrimExpr where

{- 6.5.2 -}
instance Pretty PostfixExpr where

instance Pretty ArgExprList where

{- 6.5.3 -}
instance Pretty UnaryExpr where

instance Pretty UnaryOp where

{- 6.5.4 -}
instance Pretty CastExpr where

{- 6.5.5 -}
instance Pretty MultExpr where

{- 6.5.6 -}
instance Pretty AddExpr where

{- 6.5.7 -}
instance Pretty ShiftExpr where

{- 6.5.8 -}
instance Pretty RelExpr where

{- 6.5.9 -}
instance Pretty EqExpr where

{- 6.5.10 -}
instance Pretty AndExpr where

{- 6.5.11 -}
instance Pretty XOrExpr where

{- 6.5.12 -}
instance Pretty OrExpr where

{- 6.5.13 -}
instance Pretty LAndExpr where

{- 6.5.14 -}
instance Pretty LOrExpr where

{- 6.5.15 -}
instance Pretty CondExpr where

{- 6.5.16 -}
instance Pretty AssignExpr where

instance Pretty AssignOp where

{- 6.5.17 -}
instance Pretty Expr where

{- 6.6 -}
instance Pretty ConstExpr where


{- DECLARATIONS -}
{- 6.7 -}
instance Pretty Decln where

instance Pretty DeclnSpecs where

instance Pretty InitDeclrList where

instance Pretty InitDeclr where

{- 6.7.1 -}
instance Pretty StorageClassSpec where

{- 6.7.2 -}
instance Pretty TypeSpec where

{- 6.7.2.1 -}
instance Pretty StructOrUnionSpec where

instance Pretty StructOrUnion where

instance Pretty StructDeclnList where

instance Pretty StructDecln where

instance Pretty SpecQualList where

instance Pretty StructDeclrList where

instance Pretty StructDeclr where

{- 6.7.2.2 -}
instance Pretty EnumSpec where

instance Pretty EnumrList where

instance Pretty Enumr where

{- 6.7.3 -}
instance Pretty TypeQual where

{- 6.7.4 -}
instance Pretty FunSpec where

{- 6.7.5 -}
instance Pretty Declr where

instance Pretty DirectDeclr where

instance Pretty Ptr where

instance Pretty TypeQualList where

instance Pretty ParamTypeList where

instance Pretty ParamList where

instance Pretty ParamDecln where

instance Pretty IdentList where

{- 6.7.6 -}
instance Pretty TypeName where

instance Pretty DirectAbstractDeclr where

{- 6.7.7 -}
instance Pretty TypedefName where

{- 6.7.8 -}
instance Pretty Init where

instance Pretty InitList where

instance Pretty Design where

instance Pretty DesigrList where

instance Pretty Desigr where


{- STATEMENTS -}
{- 6.8 -}
instance Pretty Stmt where

{- 6.8.1 -}
instance Pretty LabeledStmt where

{- 6.8.2 -}
instance Pretty CompoundStmt where

instance Pretty BlockItemList where

instance Pretty BlockItem where

{- 6.8.3 -}
instance Pretty ExprStmt where

{- 6.8.4 -}
instance Pretty SelectStmt where

{- 6.8.5 -}
instance Pretty IterStmt where

{- 6.8.6 -}
instance Pretty JumpStmt where


{- EXTERNAL DEFINITIONS -}
{- 6.9 -}
instance Pretty TransUnit where

instance Pretty ExtDecln where

{- 6.9.1 -}
instance Pretty FunDef where

instance Pretty DeclnList where
