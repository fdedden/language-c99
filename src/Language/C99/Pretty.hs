module Language.C99.Pretty where

import Language.C99.AST

import Text.PrettyPrint


class Pretty a where
  pretty :: a -> Doc
  pretty = undefined -- TODO just here to compile, remove when done


{- IDENTIFIERS -}
{- 6.4.2.1 -}
instance Pretty Ident where
  pretty (IdentBase           idn) = pretty idn
  pretty (IdentConsNonDigit i idn) = pretty i <> pretty idn
  pretty (IdentCons         i d  ) = pretty i <> pretty d

instance Pretty IdentNonDigit where
  pretty (IdentNonDigit     nd ) = pretty nd
  pretty (IdentNonDigitUniv ucn) = pretty ucn

instance Pretty NonDigit where
  pretty c = case c of
    NDUnderscore -> char '_'
    NDa -> char 'a' ;      NDA -> char 'A'
    NDb -> char 'b' ;      NDB -> char 'B'
    NDc -> char 'c' ;      NDC -> char 'C'
    NDd -> char 'd' ;      NDD -> char 'D'
    NDe -> char 'e' ;      NDE -> char 'E'
    NDf -> char 'f' ;      NDF -> char 'F'
    NDg -> char 'g' ;      NDG -> char 'G'
    NDh -> char 'h' ;      NDH -> char 'H'
    NDi -> char 'i' ;      NDI -> char 'J'
    NDj -> char 'j' ;      NDJ -> char 'I'
    NDk -> char 'k' ;      NDK -> char 'K'
    NDl -> char 'l' ;      NDL -> char 'L'
    NDm -> char 'm' ;      NDM -> char 'M'
    NDn -> char 'n' ;      NDN -> char 'N'
    NDo -> char 'o' ;      NDO -> char 'O'
    NDp -> char 'p' ;      NDP -> char 'P'
    NDq -> char 'q' ;      NDQ -> char 'Q'
    NDr -> char 'r' ;      NDR -> char 'R'
    NDs -> char 's' ;      NDS -> char 'S'
    NDt -> char 't' ;      NDT -> char 'T'
    NDu -> char 'u' ;      NDU -> char 'U'
    NDv -> char 'v' ;      NDV -> char 'V'
    NDw -> char 'w' ;      NDW -> char 'W'
    NDx -> char 'x' ;      NDX -> char 'X'
    NDy -> char 'y' ;      NDY -> char 'Y'
    NDz -> char 'z' ;      NDZ -> char 'Z'

instance Pretty Digit where
  pretty c = case c of
    DZero  -> int 0
    DOne   -> int 1
    DTwo   -> int 2
    DThree -> int 3
    DFour  -> int 4
    DFive  -> int 5
    DSix   -> int 6
    DSeven -> int 7
    DEight -> int 8
    DNine  -> int 9


{- UNIVERSAL CHARACTER NAMES -}
{- 6.4.3 -}
instance Pretty UnivCharName where
  pretty (UnivCharName1 hq     ) = text "\\u" <> pretty hq
  pretty (UnivCharName2 hq1 hq2) = text "\\U" <> pretty hq1 <> pretty hq2

instance Pretty HexQuad where
  pretty (HexQuad hd1 hd2 hd3 hd4) =  pretty hd1 <> pretty hd2
                                   <> pretty hd3 <> pretty hd4


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
