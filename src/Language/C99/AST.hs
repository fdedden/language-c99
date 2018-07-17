module Language.C99.AST where

{- IDENTIFIERS -}
{- 6.4.2.1 -}
data Ident = IdentBase IdentNonDigit
           | IdentConsNonDigit Ident IdentNonDigit
           | IdentCons Ident Digit

data IdentNonDigit = IdentNonDigit NonDigit
                   | IdentNonDigitUniv UnivCharName
                   {- Other implementation-defined characters -}

data NonDigit = NDUnderscore
              | NDa           | NDA
              | NDb           | NDB
              | NDc           | NDC
              | NDd           | NDD
              | NDe           | NDE
              | NDf           | NDF
              | NDg           | NDG
              | NDh           | NDH
              | NDi           | NDI
              | NDj           | NDJ
              | NDk           | NDK
              | NDl           | NDL
              | NDm           | NDM
              | NDn           | NDN
              | NDo           | NDO
              | NDp           | NDP
              | NDq           | NDQ
              | NDr           | NDR
              | NDs           | NDS
              | NDt           | NDT
              | NDu           | NDU
              | NDv           | NDV
              | NDw           | NDW
              | NDx           | NDX
              | NDy           | NDY
              | NDz           | NDZ

data Digit = DZero
           | DOne
           | DTwo
           | DThree
           | DFour
           | DFive
           | DSix
           | DSeven
           | DEight
           | DNine


{- UNIVERSAL CHARACTER NAMES -}
{- 6.4.3 -}
data UnivCharName = UnivCharName1 HexQuad
                  | UnivCharName2 HexQuad HexQuad

data HexQuad = HexQuad HexDigit HexDigit HexDigit HexDigit


{- CONSTANTS -}
{- 6.4.4 -}
data Const = ConstInt   IntConst
           | ConstFloat FloatConst
           | ConstEnum  EnumConst
           | ConstChar  CharConst

{- 6.4.4.1 -}
data IntConst = IntDec DecConst (Maybe IntSuffix)
              | IntOc  OcConst  (Maybe IntSuffix)
              | IntHex HexConst (Maybe IntSuffix)

data DecConst = DecBase NonzeroDigit
              | DecCons DecConst Digit

data OcConst = OcO
             | OcCons OcConst OcDigit

data HexConst = HexBase HexPrefix HexDigit
              | HexCons HexConst  HexDigit

data HexPrefix = OX

data NonzeroDigit = NonzeroOne
                  | NonzeroTwo
                  | NonzeroThree
                  | NonzeroFour
                  | NonzeroFive
                  | NonzeroSix
                  | NonzeroSeven
                  | NonzeroEight
                  | NonzeroNine

data OcDigit = OcZero
             | OcOne
             | OcTwo
             | OcThree
             | OcFour
             | OcFive
             | OcSix
             | OcSeven

data HexDigit = HexZero
              | HexOne
              | HexTwo
              | HexThree
              | HexFour
              | HexFive
              | HexSix
              | HexSeven
              | HexEight
              | HexNine
              | Hexa         | HexA
              | Hexb         | HexB
              | Hexc         | HexC
              | Hexd         | HexD
              | Hexe         | HexE
              | Hexf         | HexF

data IntSuffix
  = IntSuffixUnsignedLong     UnsignedSuffix (Maybe LongSuffix)
  | IntSuffixUnsignedLongLong UnsignedSuffix LongLongSuffix
  | IntSuffixLong             LongSuffix     (Maybe UnsignedSuffix)
  | IntSuffixLongLong         LongLongSuffix (Maybe UnsignedSuffix)

data UnsignedSuffix = U
data LongSuffix     = L
data LongLongSuffix = LL

{- 6.4.4.2 -}
data FloatConst = FloatDec DecFloatConst
                | FloatHex HexFloatConst

data DecFloatConst
  = DecFloatFrac   FracConst (Maybe ExpPart) (Maybe FloatSuffix)
  | DecFloatDigits DigitSeq  ExpPart         (Maybe FloatSuffix)

data HexFloatConst
  = HexFloatFrac   HexPrefix HexFracConst BinExpPart (Maybe FloatSuffix)
  | HexFloatDigits HexPrefix HexDigitSeq  BinExpPart (Maybe FloatSuffix)

data FracConst = FracZero (Maybe DigitSeq) DigitSeq
               | Frac                      DigitSeq

data ExpPart = E (Maybe Sign) DigitSeq

data Sign = SignPlus
          | SignMinus

data DigitSeq = DigitBase          Digit
              | DigitCons DigitSeq Digit

data HexFracConst = HexFracZero (Maybe HexDigitSeq) HexDigitSeq
                  | HexFrac                         HexDigitSeq

data BinExpPart = P (Maybe Sign) DigitSeq

data HexDigitSeq = HexDigitBase             HexDigit
                 | HexDigitCons HexDigitSeq HexDigit

data FloatSuffix = FF
                 | FL

{- 6.4.4.3 -}
data EnumConst = Enum Ident

{- 6.4.4.4 -}
data CharConst = Char  CCharSeq
               | CharL CCharSeq

data CCharSeq = CCharBase          CChar
              | CCharCons CCharSeq CChar

data CChar = CChar    Char      -- We are a bit lenient here
           | CCharEsc EscSeq

data EscSeq = EscSimple SimpleEscSeq
            | EscOc     OcEscSeq
            | EscHex    HexEscSeq
            | EscUniv   UnivCharName

data SimpleEscSeq = SimpleEscQuote
                  | SimpleEscDQuote
                  | SimpleEscQuestion
                  | SimpleEscBackSlash
                  | SimpleEsca
                  | SimpleEscb
                  | SimpleEscf
                  | SimpleEscn
                  | SimpleEscr
                  | SimpleEsct
                  | SimpleEscv

data OcEscSeq = OcEsc1 OcDigit
              | OcEsc2 OcDigit OcDigit
              | OcEsc3 OcDigit OcDigit OcDigit

data HexEscSeq = HexEscBase           HexDigit
               | HexEscCons HexEscSeq HexDigit


{- STRING LITERALS -}
{- 6.4.5 -}
data StringLit = StringLit  (Maybe SCharSeq)
               | StringLitL (Maybe SCharSeq)

data SCharSeq = SCharBase          SChar
              | SCharCons SCharSeq SChar

data SChar = SChar    Char    -- We are a bit lenient here
           | SCharEsc EscSeq


{- EXPRESSIONS -}
{- 6.5.1 -}
data PrimExpr = PrimIdent  Ident
              | PrimConst  Const
              | PrimString StringLit
              | PrimExpr   Expr

{- 6.5.2 -}
data PostfixExpr = PostfixPrim      PrimExpr
                 | PostfixIndex     PostfixExpr Expr
                 | PostfixFunction  PostfixExpr (Maybe ArgExprList)
                 | PostfixDot       PostfixExpr Ident
                 | PostfixArrow     PostfixExpr Ident
                 | PostfixInc       PostfixExpr
                 | PostfixDec       PostfixExpr
                 | PostfixInits     TypeName    InitList

data ArgExprList = ArgExprListBase AssignExpr
                 | ArgExprListCons ArgExprList AssignExpr

{- 6.5.3 -}
data UnaryExpr = UnaryPostfix   PostfixExpr
               | UnaryInc       UnaryExpr
               | UnaryDec       UnaryExpr
               | UnaryOp        UnaryOp   CastExpr
               | UnarySizeExpr  UnaryExpr
               | UnarySizeType  TypeName

data UnaryOp = OpRef
             | OpDeref
             | OpPlus
             | OpMin
             | OpBNot
             | OpNot

{- 6.5.4 -}
data CastExpr = CastUnary UnaryExpr
              | Cast      TypeName CastExpr

{- 6.5.5 -}
data MultExpr = MultCast           CastExpr
              | MultMult MultExpr  CastExpr
              | MultDiv  MultExpr  CastExpr
              | MultMod  MultExpr  CastExpr

{- 6.5.6 -}
data AddExpr = AddMult         MultExpr
             | AddPlus AddExpr MultExpr
             | AddMin  AddExpr MultExpr

{- 6.5.7 -}
data ShiftExpr = ShiftAdd             AddExpr
               | ShiftLeft  ShiftExpr AddExpr
               | ShiftRight ShiftExpr AddExpr

{- 6.5.8 -}
data RelExpr = RelShift         ShiftExpr
             | RelLT    RelExpr ShiftExpr
             | RelGT    RelExpr ShiftExpr
             | RelLE    RelExpr ShiftExpr
             | RelGE    RelExpr ShiftExpr

{- 6.5.9 -}
data EqExpr = EqRel        RelExpr
            | EqEq  EqExpr RelExpr
            | EqNEq EqExpr RelExpr

{- 6.5.10 -}
data AndExpr = AndEq          EqExpr
             | And    AndExpr EqExpr

{- 6.5.11 -}
data XOrExpr = XOrAnd         AndExpr
             | XOr    XOrExpr AndExpr

{- 6.5.12 -}
data OrExpr = OrXOr        XOrExpr
            | Or    OrExpr XOrExpr

{- 6.5.13 -}
data LAndExpr = LAndOr          OrExpr
              | LAnd   LAndExpr OrExpr

{- 6.5.14 -}
data LOrExpr = LOrAnd         LAndExpr
             | LOr    LOrExpr LAndExpr

{- 6.5.15 -}
data CondExpr = CondLOr LOrExpr
              | Cond Expr CondExpr

{- 6.5.16 -}
data AssignExpr = AssignCond CondExpr
                | Assign UnaryExpr AssignOp AssignExpr

data AssignOp = AssignEq
              | AssignTimes
              | AssignDiv
              | AssignMod
              | AssignAdd
              | AssignSub
              | AssignShiftL
              | AssignShiftR
              | AssignAnd
              | AssignXOr
              | AssignOr

{- 6.5.17 -}
data Expr = ExprAssign      AssignExpr
          | Expr       Expr AssignExpr

{- 6.6 -}
data ConstExpr = Const CondExpr


-- TODO
data TypeName
data InitList
