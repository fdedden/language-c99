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


-- TODO
data HexDigit
