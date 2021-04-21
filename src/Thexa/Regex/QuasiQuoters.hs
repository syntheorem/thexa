module Thexa.Regex.QuasiQuoters where

import PreludePrime

import Language.Haskell.TH (Q)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH qualified as TH

import Thexa.Regex.AST (RegexAST, Regex)
import Thexa.Regex.AST qualified as RE
import Thexa.Regex.CharSet (CharSet)
import Thexa.Regex.CharSet qualified as CS
import Thexa.Regex.CharSet.AST (CharSetAST)
import Thexa.Regex.CharSet.AST qualified as CS (CharSetAST(..))
import Thexa.Regex.Parser

-- | Quasi-quoter for regular expressions. The resulting expression has type 'RE.Regex'.
re :: QuasiQuoter
re = qqExp quoteRegex

-- | Quasi-quoter for character sets. The resulting expression has type 'CS.CharSet'.
cs :: QuasiQuoter
cs = qqExp quoteCharSet

qqExp :: (String -> Q TH.Exp) -> QuasiQuoter
qqExp qq = QuasiQuoter
  { quoteExp  = qq
  , quoteDec  = error "must be used in an expression context"
  , quotePat  = error "must be used in an expression context"
  , quoteType = error "must be used in an expression context"
  }

quoteRegex :: String -> Q TH.Exp
quoteRegex str = case parseRegex str of
  Left errs -> fail (parseErrorsPretty errs)
  Right r   -> [| $(liftRE r) :: Regex |]

quoteCharSet :: String -> Q TH.Exp
quoteCharSet str = case parseCharSet str of
  Left errs -> fail (parseErrorsPretty errs)
  Right c   -> [| $(liftCS c) :: CharSet |]

liftRE :: RegexAST -> Q TH.Exp
liftRE = \case
  RE.Empty        -> [| RE.Empty |]
  RE.Chars c      -> [| RE.chars $(liftCS c) |]
  RE.Seq r1 r2    -> [| RE.Seq $(liftRE r1) $(liftRE r2) |]
  RE.Alt r1 r2    -> [| RE.Alt $(liftRE r1) $(liftRE r2) |]
  RE.Repeat r n m -> [| RE.Repeat $(liftRE r) n m |]
  RE.Splice name  -> TH.varE (TH.mkName name)

liftCS :: CharSetAST -> Q TH.Exp
liftCS = \case
  CS.Chars c     -> [| c |]
  CS.Union c1 c2 -> [| CS.union $(liftCS c1) $(liftCS c2) |]
  CS.Diff  c1 c2 -> [| CS.difference $(liftCS c1) $(liftCS c2) |]
  CS.Splice name -> TH.varE (TH.mkName name)
