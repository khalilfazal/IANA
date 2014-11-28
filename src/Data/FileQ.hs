module Data.FileQ (
    f
) where

import Language.Haskell.TH       (stringE)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter), quoteDec, quoteExp, quoteFile, quotePat, quoteType)

f :: QuasiQuoter
f = quoteFile QuasiQuoter {
    quoteDec  = undefined,
    quoteExp  = stringE,
    quotePat  = undefined,
    quoteType = undefined
}
