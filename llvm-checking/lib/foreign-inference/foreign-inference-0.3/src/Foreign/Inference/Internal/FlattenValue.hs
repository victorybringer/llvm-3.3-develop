module Foreign.Inference.Internal.FlattenValue ( flattenValue ) where

import Data.List ( foldl' )
import Data.HashSet ( HashSet )
import qualified Data.HashSet as S

import LLVM.Analysis

-- | Turn a value into a list of all of the possible values it could
-- represent.  This effectively means returning all possible values
-- that phi and select instructions could point to.
flattenValue :: Value -> [Value]
flattenValue = S.toList . go S.empty . S.singleton

go :: HashSet Value -> HashSet Value -> HashSet Value
go visited q
  | S.null vals = visited
  | otherwise =
    let visited' = visited `S.union` vals
        q' = foldl' addValuesFrom S.empty (S.toList vals)
    in go visited' q'
  where
    vals = S.difference q visited

addValuesFrom :: HashSet Value -> Value -> HashSet Value
addValuesFrom q v =
  case valueContent' v of
    InstructionC PhiNode { phiIncomingValues = pvs } ->
      let vs = map fst pvs
      in foldr S.insert q vs
    InstructionC SelectInst { selectTrueValue = tv, selectFalseValue = fv } ->
      foldr S.insert q [tv, fv]
    _ -> S.insert v $ S.insert (stripBitcasts v) q
