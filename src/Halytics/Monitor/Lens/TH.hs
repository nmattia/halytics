module Halytics.Monitor.Lens.TH where

import Language.Haskell.TH
import Control.Lens
import Data.List (foldl')

-- plop :: DecsQ
-- plop = sequence [yeah]

plop :: DecsQ
plop = return [doNow x y | x <- [1 .. 3], y <- [ 2 .. 3], x <= y]
-- plop = return [doNow x y | x <- [1 .. 3], y <- [2 .. 6]]
-- plop = return [doNow 3 2]

doNow :: Int -> Int -> Dec
doNow nField tupSize = InstanceD [] typ [_n nField tupSize]
  where
    typ = AppT (AppT (AppT (AppT (field nField) monTup) monTup') mon) mon'
    monTup = AppT (ConT $ mkName "Monitor") tup
    monTup' = AppT (ConT $ mkName "Monitor") tup'
    mon = AppT (ConT $ mkName "Monitor") (t nField)
    mon' = AppT (ConT $ mkName "Monitor") (t' nField)
    tup = foldl' AppT (TupleT tupSize) ts
    tup' = foldl' AppT (TupleT tupSize) ts'
    ts = t <$> [1 .. tupSize]
    ts' = ts & ix (nField - 1) .~ t' nField

t :: Int -> Type
t n = VarT $ mkName $ "t" ++ show n

t' :: Int -> Type
t' n = VarT $ mkName $ "t" ++ show n ++ "'"

field :: Int -> Type
field n = ConT $ mkName $ "Field" ++ show n

_n :: Int -> Int -> Dec
_n nField tupSize = FunD (mkName $ "_" ++ show nField) [Clause pats body []]
  where
    pats = [VarP $ mkName "k", ConP (mkName "Go")  [TupP $ VarP . mkName <$> varNames]  ]
    varNames = map (\n -> "m" ++ show n) [1 .. tupSize]
    body = NormalB $ AppE (AppE fmap_ (insert nField tupSize)) (funct nField)


-- sub = (Go (m1', m2))
-- sub_ = AppE (ConE (mkName "Go")) (TupE [VarE $ mkName "a'", VarE $ mkName "b"])
-- >
sub :: Int -> Int -> Exp
sub nField tupSize = AppE (ConE (mkName "Go")) (TupE $ (VarE . mkName) <$> varNames')
  where
    varNames' = varNames & ix (nField -1) %~ (++ "'")
    varNames = map (\n -> "m" ++ show n) [1 .. tupSize]

-- > (\(Go m_nField) -> Go (m1, m2, ..., m_nField', ..., m_tupSize))
insert :: Int -> Int -> Exp
insert nField tupSize =
  LamE [ConP (mkName "Go") [VarP $ mkName $ "m" ++ show nField ++ "'"]] (sub nField tupSize)

-- > k (Go m_nField)
funct :: Int -> Exp
funct nField =
  AppE (VarE $ mkName "k")
       $ AppE (ConE $ mkName "Go")
              $ VarE (mkName $ "m" ++ show nField)

fmap_ :: Exp
fmap_ = VarE $ mkName "fmap"

-- yeah :: Q Dec
-- yeah = return $ InstanceD ctx typ decs
--   where ctx = []
--         decs = [inst]
--         typ = AppT (AppT (AppT (AppT (field 1) monTup) monTup') mon) mon'
--         monTup = AppT (ConT $ mkName "Monitor") tup
--         monTup' = AppT (ConT $ mkName "Monitor") tup'
--         mon = AppT (ConT $ mkName "Monitor") (t 1)
--         mon' = AppT (ConT $ mkName "Monitor") (t' 1)
--         tup = AppT (AppT (TupleT 2) (t 1)) (t 2)
--         tup' = AppT (AppT (TupleT 2) (t' 1)) (t 2)
--         inst = FunD (mkName "_1") [Clause pats body []]
--         body = NormalB $ AppE (AppE fmap_ lamb_) funct_
--         lamb_ = LamE [ConP (mkName "Go") [VarP $ mkName "a'"]] sub_
--         sub_ = AppE (ConE (mkName "Go")) (TupE [VarE $ mkName "a'", VarE $ mkName "b"])
--         funct_ = AppE (VarE $ mkName "k") $ AppE (ConE $ mkName "Go") $ VarE (mkName "a")
--         pats = [ VarP $ mkName "k", ConP (mkName "Go") [TupP [VarP $ mkName "a", VarP $ mkName "b"]]]
