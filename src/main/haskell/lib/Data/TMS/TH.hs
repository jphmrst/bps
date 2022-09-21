{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.TMS.TH (
  makeAccessors,
  TypeFormer, noTyParams, withParams, inList, fnToString, fnToVoid,
  datumType, informantType, ruleType, ruleTypeToVoidComp -- ,
  -- TypeForm(Simple, Params, ParamsL, ParamsToString,
  --       NodeDatumToString, InformantToString, RuleProc)
  ) where

import Language.Haskell.TH.Syntax
import Control.Monad.ST.Trans

type AccessorSpec = (String, TypeFormer, Q Exp)

makeAccessors :: Q Type -> Q Type -> Q Exp -> Q Type
                  -> [AccessorSpec] -> [AccessorSpec] -> Q [Dec]
makeAccessors valType monadType stLayerFn nodeDatumCon readOnly readWrite =do
  getters1 <- mapM (makeGetter valType monadType stLayerFn nodeDatumCon) readOnly
  getters2 <- mapM (makeGetter valType monadType stLayerFn nodeDatumCon) readWrite
  setters2 <- mapM (makeSetter valType monadType stLayerFn nodeDatumCon) readWrite
  return $
    foldr (++) [] getters1 ++ foldr (++) [] getters2 ++ foldr (++) [] setters2

type TypeFormer = Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q Type

noTyParams :: Q Type -> TypeFormer
noTyParams base _ _ _ _ _ = base

withParams :: Q Type -> TypeFormer
withParams base d i r s m = [t| $base $d $i $r $s $m |]

inList :: TypeFormer -> TypeFormer
inList f d i r s m = [t| [ $(f d i r s m) ] |]

fnToString :: TypeFormer -> TypeFormer
fnToString f d i r s m = [t| ($(f d i r s m)) -> String |]

fnToVoid :: TypeFormer -> TypeFormer
fnToVoid f d i r s m = [t| ($(f d i r s m)) -> () |]

datumType :: TypeFormer
datumType d i r s m = d

informantType :: TypeFormer
informantType d i r s m = i

ruleType :: TypeFormer
ruleType d i r s m = r

ruleTypeToVoidComp :: Q Type -> TypeFormer
ruleTypeToVoidComp comp d i r s m = [t| $r -> $comp $s $m () |]

makeGetter :: Q Type -> Q Type -> Q Exp -> Q Type -> AccessorSpec -> Q [Dec]
makeGetter valType monadType stLayerFn nodeDatumCon (coreString, resultTyFormer, fieldFn) = do
  let getter = mkName ("get" ++ coreString)
  tms <- newName "tms"
  d <- fmap VarT $ newName "d"
  i <- fmap VarT $ newName "i"
  r <- fmap VarT $ newName "r"
  s <- fmap VarT $ newName "s"
  m <- fmap VarT $ newName "m"
  resultType <- resultTyFormer (return d) (return i) (return r)
                               (return s) (return m)
  coreType <- [t| ($valType $(return d) $(return i) $(return r) $(return s) $(return m))
                    -> ($monadType $(return s) $(return m) $(return resultType)) |]
  field <- fieldFn
  stLayer <- stLayerFn
  nodeDatum <- nodeDatumCon
  return [
    SigD getter
      (ForallT [] [ AppT (ConT ''Monad) m,
                    AppT nodeDatum d ]
        coreType),
    PragmaD (InlineP getter Inline FunLike AllPhases),
    FunD getter [
        Clause [VarP tms]
          (NormalB (AppE
                     stLayer
                     (AppE
                       (VarE 'readSTRef)
                       (AppE field (VarE tms)))))
          []
        ]
    ]
makeSetter :: Q Type -> Q Type -> Q Exp -> Q Type -> AccessorSpec -> Q [Dec]
makeSetter valType monadType stLayerFn nodeDatumCon (coreString, resultTyFormer, fieldFn) = do
  let setter = mkName ("set" ++ coreString)
  let arg = mkName "arg"
  tms <- newName "tms"
  d <- fmap VarT $ newName "d"
  i <- fmap VarT $ newName "i"
  r <- fmap VarT $ newName "r"
  s <- fmap VarT $ newName "s"
  m <- fmap VarT $ newName "m"
  resultType <- resultTyFormer (return d) (return i) (return r)
                               (return s) (return m)
  coreType <-
    [t| ($valType $(return d) $(return i) $(return r) $(return s) $(return m))
          -> $(return resultType)
            -> ($monadType $(return s) $(return m) ()) |]
  field <- fieldFn
  stLayer <- stLayerFn
  nodeDatum <- nodeDatumCon
  return [
    SigD setter
      (ForallT [] [ AppT (ConT ''Monad) m,
                    AppT nodeDatum d ]
        coreType),
    PragmaD (InlineP setter Inline FunLike AllPhases),
    FunD setter [
        Clause [VarP tms, VarP arg]
          (NormalB (AppE
                     stLayer
                     (AppE (AppE
                             (VarE 'writeSTRef)
                             (AppE field (VarE tms)))
                           (VarE arg))))
          []
        ]
    ]
