{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.TMS.TH (
  makeAccessors,
  TypeFormer, noTyParams, withParams, inList, fnToString, fnToVoid,
  datumType, informantType,

  TypeForm(Simple, Params, ParamsL, ParamsToString,
           NodeDatumToString, InformantToString, RuleProc)) where

import Language.Haskell.TH.Syntax
import Control.Monad.ST.Trans

type AccessorSpec = (String, TypeForm, Q Exp)

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

data TypeForm =
  Simple (Q Type) | Params (Q Type) | ParamsL (Q Type) | ParamsToString (Q Type)
  | NodeDatumToString | InformantToString | RuleProc (Q Type)

applyForm ::
  TypeForm -> Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q Type
applyForm (Simple n) _ _ _ _ _ = n
applyForm (Params n) d i r s m = [t| $n $d $i $r $s $m |]
applyForm (ParamsL n) d i r s m = [t| [ $n $d $i $r $s $m ] |]
applyForm (ParamsToString n) d i r s m = [t| ($n $d $i $r $s $m) -> String |]
applyForm (NodeDatumToString) d _ _ _ _ = [t| $d -> String |]
applyForm (InformantToString) _ i _ _ _ = [t| $i -> String |]
applyForm (RuleProc mdCon) d i r s m = [t| $r -> ($mdCon $s $m ()) |]

makeGetter :: Q Type -> Q Type -> Q Exp -> Q Type -> AccessorSpec -> Q [Dec]
makeGetter valType monadType stLayerFn nodeDatumCon (coreString, resultTyFormer, fieldFn) = do
  let getter = mkName ("get" ++ coreString)
  tms <- newName "tms"
  d <- fmap VarT $ newName "d"
  i <- fmap VarT $ newName "i"
  r <- fmap VarT $ newName "r"
  s <- fmap VarT $ newName "s"
  m <- fmap VarT $ newName "m"
  resultType <- applyForm resultTyFormer (return d) (return i) (return r)
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
  resultType <- applyForm resultTyFormer (return d) (return i) (return r)
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
