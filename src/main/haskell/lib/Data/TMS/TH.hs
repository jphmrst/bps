{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.TMS.TH (
  makeAccessors,
  TypeForm(Simple, Params, ParamsL, ParamsToString, NodeDatumToString)) where

import Language.Haskell.TH.Syntax
import Control.Monad.ST.Trans

makeAccessors :: Name -> Name -> Name -> Name
                  -> [(String, TypeForm, Name)]
                   -> [(String, TypeForm, Name)]
                    -> Q [Dec]
makeAccessors valType monadType stLayerFn nodeDatumCon readOnly readWrite =do
  getters1 <- mapM (makeGetter valType monadType stLayerFn nodeDatumCon) readOnly
  getters2 <- mapM (makeGetter valType monadType stLayerFn nodeDatumCon) readWrite
  setters2 <- mapM (makeSetter valType monadType stLayerFn nodeDatumCon) readWrite
  return $
    foldr (++) [] getters1 ++ foldr (++) [] getters2 ++ foldr (++) [] setters2
--   $getter ::
--      (Monad m, NodeDatum d) => LTMS d i r s m -> LTMST s m [$typ]
--   {-# INLINE $getter #-}


infixr `tarr`
tarr :: Type -> Type -> Type
tarr a b = (AppT (AppT ArrowT a) b)

infixl `tapp`
tapp :: Type -> Type -> Type
tapp a b = AppT a b

data TypeForm =
  Simple Name | Params Name | ParamsL Name | ParamsToString Name
  | NodeDatumToString

applyForm :: TypeForm -> Name -> Name -> Name -> Name -> Name -> Type
applyForm (Simple n) _ _ _ _ _ = ConT n
applyForm (Params n) d i r s m =
  (ConT n) `tapp` (VarT d) `tapp` (VarT i) `tapp` (VarT r)
           `tapp` (VarT s) `tapp` (VarT m)
applyForm (ParamsL n) d i r s m =
  ListT `tapp` ((ConT n) `tapp` (VarT d) `tapp` (VarT i) `tapp` (VarT r)
                         `tapp` (VarT s) `tapp` (VarT m))
applyForm (ParamsToString n) d i r s m =
  ((ConT n) `tapp` (VarT d) `tapp` (VarT i) `tapp` (VarT r)
    `tapp` (VarT s) `tapp` (VarT m)) `tarr` (ConT ''String)
applyForm (NodeDatumToString) d i r s m = (VarT d) `tarr` (ConT ''String)

makeGetter :: Name -> Name -> Name -> Name -> (String, TypeForm, Name) -> Q [Dec]
makeGetter valType monadType stLayerFn nodeDatumCon (getterString, resultTyFormer, fieldFn) = do
  let getter = mkName getterString
  tms <- newName "tms"
  d <- newName "d"
  i <- newName "i"
  r <- newName "r"
  s <- newName "s"
  m <- newName "m"
  let resultType = applyForm resultTyFormer d i r s m
  return [
    SigD getter
      (ForallT [] [ AppT (ConT ''Monad) (VarT m),
                    AppT (ConT nodeDatumCon) (VarT d) ]
        (((ConT valType) `tapp` (VarT d) `tapp` (VarT i) `tapp` (VarT r)
                         `tapp` (VarT s) `tapp` (VarT m))
          `tarr` ((ConT monadType) `tapp` (VarT s) `tapp` (VarT m)
                                   `tapp` resultType))),
    PragmaD (InlineP getter Inline FunLike AllPhases),
    FunD getter [
        Clause [VarP tms]
          (NormalB (AppE
                     (VarE stLayerFn)
                     (AppE
                       (VarE 'readSTRef)
                       (AppE (VarE fieldFn) (VarE tms)))))
          []
        ]
    ]
-- getDebugging :: (Monad m, NodeDatum d) => LTMS d i r s m -> LTMST s m Bool
-- {- # INLINE getDebugging #-}
-- getDebugging = getLTMSMutable ltmsDebugging
-- getLTMSMutable refGetter ltms = sttLayer $ readSTRef (refGetter ltms)
-- getLTMSMutable refGetter = sttLayer . readSTRef . refGetter

makeSetter :: Name -> Name -> Name -> Name -> (String, TypeForm, Name) -> Q [Dec]
makeSetter valType monadType stLayerFn nodeDatumCon (setterString, resultFormer, fieldFn) =
  return []

{-
makeGetter valTyCon monadTyCon getterString resultType fieldFn =
--   $getter ::
--      (Monad m, NodeDatum d) => LTMS d i r s m -> LTMST s m [$typ]
--   {-# INLINE $getter #-}
  return [
    FunD (mkName getterString) [
        Clause []
          (NormalB (AppE (VarE getMutable) (VarE field)))
          []
        ]
    ]

makeGetterSetter :: String -> String -> Name -> Name -> Name -> Name -> Q [Dec]
makeGetterSetter getter setter getMutable setMutable typ field =
--   $getter ::
--      (Monad m, NodeDatum d) => LTMS d i r s m -> LTMST s m [$typ]
--   {-# INLINE $getter #-}
  return [
    FunD (mkName getter) [
        Clause []
          (NormalB (AppE (VarE getMutable) (VarE field)))
          []
        ],
    FunD (mkName setter) [
        Clause []
          (NormalB (AppE (VarE setMutable) (VarE field)))
          []
        ]
  ]
-}
