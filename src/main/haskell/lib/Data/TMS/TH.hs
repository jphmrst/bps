{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.TMS.TH (
  makeAccessors,
  TypeForm(Simple, Params, ParamsL, ParamsToString,
           NodeDatumToString, InformantToString, RuleProc)) where

import Language.Haskell.TH.Syntax
import Control.Monad.ST.Trans

makeAccessors :: Q Type -> Q Type -> Q Exp -> Q Type
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
tarr :: Q Type -> Q Type -> Q Type
tarr a b = [t| $a -> $b |]

infixl `tapp`
tapp :: Q Type -> Q Type -> Q Type
tapp a b = [t| $a $b |]

data TypeForm =
  Simple Name | Params Name | ParamsL Name | ParamsToString Name
  | NodeDatumToString | InformantToString | RuleProc Name

qtf :: Name -> Q Type
qtf = return . ConT

applyForm ::
  TypeForm -> Q Type -> Q Type -> Q Type -> Q Type -> Q Type -> Q Type
applyForm (Simple n) _ _ _ _ _ = qtf n
applyForm (Params n) d i r s m = [t| $(return $ ConT n)  $d $i $r $s $m |]
applyForm (ParamsL n) d i r s m = [t| [ $(return $ ConT n) $d $i $r $s $m ] |]
applyForm (ParamsToString n) d i r s m =
  [t| ($(return $ ConT n) $d $i $r $s $m) -> String |]
applyForm (NodeDatumToString) d _ _ _ _ = [t| $d -> String |]
applyForm (InformantToString) _ i _ _ _ = [t| $i -> String |]
applyForm (RuleProc mdCon) d i r s m =
  [t| $r -> ($(return $ ConT mdCon) $s $m ()) |]

makeGetter :: Q Type -> Q Type -> Q Exp -> Q Type -> (String, TypeForm, Name) -> Q [Dec]
makeGetter valType monadType stLayerFn nodeDatumCon (getterString, resultTyFormer, fieldFn) = do
  let getter = mkName getterString
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
                       (AppE (VarE fieldFn) (VarE tms)))))
          []
        ]
    ]
-- getDebugging :: (Monad m, NodeDatum d) => LTMS d i r s m -> LTMST s m Bool
-- {- # INLINE getDebugging #-}
-- getDebugging = getLTMSMutable ltmsDebugging
-- getLTMSMutable refGetter ltms = sttLayer $ readSTRef (refGetter ltms)
-- getLTMSMutable refGetter = sttLayer . readSTRef . refGetter

makeSetter :: Q Type -> Q Type -> Q Exp -> Q Type -> (String, TypeForm, Name) -> Q [Dec]
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
