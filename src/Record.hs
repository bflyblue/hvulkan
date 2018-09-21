{-# LANGUAGE TemplateHaskell #-}

module Record
    ( makeRecord
    , module Foreign.Ptr
    , module Foreign.Storable
    )
where

import           Data.Char
import           Foreign.Ptr
import           Foreign.Storable
import           Language.Haskell.TH

{-
data pair
  = pair
    { a :: a
    , b :: b
    }

makeRecord ''Pair
aOffset  = 0
aSize    = sizeOf (undefined :: A)
bOffset  = aOffset + aSize
bSize    = sizeOf (undefined :: B)
pairSize = aSize + bSize

instance Storable TileFixedData where
    sizeOf _ = pairSize
    alignment _ = 0
    peek ptr =
        Pair
            <$> peek (plusPtr ptr aOffset)
            <*> peek (plusPtr ptr bOffset)
    poke ptr x = do
        poke (plusPtr ptr aOffset) (a x)
        poke (plusPtr ptr bOffset) (b x)
-}

makeRecord :: Name -> Q [Dec]
makeRecord ty = do
    (TyConI tyCon) <- reify ty
    (tyConName, c) <- case tyCon of
        DataD _ nm _ _ [c] _ -> return (nm, c)
        _ -> fail $ "makeRecord: " ++ show ty ++ " must be data with single constructor"
    (recConName, recFields) <- case c of
        RecC nm flds -> return (nm, flds)
        _ -> fail $ "makeRecord: " ++ show ty ++ " must be record constructor"
    fieldDefs <- mkFields (LitE $ IntegerL 0) recFields
    let sizeName = mkName (toLowerName $ nameBase tyConName ++ "Size")
    recDefs <- mkRec sizeName recFields
    inst <- mkDeriving tyConName recConName sizeName recFields
    return $ fieldDefs ++ recDefs ++ inst
  where
    mkFields _ [] = return []
    mkFields offset ((fldName, _, fldTy) : flds) = do
        let offsetName = mkName (nameBase fldName ++ "Offset")
            sizeName   = mkName (nameBase fldName ++ "Size")
            defs =
                [ SigD offsetName (ConT ''Int)
                , FunD offsetName [
                    Clause [] (
                      NormalB offset
                    ) []
                  ]
                , SigD sizeName (ConT ''Int)
                , FunD sizeName [
                    Clause [] (
                      NormalB (
                        AppE
                          (VarE 'sizeOf)
                          (SigE (VarE 'undefined) fldTy)
                      )
                    ) []
                  ]
                ]
            offset' = InfixE (Just $ VarE offsetName) (VarE '(+)) (Just $ VarE sizeName)
        defs' <- mkFields offset' flds
        return $ defs ++ defs'

    mkRec sizeName flds =
        return
          [ SigD sizeName (ConT ''Int)
          , FunD sizeName [
              Clause [] (
                NormalB (
                  AppE
                    (VarE 'sum)
                    (ListE (map (\(fldName, _, _) -> VarE $ mkName (nameBase fldName ++ "Size")) flds))
                )
              ) []
            ]
          ]

    mkDeriving tyConName recConName sizeName flds = do
        peekPtr <- newName "ptr"
        pokePtr <- newName "ptr"
        pokeA   <- newName "a"
        let peekE v (n, _, _) = AppE (VarE $ mkName "peek")
                                     (AppE (AppE (VarE 'plusPtr) (VarE v))
                                           (VarE $ mkName (nameBase n ++ "Offset")))
            peeks = map (peekE peekPtr) flds
            pokeE v a (n, _, _)
                              = AppE (AppE (VarE $ mkName "poke")
                                           (AppE (AppE (VarE 'plusPtr) (VarE v))
                                                 (VarE $ mkName (nameBase n ++ "Offset"))))
                                     (AppE (VarE $ mkName (nameBase n))
                                           (VarE a))
            pokes = map (pokeE pokePtr pokeA) flds
        return
          [ InstanceD Nothing [] (AppT (ConT ''Storable) (ConT tyConName) )
            [ FunD (mkName "sizeOf") [
                Clause [WildP] (NormalB (VarE sizeName)) []
              ]
            , FunD (mkName "alignment") [
                Clause [WildP] (NormalB (LitE $ IntegerL 0)) []
              ]
            , FunD (mkName "peek") [
                Clause [VarP peekPtr] (
                  NormalB (
                    foldl
                      (\a b -> InfixE (Just a)
                                      (VarE '(<*>))
                                      (Just b))
                      (InfixE (Just $ ConE recConName)
                              (VarE '(<$>))
                              (Just $ head peeks))
                      (tail peeks)
                  )
                ) []
              ]
            , FunD (mkName "poke") [
                Clause [VarP pokePtr, VarP pokeA] (
                  NormalB (
                    DoE $ map NoBindS pokes
                  )
                ) []
              ]
            ]
          ]

toLowerName :: String -> String
toLowerName (c:cs) = toLower c : cs
toLowerName _      = error "makeRecord: name must contain at least one character"
