{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Verification
-}
{-# LANGUAGE BinaryLiterals        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}

module Clash.Verification.Types
 ( CvExpression
 , CvExpression'(..)
 , CvProperty
 , CvProperty'(..)
 , CvAssertion
 , CvAssertion'(..)
 , CvResult(..)
 , CvValue
 , CvRenderAs(..)
 , toCvExpr
 )
 where

import           Data.Text                      (Text)

import Clash.Annotations.BitRepresentation
  (ConstrRepr(..), DataReprAnn(..), liftQ)
import           Clash.Signal.Internal          (Domain, Signal)

-- |
data CvRenderAs
  = PSL
  -- ^ Property Specification Language
  | SVA
  -- ^ SystemVerilog Assertions
  deriving (Show)

data CvExpression' a
  = CvPure a
  | CvLit Bool

  | CvAnd (CvExpression' a) (CvExpression' a)
  | CvOr (CvExpression' a) (CvExpression' a)
  | CvImplies (CvExpression' a) (CvExpression' a)

  | CvNext Int (CvExpression' a)
  | CvAfter (CvExpression' a) (CvExpression' a)
  | CvTemporalImplies Int (CvExpression' a) (CvExpression' a)
  deriving (Show, Functor, Foldable, Traversable)

data CvProperty' a
  = CvAlways (CvExpression' a)
  | CvNever (CvExpression' a)
  deriving (Show, Functor, Foldable, Traversable)

data CvAssertion' a
  = CvAssert (CvProperty' a)
  | CvCover (CvProperty' a)
  deriving (Show, Functor, Foldable, Traversable)

type CvExpression (dom :: Domain) = CvExpression' (Maybe Text, Signal dom Bool)
type CvProperty (dom :: Domain) = CvProperty' (Maybe Text, Signal dom Bool)
type CvAssertion (dom :: Domain) = CvAssertion' (Maybe Text, Signal dom Bool)

data CvResult = CvResult
  { cvPropName :: !String  -- I'd like text, but Clash complains :[
  -- ^ Name of property belonging to this result
  , cvPass :: !Bool
  -- ^ False whenever property is violated, True otherwise
  }
  deriving (Eq)
{-# ANN module (
  DataReprAnn
    $(liftQ [t| CvResult |])
    0
    [ ConstrRepr 'CvResult 0 0 [0b0, 0b0]
    ]) #-}
{- ^ Mark as zero-width so Clash won't stumble on the fact it's unrepresentable. -}

class CvValue dom a where
  toCvExpr :: a -> CvExpression dom

instance CvValue dom Bool where
  toCvExpr = CvLit
  {-# INLINE toCvExpr #-}

instance dom1 ~ dom2 => CvValue dom1 (CvExpression dom2) where
  toCvExpr = id
  {-# INLINE toCvExpr #-}

instance CvValue dom (Signal dom Bool) where
  toCvExpr s = CvPure (Nothing, s)
  {-# INLINE toCvExpr #-}
