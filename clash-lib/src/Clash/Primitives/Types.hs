{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016-2017, Myrtle Software Ltd
                    2018     , Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Type and instance definitions for Primitive
-}

{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Clash.Primitives.Types
  ( TemplateSource(..)
  , TemplateType(..)
  , BlackBoxFunctionName(..)
  , Primitive(..)
  , PrimMap
  , UnresolvedPrimitive
  , ResolvedPrimitive
  , ResolvedPrimMap
  , CompiledPrimitive
  , CompiledPrimMap
  , removeTypeTag
  ) where

import           Clash.Netlist.BlackBox.Types
  (BlackBoxTemplate, BlackBoxFunction)
import           Control.Applicative          ((<|>))
import           Data.Aeson
  (FromJSON (..), Value (..), (.:), (.:?), (.!=))
import           Data.Char                    (isUpper, isLower, isAlphaNum)
import           Data.Either                  (lefts)
import           Data.HashMap.Lazy            (HashMap)
import qualified Data.HashMap.Strict          as H
import           Data.List                    (intercalate)
import qualified Data.Text                    as S
import           Data.Text.Lazy               (Text)
import           GHC.Stack                    (HasCallStack)

-- | An unresolved primitive still contains pointers to files.
type UnresolvedPrimitive = Primitive Text TemplateSource (Maybe TemplateSource)

-- | A parsed primitive does not contain pointers to filesystem files anymore,
-- but holds uncompiled @BlackBoxTemplate@s and @BlackBoxFunction@s.
type ResolvedPrimitive = Primitive Text Text (Maybe Text)
type ResolvedPrimMap   = PrimMap ResolvedPrimitive

-- | A compiled primitive has compiled all templates and functions from its
-- @ResolvedPrimitive@ counterpart.
type CompiledPrimitive = Primitive BlackBoxTemplate BlackBoxTemplate BlackBoxFunction
type CompiledPrimMap   = PrimMap CompiledPrimitive

-- | A @PrimMap@ maps primitive names to a @Primitive@
type PrimMap a = HashMap S.Text a

data TemplateType a
  = TDecl a
  | TExpr a
  deriving (Show, Eq, Functor, Foldable, Traversable)

removeTypeTag :: TemplateType a -> a
removeTypeTag (TDecl a) = a
removeTypeTag (TExpr a) = a

-- | A BBFN is a parsed version of a fully qualified function name. It is
-- guaranteed to have at least one module name which is not /Main/.
data BlackBoxFunctionName =
  BlackBoxFunctionName [String] String
    deriving (Eq)

instance Show BlackBoxFunctionName where
  show (BlackBoxFunctionName mods funcName) =
    "BBFN<" ++ intercalate "." mods ++ "." ++ funcName ++ ">"

-- | Quick and dirty implementation of Text.splitOn for Strings
splitOn :: String -> String -> [String]
splitOn (S.pack -> sep) (S.pack -> str) =
  map S.unpack $ S.splitOn sep str

-- | Parses a string into a list of modules and a function name. I.e., it parses
-- the string "Clash.Primitives.Types.parseBBFN" to ["Clash", "Primitives",
-- "Types"] and "parseBBFN". The result is stored as a BlackBoxFunctionName.
parseBBFN
  :: HasCallStack
  => String
  -> Either String BlackBoxFunctionName
parseBBFN bbfn =
  case splitOn "." bbfn of
    []  -> Left $ "Empty function name: " ++ bbfn
    [_] -> Left $ "No module or function defined: " ++ bbfn
    nms ->
      let (mods, func) = (init nms, last nms) in
      let errs = lefts $ checkFunc func : map checkMod mods in
      case errs of
        [] -> Right $ BlackBoxFunctionName mods func
        _  -> Left $ "Error while parsing " ++ show bbfn ++ ": " ++ head errs
  where
    checkMod mod'
      | isLower (head mod') =
          Left $ "Module name cannot start with lowercase: " ++ mod'
      | any (not . isAlphaNum) mod' =
          Left $ "Module name must be alphanumerical: " ++ mod'
      | otherwise =
          Right mod'

    checkFunc func
      | isUpper (head func) =
          Left $ "Function name must start with lowercase: " ++ func
      | otherwise =
          Right func

data TemplateSource
  = TFile FilePath
  -- ^ Template source stored in file on filesystem
  | TInline Text
  -- ^ Template stored inline
  deriving (Show, Eq)

-- | Externally defined primitive
data Primitive a b c
  -- | Primitive template written in a Clash specific templating language
  = BlackBox
  { name      :: !S.Text
    -- ^ Name of the primitive
  , outputReg :: Bool
    -- ^ Verilog only: whether the result should be a /reg/(@True@) or /wire/
    -- (@False@); when not specified in the /.json/ file, the value will default
    -- to @False@ (i.e. /wire/).
  , libraries :: [a]
    -- ^ VHDL only: add /library/ declarations for the given names
  , imports   :: [a]
    -- ^ VHDL only: add /use/ declarations for the given names
  , includes  :: [((S.Text,S.Text),b)]
    -- ^ Create files to be included with the generated primitive. The fields
    -- are ((name, extension), content), where content is a template of the file
    -- Defaults to @[]@ when not specified in the /.json/ file
  , template :: TemplateType b
    -- ^ Used to indiciate type of template (declaration or expression). Will be
    -- filled with @Template@ or an @Either decl expr@.
  }
  -- | Primitive template rendered by a Haskell function (given as raw source code)
  | BlackBoxHaskell
  { name :: !S.Text
    -- ^ Name of the primitive
  , functionName :: BlackBoxFunctionName
  , function :: TemplateType c
    -- ^ Used to indiciate type of template (declaration or expression).
  }
  -- | A primitive that carries additional information
  | Primitive
  { name     :: !S.Text
    -- ^ Name of the primitive
  , primType :: !Text
    -- ^ Additional information
  }
  deriving Show

instance FromJSON UnresolvedPrimitive where
  parseJSON (Object v) =
    case H.toList v of
      [(conKey,Object conVal)] ->
        case conKey of
          "BlackBoxHaskell"  -> do
            name' <- conVal .: "name"
            fName <- conVal .: "functionName"
            isDecl <- conVal .:? "isDeclaration" .!= True
            templ <- (Just . TInline <$> conVal .: "template")
                 <|> (Just . TFile <$> conVal .: "templateFile")
                 <|> (pure Nothing)

            let templ' = (if isDecl then TDecl else TExpr) templ
            let fName' = either error id (parseBBFN fName)

            return (BlackBoxHaskell name' fName' templ')
          "BlackBox"  ->
            BlackBox <$> conVal .: "name"
                     <*> conVal .:? "outputReg" .!= False
                     <*> conVal .:? "libraries" .!= []
                     <*> conVal .:? "imports" .!= []
                     <*> (conVal .:? "includes" .!= [] >>= traverse parseInclude)
                     <*> ( (TDecl . TInline <$> conVal .: "templateD")
                       <|> (TExpr . TInline <$> conVal .: "templateE")
                       <|> (TDecl . TFile   <$> conVal .: "templateFileD")
                       <|> (TDecl . TFile   <$> conVal .: "templateFileE")
                         )
          "Primitive" ->
            Primitive <$> conVal .: "name"
                      <*> conVal .: "primType"

          e -> error $ "[1] Expected: BlackBox or Primitive object, got: " ++ show e
      e -> error $ "[2] Expected: BlackBox or Primitive object, got: " ++ show e
    where
      parseInclude c =
        (,) <$> ((,) <$> c .: "name" <*> c .: "extension")
            <*> (TInline <$> c .: "content" <|>
                 TFile   <$> c .: "file")
  parseJSON unexpected =
    error $ "[3] Expected: BlackBox or Primitive object, got: " ++ show unexpected
