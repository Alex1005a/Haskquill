{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE RankNTypes #-}
module DoNotationDesugar where

import Data.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

--https://stackoverflow.com/questions/65417779/traversing-a-template-haskell-ast
doNotationAsInScala :: Data d => d -> d
doNotationAsInScala = everywhere (mkT doAsScala)
  where doAsScala (DoE mb_mod stmts) = DoE mb_mod (dsDoStmts mb_mod stmts)
        doAsScala e = e

-- If the last statement in the do block is pure and there is bind before it,
-- then we transform both statements into fmap.
dsDoStmts ::  Maybe ModName -> [Stmt] -> [Stmt]
dsDoStmts mbMod = go
  where
    go :: [Stmt] -> [Stmt]
    go [] = []
    go [BindS pat bexp, NoBindS (AppE (VarE n) arg)]
        | n == 'pure =
        let fmapName = mkQualDoName mbMod 'fmap in
        [NoBindS $ AppE (AppE (VarE fmapName) (LamE [pat] arg)) bexp]
    go (stmt : rest) = do
      stmt : go rest

mkQualDoName  :: Maybe ModName -> Name -> Name
mkQualDoName  mbMod origName = case mbMod of
  Nothing   -> origName
  Just mod_ -> Name (OccName (nameBase origName)) (NameQ mod_)
