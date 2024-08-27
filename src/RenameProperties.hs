{-# LANGUAGE TupleSections #-}
module RenameProperties where
  
import Ast
import qualified Control.Monad.State as ST
import qualified Data.Map as Map
import Control.Arrow (first)
import Traversal (mapAstM)

renameProps' :: AST -> ST.State (Map.Map Name [(Name, String)]) AST
renameProps' = mapAstM act
  where
    act :: AST -> ST.State (Map.Map Name [(Name, String)]) AST
    act (Filter (Entity table propAlias) n body) = go Filter table n propAlias body
    act (FlatMap (Entity table propAlias) n body) = go FlatMap table n propAlias body
    act (Map (Entity table propAlias) n body) = go Map table n propAlias body
    act (GroupBy (Entity table propAlias) n body) = go GroupBy table n propAlias body
    act (OrderBy (Entity table propAlias) n body) = go OrderBy table n propAlias body
    act (Join ty (Entity table1 propAlias1) n1 (Entity table2 propAlias2) n2 on) = do
      ST.modify $ Map.insert n2 propAlias2 . Map.insert n1 propAlias1
      Join ty (Entity table1 propAlias1) n1 (Entity table2 propAlias2) n2 <$> renameProps' on
    act (Join ty (Entity table1 propAlias1) n1 q2 n2 on) = do
      ST.modify $ Map.insert n1 propAlias1
      Join ty (Entity table1 propAlias1) n1 <$> renameProps' q2 <*> pure n2 <*> renameProps' on
    act (Join ty q1 n1 (Entity table2 propAlias2) n2 on) = do
      ST.modify $ Map.insert n2 propAlias2
      Join ty <$> renameProps' q1 <*> pure n1 <*> pure (Entity table2 propAlias2) <*> pure n2 <*> renameProps' on
    act a@(Act (Insert (Entity table propAlias) assigments)) =
      if not $ null propAlias then
        let newAssigments = first (\prop -> maybe prop NameStr (lookup prop propAlias)) <$> assigments in
        pure $ Act $ Insert (Entity table propAlias) newAssigments
      else pure a
    act a@(Act (Update q alias assigments)) =
      case getPropAlias q of
        Nothing -> pure a
        Just propAlias -> do
          ST.modify $ Map.insert alias propAlias
          newAssigments <- traverse (\(prop, newVal) -> (maybe prop NameStr (lookup prop propAlias),) <$> renameProps' newVal) assigments
          pure $ Act $ Update q alias newAssigments
    act a@(Property (Ident n) prop) = do
      identAlias <- ST.get
      case Map.lookup n identAlias of
        Nothing -> pure a
        Just propAlias -> do
          let new = maybe prop NameStr (lookup prop propAlias)
          pure $ Property (Ident n) new
    act a = pure a

    go f table n propAlias body = do
      ST.modify $ Map.insert n propAlias
      f (Entity table propAlias) n <$> renameProps' body

    getPropAlias (Filter (Entity _ propAlias) _ _) = Just propAlias
    getPropAlias (Entity _ propAlias) = Just propAlias
    getPropAlias _ = Nothing

renameProps :: AST -> AST
renameProps a = ST.evalState (renameProps' a) Map.empty
