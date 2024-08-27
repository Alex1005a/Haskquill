module Traversal where

import Ast
import Control.Monad.Identity (runIdentity)

mapAstM :: Monad m => (AST -> m AST) -> AST -> m AST
mapAstM act = goAst where
  goAst a@(Entity {}) = act a
  goAst (Union isAll q1 q2) = act =<< Union isAll <$> goAst q1 <*> goAst q2
  goAst (Filter q n body) = act =<< Filter <$> goAst q <*> pure n <*> goAst body
  goAst (Map q n body) = act =<< Map <$> goAst q <*> pure n <*> goAst body
  goAst (FlatMap q n body) = act =<< FlatMap <$> goAst q <*> pure n <*> goAst body
  goAst (GroupBy q n body) = act =<< GroupBy <$> goAst q <*> pure n <*> goAst body
  goAst (OrderBy q n body) = act =<< OrderBy <$> goAst q <*> pure n <*> goAst body
  goAst (Join ty q1 n1 q2 n2 on) = act =<<
    Join ty <$> goAst q1 <*> pure n1 <*> goAst q2 <*> pure n2 <*> goAst on
  goAst (Aggregation aggTy q) = act . Aggregation aggTy =<< goAst q
  goAst (Take i q) = act =<< Take <$> goAst i <*> goAst q
  goAst (Drop i q) = act =<< Drop <$> goAst i <*> goAst q
  goAst (Tuple values) = act . Tuple =<< traverse goAst values
  goAst a@(Const {}) = act a
  goAst (Operation q1 op q2) = act =<< Operation <$> goAst q1 <*> pure op <*> goAst q2
  goAst (UnaryOperation op q) = act . UnaryOperation op =<< goAst q
  goAst (FunApply fun args) = act =<< FunApply <$> goAst fun <*> traverse goAst args
  goAst (Act (Insert entity assigments)) = do
    assigments' <- traverse (\(n, q) ->
        do
          q' <- goAst q
          pure (n, q')
      ) assigments
    entity' <- goAst entity
    act $ Act (Insert entity' assigments')
  goAst (Act (Update q n assigments)) = do
    assigments' <- traverse (\(n', q') ->
        do
          q''<- goAst q'
          pure (n', q'')
      ) assigments
    q' <- goAst q
    act $ Act (Update q' n assigments')
  goAst (Act (Delete q)) = act . Act . Delete =<< goAst q
  goAst (Property q prop) = act =<< Property <$> goAst q <*> pure prop
  goAst a@(Ident {}) = act a
  goAst (Function params body) = act . Function params =<< goAst body
  goAst a@(Lift {}) = act a

mapAst :: (AST -> AST) -> AST -> AST
mapAst act ast = runIdentity $ mapAstM (pure . act) ast
