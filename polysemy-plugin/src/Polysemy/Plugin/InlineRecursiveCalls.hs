module Polysemy.Plugin.InlineRecursiveCalls
  ( inlineRecursiveCalls
  ) where

import BasicTypes
import Control.Monad
import Control.Monad.Trans.State
import CoreMonad
import CoreSyn
import Data.Monoid
import Data.Traversable
import GHC
import Generics.SYB
import HscTypes
import IdInfo
import Name
import UniqSupply
import Unique
import Var
import Outputable hiding ((<>))

inlineRecursiveCalls :: ModGuts -> CoreM ModGuts
inlineRecursiveCalls mg = do
  uniqSupply <- liftIO $ mkSplitUniqSupply '\x264a'
  flip evalStateT uniqSupply $ do
    bs <- traverse loopbreakBinds $ mg_binds mg
    pure $ mg { mg_binds = bs }


type CoreSupplyM = StateT UniqSupply CoreM


getUniq :: CoreSupplyM Unique
getUniq = do
  (u, s) <- gets takeUniqFromSupply
  put s
  pure u


containsName :: CoreBndr -> CoreExpr -> Bool
containsName n e =
  getAny $
    everything
      (<>)
      (mkQ (Any False) $ matches n)
      e


matches :: CoreBndr -> CoreExpr -> Any
matches n (Var n') | n == n' = Any True
matches _ _ = Any False


replace :: Data a => Id -> Id -> a -> a
replace n n' = everywhere $ mkT go
  where
    go :: Expr CoreBndr -> Expr CoreBndr
    go v@(Var nn)
      | nn == n   = Var n'
      | otherwise = v
    go x = x


noInlinePragma :: InlinePragma
noInlinePragma = defaultInlinePragma { inl_inline = NoInline }

clearIsLoopbreaker :: OccInfo -> OccInfo
clearIsLoopbreaker (IAmALoopBreaker _ tci) = ManyOccs tci
clearIsLoopbreaker a = a

setIsLoopbreaker :: OccInfo -> OccInfo
setIsLoopbreaker (ManyOccs tci) = IAmALoopBreaker False tci
setIsLoopbreaker a = a

modifyOccInfo :: (OccInfo -> OccInfo) -> IdInfo -> IdInfo
modifyOccInfo f info = setOccInfo info $ f $ occInfo info


loopbreaker :: CoreBndr -> CoreExpr -> CoreSupplyM [(Var, CoreExpr)]
loopbreaker n b = do
  u <- getUniq
  let info1 = idInfo n
      info1' = setInlinePragInfo info1 alwaysInlinePragma
      n' = mkLocalVar
             (idDetails n)
             (mkInternalName u (occName n) noSrcSpan)
             (idType n)
         $ modifyOccInfo setIsLoopbreaker
         $ setInlinePragInfo vanillaIdInfo noInlinePragma

  let foo =  [ ( lazySetIdInfo n $ modifyOccInfo clearIsLoopbreaker info1'
               , replace n n' b
               )

             , ( n', Var n)
             ]
  pprTraceM "loop breaker for " $ ppr $ Rec $ foo
  pure foo


-- TODO(sandy): Make this only break loops in functions whose type ends in `Sem
-- * * -> Sem * *` for wildcards `*`
loopbreakBinds
    :: Bind CoreBndr
    -> CoreSupplyM (Bind CoreBndr)
loopbreakBinds nr@(NonRec n b)
  | containsName n b = Rec <$> loopbreaker n b
  | otherwise        = pure nr
loopbreakBinds (Rec bs) = fmap (Rec . join) . for bs $ \(n, b) ->
  case containsName n b of
    False -> pure [(n, b)]
    True  -> loopbreaker n b

