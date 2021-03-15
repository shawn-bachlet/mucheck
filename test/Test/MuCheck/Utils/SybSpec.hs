module Test.MuCheck.Utils.SybSpec where

import Control.Monad (MonadPlus, mplus, mzero)
import Data.Generics (Data, GenericQ, Typeable, listify, mkMp, mkQ)
import Language.Haskell.Exts
import System.Random
import Test.Hspec
import Test.MuCheck.MuOp (MuOp, mkMpMuOp)
import qualified Test.MuCheck.Utils.Syb as S

main :: IO ()
main = hspec spec

srcLoc = SrcLoc "<unknown>.hs" 15 1

m1 a b = Match srcLoc
           (Ident srcLoc a)
           [PApp srcLoc (UnQual srcLoc (Ident srcLoc b)) [],PLit srcLoc (Signless srcLoc) (Int srcLoc 0 "0")]
           (UnGuardedRhs srcLoc (Lit srcLoc (Int srcLoc 1 "1")))
           Nothing
           --(BDecls [])

replM :: MonadPlus m => Name SrcLoc -> m (Name SrcLoc)
replM (Ident l "x") = return $ Ident l "y"
replM t = mzero


spec :: Spec
spec = do
  describe "once" $ do
    it "apply a function once on exp" $ do
      (S.once (mkMp replM) (FunBind srcLoc [m1 "y" "x"]) :: Maybe (Decl SrcLoc)) `shouldBe` Just (FunBind srcLoc [m1 "y" "y"] :: (Decl SrcLoc))
    it "apply a function just once" $ do
      (S.once (mkMp replM) (FunBind srcLoc [m1 "x" "x"]) :: Maybe (Decl SrcLoc)) `shouldBe` Just (FunBind srcLoc [m1 "y" "x"] :: (Decl SrcLoc))
    it "apply a function just once if possible" $ do
      (S.once (mkMp replM) (FunBind srcLoc [m1 "y" "y"]) :: Maybe (Decl SrcLoc)) `shouldBe` Nothing
    it "should return all possibilities" $ do
      (S.once (mkMp replM) (FunBind srcLoc [m1 "x" "x"]) :: [(Decl SrcLoc)]) `shouldBe`  ([FunBind srcLoc [m1 "y" "x"], FunBind srcLoc [m1 "x" "y"]] :: [(Decl SrcLoc)])

