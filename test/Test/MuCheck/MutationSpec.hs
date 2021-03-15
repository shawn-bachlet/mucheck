{-# LANGUAGE QuasiQuotes #-}
module Test.MuCheck.MutationSpec where

import Control.Monad (MonadPlus, mplus, mzero)
import Data.Generics (Data, GenericQ, Typeable, listify, mkMp, mkQ)
import Data.String.Here (i)
import System.Random
import Test.Hspec
import Test.MuCheck.MuOp ((==>), MuOp, mkMpMuOp)
import Test.MuCheck.Mutation
import qualified Test.MuCheck.MutationSpec.Helpers as H
import qualified Test.MuCheck.Utils.Helpers

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "selectLitOps" $ do
    it "returns binarylit muops" $ do
      let text = [i|
module Prop where
import Test.QuickCheck

myFn x = if x == 1 then True else False
|]
          res =  [[i|{
1
} ==> {
2
}|],[i|{
1
} ==> {
0
}|],[i|{
1
} ==> {
1
}|] ]

      map show (selectLitOps (getASTFromStr text)) `shouldBe` res

  describe "selectBLitOps" $ do
    it "returns binarylit muops" $ do
      let text = [i|
module Prop where
import Test.QuickCheck

myFn x = if x == 1 then True else False
|]
          res =  [[i|{
True
} ==> {
False
}|],[i|{
False
} ==> {
True
}|] ]
      map show (selectBLitOps (getASTFromStr text)) `shouldBe` res


  describe "selectIfElseBoolNegOps" $ do
    it "returns ifelse muops" $ do
      let text = [i|
module Prop where
import Test.QuickCheck

myFn x = if x == 1 then True else False
|]
          res =  [[i|{
if x == 1 then True else False
} ==> {
if x == 1 then False else True
}|]]
      map show (selectIfElseBoolNegOps (getASTFromStr text)) `shouldBe` res

  describe "selectGuardedBoolNegOps" $ do
    it "returns guardedboolean muops" $ do
      let text = [i|
module Prop where
import Test.QuickCheck

myFn x | x == 1 = True
myFn   | otherwise = False
|]
          res =  [[i|{
| x == 1 = True
} ==> {
| not (x == 1) = True
}|]]
      map show (selectGuardedBoolNegOps (getASTFromStr text)) `shouldBe` res

  describe "selectFnMatches" $ do
    it "returns fn muops" $ do
      let text = [i|
module Prop where
import Test.QuickCheck

myFn [] = 0
myFn (x:xs) = 1 + myFn xs
|]
          res =  [[i|{
myFn [] = 0
myFn (x : xs) = 1 + myFn xs
} ==> {
myFn (x : xs) = 1 + myFn xs
myFn [] = 0
}|],[i|{
myFn [] = 0
myFn (x : xs) = 1 + myFn xs
} ==> {
myFn [] = 0
}|],[i|{
myFn [] = 0
myFn (x : xs) = 1 + myFn xs
} ==> {
myFn (x : xs) = 1 + myFn xs
}|]]
      map show (selectFnMatches (getASTFromStr text)) `shouldBe` res


{-
  describe "selectGuardedBoolNegOps" $ do
    it "returns relevant function guard ordering mutators" $ do
      let text = "myFn x | x == 1 = True\nmyFn   | otherwise = False\n"
      selectGuardedBoolNegOps (getASTFromStr text) `shouldBe` [
       GuardedRhs (SrcLoc "<unknown>.hs" 1 8)
                  [Qualifier (InfixApp (Var (UnQual (Ident "x"))) (QVarOp (UnQual (Symbol "=="))) (Lit (Int 1)))]
                  (Con (UnQual (Ident "True")))
        ==>
        GuardedRhs (SrcLoc "<unknown>.hs" 1 8)
                  [Qualifier (App (Var (UnQual (Ident "not"))) (InfixApp (Var (UnQual (Ident "x"))) (QVarOp (UnQual (Symbol "=="))) (Lit (Int 1))))]
                  (Con (UnQual (Ident "True"))),
        GuardedRhs (SrcLoc "<unknown>.hs" 2 8)
                  [Qualifier (Var (UnQual (Ident "otherwise")))]
                  (Con (UnQual (Ident "False")))
        ==>
        GuardedRhs (SrcLoc "<unknown>.hs" 2 8)
                   [Qualifier (Var (UnQual (Ident "otherwise")))]
                   (Con (UnQual (Ident "False")))
        ]


  describe "selectIfElseBoolNegOps" $ do
    it "returns relevant ifElse mutators" $ do
      let text = "myFn x = if True then 1 else 0\n"
      selectIfElseBoolNegOps (getASTFromStr text) `shouldBe` [
        If (Con (UnQual (Ident "True"))) (Lit (Int 1)) (Lit (Int 0))
        ==>
        If (Con (UnQual (Ident "True"))) (Lit (Int 0)) (Lit (Int 1))]

  describe "selectLitOps" $ do
    it "returns relevant literal mutators" $ do
      let text = "myFn x = if True then 1 else 0\n"
      selectLitOps (getASTFromStr text) `shouldBe` [
        Int 1 ==> Int 2,
        Int 1 ==> Int 0,
        Int 1 ==> Int 1,
        Int 0 ==> Int 1,
        Int 0 ==> Int (-1),
        Int 0 ==> Int 0]

  describe "selectBLitOps" $ do
    it "returns relevant literal mutators" $ do
      let text = "myFn x = if True then 1 else 0\n"
      selectBLitOps (getASTFromStr text) `shouldBe` [
        Ident "True" ==> Ident "False"]

  describe "selectFnMatches" $ do
    it "returns mutated function definition by removing one of the patterns" $ do
      let text = "myFn True = 1\nmyFn False = 0\n"
      selectFnMatches (getASTFromStr text) `shouldBe` [
        FunBind [Match (SrcLoc "<unknown>.hs" 1 1)
                       (Ident "myFn")
                       [PApp (UnQual (Ident "True")) []]
                       Nothing
                       (UnGuardedRhs (Lit (Int 1)))
                       (BDecls []),
                Match (SrcLoc "<unknown>.hs" 2 1)
                      (Ident "myFn")
                      [PApp (UnQual (Ident "False")) []]
                      Nothing
                      (UnGuardedRhs (Lit (Int 0)))
                      (BDecls [])]
        ==>
        FunBind [Match (SrcLoc "<unknown>.hs" 1 1)
                      (Ident "myFn")
                      [PApp (UnQual (Ident "True")) []]
                      Nothing
                      (UnGuardedRhs (Lit (Int 1)))
                      (BDecls []),
                Match (SrcLoc "<unknown>.hs" 2 1)
                      (Ident "myFn")
                      [PApp (UnQual (Ident "False")) []]
                      Nothing
                      (UnGuardedRhs (Lit (Int 0)))
                      (BDecls [])] ]
  describe "mutate" $ do
    it "returns relevant mutations" $ do
      let text = "myFn x = if True then 1 else 0\n"
      mutate (Int 1 ==> Int 2) (getASTFromStr text) `shouldBe`[
        Module
          (SrcLoc "<unknown>.hs" 1 1)
          (ModuleName "Main")
          []
          Nothing
          (Just [EVar NoNamespace (UnQual (Ident "main"))])
          []
          [FunBind
            [Match (SrcLoc "<unknown>.hs" 1 1)
                   (Ident "myFn")
                   [PVar (Ident "x")]
                   Nothing
                   (UnGuardedRhs (If (Con (UnQual (Ident "True"))) (Lit (Int 2)) (Lit (Int 0))))
                   (BDecls [])]] ]

  describe "mutatesN" $ do
    it "returns relevant mutations" $ do
      let text = "myFn x = if True then 1 else 0\n"
      mutatesN [Int 1 ==> Int 2] (getASTFromStr text) 1 `shouldBe`[
        Module
          (SrcLoc "<unknown>.hs" 1 1)
          (ModuleName "Main")
          []
          Nothing
          (Just [EVar NoNamespace (UnQual (Ident "main"))])
          []
          [FunBind
            [Match (SrcLoc "<unknown>.hs" 1 1)
                   (Ident "myFn")
                   [PVar (Ident "x")]
                   Nothing
                   (UnGuardedRhs (If (Con (UnQual (Ident "True"))) (Lit (Int 2)) (Lit (Int 0))))
                   (BDecls [])]] ]
    it "returns all relevant mutations" $ do
      let text = "myFn x = if 1 then 1 else 0\n"
      mutatesN [Int 1 ==> Int 2] (getASTFromStr text) 1 `shouldBe`[
        Module
          (SrcLoc "<unknown>.hs" 1 1)
          (ModuleName "Main")
          []
          Nothing
          (Just [EVar NoNamespace (UnQual (Ident "main"))])
          []
          [FunBind [Match (SrcLoc "<unknown>.hs" 1 1) (Ident "myFn") [PVar (Ident "x")] Nothing (UnGuardedRhs (If (Lit (Int 2)) (Lit (Int 1)) (Lit (Int 0)))) (BDecls [])]],
        Module
          (SrcLoc "<unknown>.hs" 1 1)
          (ModuleName "Main")
          []
          Nothing
          (Just [EVar NoNamespace (UnQual (Ident "main"))])
          []
          [FunBind [Match (SrcLoc "<unknown>.hs" 1 1) (Ident "myFn") [PVar (Ident "x")] Nothing (UnGuardedRhs (If (Lit (Int 1)) (Lit (Int 2)) (Lit (Int 0)))) (BDecls [])]] ]
-}
