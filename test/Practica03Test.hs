module Main (main) where

import Test.Hspec
import Practica03
import Test.Hspec.Runner
import System.Timeout (timeout)
import Control.DeepSeq (deepseq)

main :: IO ()
main = hspecWith defaultConfig specs


specs :: Spec
specs = do

    let formFNN = Not (And (Impl (Var "p") (Var "r")) (Var "s")) --Pasa test
    let formFNC = Or (Or (And (Var "p") (Var "r")) (Var "s")) (Var "q") --Genera resultado en pantalla
    let formTt =  Syss (Not (Or (Var "p") (Var "q"))) (And (Not (Var "p")) (Not (Var "q"))) --Genera error de recursion
    let formCr = Syss (Or (Var "p") (Var "q")) (And (Not (Var "p")) (Not (Var "q"))) --Pasa test
    let formNotEnd = Impl (Impl (Var "q") (Var "p")) (Impl (Impl (Not (Var "q")) (Var "p")) (Var "p")) -- Genera resultado en pantalla 

    describe "Tests Forma Normal Negativa" $ do
        it "Fórmula Tt" $ do
            fnn formTt `shouldBe` And (Or (Or (Var "p") (Var "q")) (And (Not (Var "p")) (Not (Var "q")))) (Or (Or (Var "p") (Var "q")) (And (Not (Var "p")) (Not (Var "q")))) --Pasa test 
        it "Fórmula Cr" $ do 
            fnn formCr `shouldBe` And (Or (And (Not (Var "p")) (Not (Var "q"))) (And (Not (Var "p")) (Not (Var "q")))) (Or (Or (Var "p") (Var "q")) (Or (Var "p") (Var "q"))) --Pasa test
        it "Fórmula Truco" $ do 
            fnn formFNN `shouldBe` Or (And (Var "p") (Not (Var "r"))) (Not (Var "s"))
        --Pasa test pero las arroja en orden distinto
    describe "Tests Forma Normal Conjuntiva" $ do
        it "Fórmula Tt" $ do
            fnc formTt `shouldBe` And (And (Or (Or (Var "p") (Var "q")) (Not (Var "p"))) (Or (Or (Var "p") (Var "q")) (Not (Var "q")))) (And (Or (Or (Var "p") (Var "q")) (Not (Var "p"))) (Or (Or (Var "p") (Var "q")) (Not (Var "q"))))
        it "Fórmula Cr" $ do 
            fnc formCr `shouldBe` And (And (And (Or (Not (Var "p")) (Not (Var "p"))) (Or (Not (Var "q")) (Not (Var "p")))) (And (Or (Not (Var "p")) (Not (Var "q"))) (Or (Not (Var "q")) (Not (Var "q"))))) (Or (Or (Var "p") (Var "q")) (Or (Var "p") (Var "q")))
        it "Fórmula Truco" $ do 
            fnc formFNC `shouldBe` And (Or (Or (Var "p") (Var "s")) (Var "q")) (Or (Or (Var "r") (Var "s")) (Var "q"))

    describe "Tests clausulas" $ do 
        it "Clausulas del Tt" $ do 
            clausulas (And (And (Or (Or (Var "p") (Var "q")) (Not (Var "p"))) (Or (Or (Var "p") (Var "q")) (Not (Var "q")))) (And (Or (Or (Var "p") (Var "q")) (Not (Var "p"))) (Or (Or (Var "p") (Var "q")) (Not (Var "q"))))) `shouldMatchList` [[Var "p",Var "q",Not (Var "p")],[Var "p",Var "q",Not (Var "q")],[Var "p",Var "q",Not (Var "p")],[Var "p",Var "q",Not (Var "q")]]
        it "Clausulas del Cr" $ do 
            clausulas (And (And (And (Or (Not (Var "p")) (Not (Var "p"))) (Or (Not (Var "q")) (Not (Var "p")))) (And (Or (Not (Var "p")) (Not (Var "q"))) (Or (Not (Var "q")) (Not (Var "q"))))) (Or (Or (Var "p") (Var "q")) (Or (Var "p") (Var "q")))) `shouldMatchList` [[Not (Var "p")],[Not (Var "q"),Not (Var "p")],[Not (Var "p"),Not (Var "q")],[Not (Var "q")],[Var "p",Var "q"]]
        it "Clausulas del Truco" $ do 
            clausulas (And (Or (Or (Var "p") (Var "s")) (Var "q")) (Or (Or (Var "r") (Var "s")) (Var "q"))) `shouldMatchList` [[Var "p",Var "s",Var "q"],[Var "r",Var "s",Var "q"]]

    describe "Tests resolucion" $ do
        it "Clausulas del Tt" $ do
            resolucion [Var "p",Var "q",Not (Var "p")] [Var "p",Var "q",Not (Var "q")] `shouldMatchList` [Not (Var "p"),Var "p",Var "q"]
        it "Clausulas del Cr" $ do
            resolucion [Not (Var "q"),Not (Var "p")] [Var "p",Var "q"] `shouldMatchList` [Not (Var "p"),Var "p"]
        it "Clausulas del truco" $ do
            resolucion [Var "p",Var "s",Var "q"] [Var "r",Var "s",Var "q"] `shouldMatchList` [Var "p",Var "r",Var "s",Var "q"]

    describe "Tests hayResolvente" $ do
        it "Clausulas del Tt" $ do
            hayResolvente [Var "p",Var "q",Not (Var "p")] [Var "p",Var "q",Not (Var "q")] `shouldBe` True 
        it "Clausulas del Cr" $ do
            hayResolvente [Not (Var "q"),Not (Var "p")] [Var "p",Var "q"] `shouldBe` True
        it "Clausulas del truco" $ do
            hayResolvente [Var "p",Var "s",Var "q"] [Var "r",Var "s",Var "q"] `shouldBe` False

    describe "Tests saturacion" $ do 
        it "Fórmula Tt" $ do
            saturacion formTt `shouldBe` True
        it "Fórmula Cr" $ do
            saturacion formCr `shouldBe` False
        it "Fórmula Truco" $ do 
            resultado <- timeout 1000000 (saturacion formNotEnd `deepseq` return True)
            resultado `shouldBe` Nothing
