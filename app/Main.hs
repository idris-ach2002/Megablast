module Main (main) where

import Lib

afficheMur :: Int -> Hitbox -> IO ()
afficheMur n (MurGauche pts) = print (take n pts)
afficheMur _ _ = putStrLn "Ce n'est pas un mur gauche."

main :: IO ()
main = do
    let mur = mur_dent_scie_Exam
        p1  = Point 1 8
        p2  = Point 5 19

    putStrLn "Test collision (Point x y) (MurGauche ls) :"
    putStrLn $ "collision " ++ show p1 ++ " mur = " ++ show (collision p1 mur)
    putStrLn $ "collision " ++ show p2 ++ " mur = " ++ show (collision p2 mur)
