{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import SimpleJSON


main :: IO ()
main = do
    putStrLn (show $ toJValue (10::Integer))
