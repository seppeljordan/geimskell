{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.QuickCheck

return []
runTests = $quickCheckAll

main = runTests
