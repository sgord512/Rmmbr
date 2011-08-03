module Main where

import Test.QuickCheck

main = mapM_ verboseCheckResult tests

tests :: [Property]
tests = [property True]


{-- Test that when you run clean, no completed entries remain, and that they are saved to the list of completed things --}

{-- Test that sorting once is the same as sorting twice. --}

{-- Test that when you run done or begin, the specified positions are in fact correctly modified --}