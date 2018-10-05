-- Most of test framework is copied from Hakyll project
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

--------------------------------------------------------------------------------
import           Test.Tasty                  (TestTree, testGroup, defaultMain)
import           Test.Tasty.HUnit            (Assertion, testCase, (@=?))

--------------------------------------------------------------------------------
import           Hakyll.Core.Compiler
import           Hakyll.Core.Identifier
import           Hakyll.Core.Provider
import           Hakyll.Core.Store           (Store)
import           Hakyll.Web.Template.Context
import           TestSuite.Util

import           Hakyll.Web.Template.Context.Date


--------------------------------------------------------------------------------
main :: IO ()
main = defaultMain $ testGroup "Hakyll"
    [ testCase "testDateField" testDateField
    ]

--------------------------------------------------------------------------------
testDateField :: Assertion
testDateField = do
    store    <- newTestStore
    provider <- newTestProvider store

    dateFormatted <- testContextDone store provider
      "posts/2018-09-28-fall-prep.md" "modified" $
      formatDateFieldValue "modified" "%Y-%m-%d %H:%M:%S" "%B %e, %Y - %r"
    dateFormatted @=? "September 29, 2018 - 06:58:15 PM"

    cleanTestEnv


--------------------------------------------------------------------------------
testContextDone :: Store -> Provider -> Identifier -> String
                -> Context String -> IO String
testContextDone store provider identifier key context =
    testCompilerDone store provider identifier $ do
        item <- getResourceBody
        cf   <- unContext context key [] item
        case cf of
            StringField str -> return str
            ListField _ _   -> error $
                "Hakyll.Web.Template.Context.Tests.testContextDone: " ++
                "Didn't expect ListField"
