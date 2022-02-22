{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Test combinators for PathPiece
module Pact.Web.Server.PathPiece where

import Data.GenValidity
import Data.Typeable
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity.Utils
import Yesod

pathPieceSpec ::
  forall a.
  (Show a, Eq a, Typeable a, GenValid a, PathPiece a) =>
  Spec
pathPieceSpec = pathPieceSpecOnGen (genValid @a) "valid" shrinkValid

pathPieceSpecOnGen ::
  forall a.
  (Show a, Eq a, Typeable a, PathPiece a) =>
  Gen a ->
  String ->
  (a -> [a]) ->
  Spec
pathPieceSpecOnGen gen genname s =
  parallel $ do
    describe ("PathPieceField " ++ name ++ " (" ++ genname ++ ")") $ do
      describe ("fromPathPiece :: Text -> Maybe " ++ name) $
        it testDescription $ pathPieceTest gen s
  where
    name = nameOf @a
    testDescription =
      unwords
        [ "ensures that toPathPiece and fromPathPiece are inverses for",
          "\"" ++ genname,
          name ++ "\"" ++ "'s"
        ]

pathPieceTest ::
  (Show a, Eq a, PathPiece a) => Gen a -> (a -> [a]) -> Property
pathPieceTest gen s =
  forAllShrink gen s $ \(a :: a) ->
    let encoded = toPathPiece a
        errOrDecoded = fromPathPiece encoded
     in case errOrDecoded of
          Nothing ->
            expectationFailure $
              unlines
                [ "Decoding failed with an error instead of decoding to",
                  show a,
                  "'encode' encoded it to the pathPiece",
                  show encoded
                ]
          Just decoded ->
            let ctx =
                  unlines
                    [ "Decoding succeeded, but the decoded value",
                      show decoded,
                      "differs from expected decoded value",
                      show a,
                      "'encode' encoded it to the pathPiece",
                      show encoded
                    ]
             in context ctx $ decoded `shouldBe` a
