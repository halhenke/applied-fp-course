{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)

import           FirstApp.Types           (ContentType (..), Error (..),
                                           RqType (..), getCommentText,
                                           getTopic, mkCommentText, mkTopic,
                                           renderContentType)

-- --------------------------------------------
-- - Don't start here, go to FirstApp.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
-- mkResponse s cType =
--   responseLBS s [(hContentType, renderContentType cType)]
-- mkResponse status200 c bs = resp200 c bs
-- mkResponse status404 c bs = resp404 c bs
-- mkResponse status400 c bs = resp400 c bs
-- mkResponse s c bs = case (statusCode s) of
mkResponse s c bs = case fromEnum s of
  200 -> resp200 c bs
  404 -> resp404 c bs
  400 -> resp400 c bs
  _   -> mkErrorResponse WeirdStatus

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 ct bs = case ct of
  PlainText  -> responseLBS (toEnum 200) [] bs
  -- Right -> that
  -- error "resp200 not implemented"

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 _ bs = responseLBS (toEnum 404) [] bs
  -- error "resp404 not implemented"

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 _ bs = responseLBS (toEnum 400) [] bs
  -- error "resp400 not implemented"

resp500
  :: ContentType
  -> LBS.ByteString
  -> Response
resp500 _ bs = responseLBS (toEnum 500) [] bs
  -- error "resp400 not implemented"

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest t bs = AddRq <$> mkTopic t <*> com where
  com = mkCommentText $ decodeUtf8 $ LBS.toStrict bs
-- mkAddRequest t bs = do
--   top <- (mkTopic t)
--   com <- (mkCommentText $ decodeUtf8 $ LBS.toStrict bs)
--   return $ AddRq top com

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest :: Text -> Either Error RqType
mkViewRequest t = ViewRq <$> mkTopic t
-- mkViewRequest t = do
--   top <- mkTopic t
--   return $ ViewRq top

mkListRequest
  :: Either Error RqType
mkListRequest =
  error "mkListRequest not implemented"

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse UnknownRouteError = resp404 PlainText "You are fucking crazy - where are you trying to go?"
mkErrorResponse WeirdStatus = resp500 PlainText "Actually this may be my fault..."
mkErrorResponse EmptyCommentText = resp500 PlainText "No Comment Provided..."
mkErrorResponse EmptyTopic = resp500 PlainText "No Topic Provided..."
-- mkErrorResponse =
--   error "mkErrorResponse not implemented"

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest r = do
  body <- strictRequestBody r
  return $ case pathInfo r of
    ["add", t]  -> mkAddRequest t body
    ["view", t] -> mkViewRequest t
    ["list"]    -> mkListRequest

  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
-- mkRequest =
--   error "mkRequest not implemented"

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest rt = case rt of
  AddRq t ct -> Right $ resp200 PlainText $ LBS.fromStrict $ encodeUtf8 $ getCommentText ct
  ViewRq t   -> Right $ resp200 PlainText $ LBS.fromStrict $ encodeUtf8 $ getTopic t
  ListRq     -> Right $ resp200 PlainText "lots and lots of stuff...."
  -- error "handleRequest not implemented"

-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app
  :: Application
app =
  error "app not reimplemented"

runApp :: IO ()
runApp = run 3000 app
