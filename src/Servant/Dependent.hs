{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module: Servant.Dependent
-- Description: DepReqBody combinator
-- Copyright: (c) NoRedInk, 2025
-- License: BSD-3-Clause
-- Maintainer: haskell-open-source@noredink.com
--
--
-- This module provides a combinator `DepReqBody` that allows dependently typed routing based on the /value/ of the request body.
module Servant.Dependent
  ( -- * When to use this combinator
    -- $whenToUse

    -- * How to use this combinator
    -- $howToUse

    -- * Combinator
    DepReqBody,

    -- * Server
    DepServer (..),
    HasDepServer (..),

    -- * Client
    DepClient (..),
    HasDepClient (..),
  )
where

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.ByteString.Lazy as BSL
import Data.Constraint (Dict (..))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Singletons (Sing, type (@@), type (~>))
import Data.Singletons.Sigma (Sigma (..))
import Data.Typeable (Typeable, typeRep)
import Network.HTTP.Types (hContentType)
import qualified Network.Wai as Wai
import Servant.API (MimeRender (mimeRender))
import Servant.API.ContentTypes (AllCTUnrender (canHandleCTypeH), contentType)
import Servant.Client.Core (RunClient, setRequestBodyLBS)
import Servant.Client.Core.HasClient (HasClient (..))
import Servant.Server (ErrorFormatters, HasContextEntry (getContextEntry), HasServer (..), ServerT, err415, notFoundErrorFormatter)
import Servant.Server.Internal.Delayed (Delayed (..), emptyDelayed, runDelayed)
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFail, delayedFailFatal, withRequest)
import Servant.Server.Internal.ErrorFormatter (MkContextWithErrorFormatter, bodyParserErrorFormatter, mkContextWithErrorFormatter)
import Servant.Server.Internal.RouteResult (RouteResult (..))
import Servant.Server.Internal.Router (leafRouter, runRouterEnv)

-- | A variant of `Servant.API.ReqBody` that is dependently typed based on the value of the body.
--
-- Example:
--
-- > -- GET /lookup
-- > data MkBody :: MediaType ~> Type
-- > data MkResponse :: MediaType ~> Type
-- >
-- > type MyApi = "lookup" :> DepReqBody '[JSON] MkBody MkResponse
data DepReqBody (contentTypes :: [Type]) (body :: ix ~> Type) (route :: ix ~> Type)

-- | A wrapper around a function that handles routes for all possible @ix@ types
newtype DepServer (body :: ix ~> Type) (route :: ix ~> Type) (m :: Type -> Type)
  = DepServer (forall (a :: ix). Sing a -> body @@ a -> ServerT (route @@ a) m)

-- | A typeclass that captures the `HasServer` instance for all possible routes under the @ix@ types
class HasDepServer (body :: ix ~> Type) (route :: ix ~> Type) where
  hasDepServer :: Proxy body -> Proxy route -> Proxy m -> Sing (a :: ix) -> Dict (HasServer (route @@ a) m)

addDepBodyCheck :: Delayed env server -> DelayedIO c -> (c -> DelayedIO sigma) -> Delayed env (sigma, server)
addDepBodyCheck Delayed {..} newContentD newBodyD =
  Delayed
    { contentD = (,) <$> contentD <*> newContentD,
      bodyD = \(content, c) -> (,) <$> bodyD content <*> newBodyD c,
      serverD = \c p h a (z, v) req -> (v,) <$> serverD c p h a z req,
      ..
    }

runActionM ::
  Delayed env a ->
  env ->
  Wai.Request ->
  (RouteResult Wai.Response -> IO r) ->
  (a -> IO r) ->
  IO r
runActionM action env req respond k =
  runResourceT $
    runDelayed action env req >>= liftIO . go
  where
    go (Fail e) = respond $ Fail e
    go (FailFatal e) = respond $ FailFatal e
    go (Route a) = let !ka = k a in ka

instance
  ( AllCTUnrender contentTypes (Sigma q body),
    HasDepServer body route,
    HasContextEntry (MkContextWithErrorFormatter context) ErrorFormatters,
    Typeable body,
    Typeable route,
    Typeable q,
    Typeable contentTypes
  ) =>
  HasServer (DepReqBody contentTypes body route) context
  where
  type ServerT (DepReqBody contentTypes body route) m = DepServer body route m

  hoistServerWithContext _ pc nt (DepServer f) =
    DepServer
      ( \(s :: Sing a) v ->
          case hasDepServer (Proxy :: Proxy body) (Proxy :: Proxy route) pc s of
            Dict ->
              hoistServerWithContext (Proxy :: Proxy (route @@ a)) pc nt (f s v)
      )

  route Proxy context delayedDepServer = leafRouter router'
    where
      router' env request respond =
        runActionM
          (addDepBodyCheck delayedDepServer ctCheck bodyCheck)
          env
          request
          respond
          ( \((s :: Sing a) :&: v, DepServer subserver) ->
              case hasDepServer (Proxy :: Proxy body) (Proxy :: Proxy route) (Proxy @context) s of
                Dict ->
                  let delayed = emptyDelayed (Route (subserver s v))
                   in runRouterEnv format404 (route (Proxy :: Proxy (route @@ a)) context delayed) env request respond
          )

      format404 = notFoundErrorFormatter . getContextEntry . mkContextWithErrorFormatter $ context

      rep = typeRep (Proxy :: Proxy (DepReqBody contentTypes body route))
      formatError = bodyParserErrorFormatter $ getContextEntry (mkContextWithErrorFormatter context)

      ctCheck = withRequest $ \request -> do
        let contentTypeH =
              fromMaybe "application/octet-stream" $
                lookup hContentType $
                  Wai.requestHeaders request
        case canHandleCTypeH (Proxy :: Proxy contentTypes) (BSL.fromStrict contentTypeH) :: Maybe (BSL.ByteString -> Either String (Sigma q body)) of
          Nothing -> delayedFail err415
          Just f -> return f

      bodyCheck f = withRequest $ \request -> do
        mrqbody <- f <$> liftIO (Wai.lazyRequestBody request)
        case mrqbody of
          Left e -> delayedFailFatal $ formatError rep request e
          Right v -> return v

-- | A wrapper around a function that can be called for any @ix@ type to produce a client
newtype DepClient (body :: ix ~> Type) (route :: ix ~> Type) (m :: Type -> Type)
  = DepClient (forall (a :: ix). Sing a -> body @@ a -> Client m (route @@ a))

-- | A typeclass to capture the `HasClient` instances for all possible @ix@ types
class HasDepClient (body :: ix ~> Type) (route :: ix ~> Type) m where
  hasDepClient :: Proxy body -> Proxy route -> Proxy m -> Sing (a :: ix) -> Dict (HasClient m (route @@ a))

instance
  ( HasDepClient body route m,
    RunClient m,
    MimeRender ct (Sigma q body)
  ) =>
  HasClient m (DepReqBody (ct ': cts) body route)
  where
  type Client m (DepReqBody (ct ': cts) body route) = DepClient body route m

  clientWithRoute pm Proxy req =
    DepClient
      ( \(s :: Sing a) v ->
          case hasDepClient (Proxy :: Proxy body) (Proxy :: Proxy route) pm s of
            Dict ->
              clientWithRoute
                pm
                (Proxy :: Proxy (route @@ a))
                ( let ctProxy = Proxy :: Proxy ct
                   in setRequestBodyLBS
                        (mimeRender ctProxy ((:&:) @_ @body @a s v))
                        -- We use first contentType from the Accept list
                        (contentType ctProxy)
                        req
                )
      )

  hoistClientMonad pm _ f (DepClient run) =
    DepClient
      ( \(s :: Sing a) v ->
          case hasDepClient (Proxy :: Proxy body) (Proxy :: Proxy route) pm s of
            Dict ->
              hoistClientMonad pm (Proxy :: Proxy (route @@ a)) f (run s v)
      )

-- $whenToUse
-- Only when you absolutely need to.
--
-- Generally having overlapping routes is the preferred way handle different kinds of responses at the same URI.
-- servant-server does an excellent job of matching overlapping routes and error reporting in general.
-- However, servant-server can only read the body once and therefore will throw a non-recoverable error if it fails to match the body of a route.
-- If your API is /only differentiated by the body/ then you might need `DepReqBody`.
--
-- Only put combinators inside of `DepReqBody` that need to be there.  They will be parsed in a secondary stage of processing compared to combinators outside.
--
-- We would not recommend __not__ designing a new API with dependent typing like this if possible.

-- $howToUse
--
-- Let's imagine we are modeling an endpoint that can lookup values of different types of media:
--
-- > data MediaType
-- >   = Book
-- >   | Movie
--
-- Different media types will have different information we would like to return
--
-- > data BookData = BookData
-- >   { title :: Text, author :: Text }
-- >   deriving (ToJSON, FromJSON)
-- >
-- > data MovieData = MovieData
-- >   { title :: Text, director :: Text }
-- >   deriving (ToJSON, FromJSON)
--
-- We could craft our API to take in the MediaType in the body and return the appropriate type of data in the response.
--
-- > type API = "lookup" :> Capture "identifier" Text :> ReqBody '[JSON] MediaType :> GET '[JSON] (BookData | MovieData)
--
-- How can we pull this off with `DepReqBody`?
--
-- First we will need use the singletons library to create two type constructors:
--
--   (1) The type of the request body
--   (2) The type of the rest of the response
--
-- Both of these construtors will be of kind @ix ~> Type@
--
-- > data Body (mediaType :: MediaType) = Body { identifier :: Int }
-- >   deriving (Generic)
-- > type MkBody = TyCon1 Body
--
-- > data MkResponse :: MediaType ~> Type
-- >
-- > type instance Apply MkResponse Book = GET '[JSON] BookData
-- > type instance Apply MkResponse Movie = GET '[JSON] MovieData
--
-- Finally we can model our API as
--
-- > type API = "lookup" :> DepReqBody '[JSON] MkBody MkResponse
--
-- == Serving our API
-- In order to serve our API, we will need to provide a function that can handle any of our possible body types.
--
-- > serve :: DepServer MkBody MkRequest Handler
-- > serve = DepServer (\sing body ->
-- >   case sing of
-- >     SBook ->
-- >       pure $ BookData { title = "Catcher in the Rye", author = "J. D. Salinger" }
-- >     SMovie ->
-- >       pure $ MovieData { title = "Interstellar", director = "Christopher Nolan" }
-- > )
--
-- We also need to add a `Data.Aeson.FromJSON` instance for our body type:
--
-- > instance FromJSON (Sigma MediaType MkBody) where
-- >  parseJSON v =
-- >    Aeson.withObject
-- >      "MediaType"
-- >      ( \keyMap ->
-- >          case KeyMap.lookup "mediaType" keyMap of
-- >            Just (Aeson.String "book") ->
-- >              fmap (SBook :&:) (Aeson.genericParseJSON Aeson.defaultOptions v)
-- >            Just (Aeson.String "movie") ->
-- >              fmap (SMovie :&:) (Aeson.genericParseJSON Aeson.defaultOptions v)
-- >            _ ->
-- >              fail "Unrecognized mediaType"
-- >      )
-- >      v
--
-- Finally we need to provide an instance of `HasDepServer` for our response type to capture
-- the remaining `HasServer` routing information for each possible branch of our index type:
--
-- > import Data.Constraint (Dict(..))
-- >
-- > ...
-- >
-- > instance HasDepServer MkBody MkResponse where
-- >   hasDepServer _ _ _ s =
-- >     case s of
-- >       SBook -> Dict
-- >       SMovie -> Dict
--
-- == Making a Client for our API
-- The result of calling `Servant.Client.client` on an API terminating in `DepReqBody` will be a function that you can call with an appropriate singleton type to make the request.
--
-- > lookupBook :: Body Book -> ClientM BookData
-- > lookupBook body = case client (Proxy :: Proxy API) of
-- >   DepClient f ->
-- >     f SBook body
-- >
-- > lookupMovie :: Body Movie -> ClientM MovieData
-- > lookupMovie body = case client (Proxy :: Proxy API) of
-- >   DepClient f ->
-- >     f SMovie body
--
-- A more general lookup:
--
-- > lookup :: (SingI mediaType) => Body mediaType -> ClientM (MkResponse @@ mediaType)
-- > lookup body = case client (Proxy :: Proxy API) of
-- >   DepClient f ->
-- >     f (sing @mediaType) body
--
-- We also need to provide a `Data.Aeson.ToJSON` instance for our body type:
--
-- > instance ToJSON (Sigma MediaType MkBody) where
-- >   toJSON (s :&: v) =
-- >     case (s, Aeson.genericToJSON Aeson.defaultOptions v) of
-- >       (SBook, Aeson.Object keyMap) ->
-- >         KeyMap.insert "mediaType" (Aeson.String "book") keyMap
-- >           & Aeson.Object
-- >       (SMovie, Aeson.Object keyMap) ->
-- >         KeyMap.insert "mediaType" (Aeson.String "movie") keyMap
-- >           & Aeson.Object
-- >       (_, x) ->
-- >         x
--
-- And finally we need to provide an instance of `HasDepClient` for our response type to capture
-- the remaining `HasClient` routing information for each possible branch of our index type:
--
-- > import Data.Constraint (Dict(..))
-- >
-- > ...
-- >
-- > instance (RunClient m) => HasDepClient MkBody MkResponse m where
-- >   hasDepClient _ _ _ s =
-- >     case s of
-- >       SBook -> Dict
-- >       SMovie -> Dict
--
-- == Further Reading
--
-- * An excellent introduction to the singletons library: https://blog.jle.im/entries/series/+introduction-to-singletons.html
-- * The blog post that orignally implemented this for servant server: https://well-typed.com/blog/2015/12/dependently-typed-servers/