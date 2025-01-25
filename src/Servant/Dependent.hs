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

module Servant.Dependent where

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

data DepReqBody (contentTypes :: [Type]) (body :: ix ~> Type) (route :: ix ~> Type)

newtype DepServer (body :: ix ~> Type) (route :: ix ~> Type) (m :: Type -> Type)
  = DepServer (forall (a :: ix). Sing a -> body @@ a -> ServerT (route @@ a) m)

class HasDepServer (body :: ix ~> Type) (route :: ix ~> Type) where
  hasDepServer :: Proxy body -> Proxy route -> Proxy m -> Sing (a :: ix) -> Dict (HasServer (route @@ a) m)

{- While we are building up a list of checks for route matching, we are also building a function to call -}

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

newtype DepClient (body :: ix ~> Type) (route :: ix ~> Type) (m :: Type -> Type)
  = DepClient (forall (a :: ix). Sing a -> body @@ a -> Client m (route @@ a))

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