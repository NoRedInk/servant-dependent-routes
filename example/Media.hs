{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Media where

import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, genericParseJSON, genericToJSON)
import Data.Constraint (Dict (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Singletons (Apply, type (~>))
import Data.Singletons.Sigma (Sigma (..))
import Data.Singletons.TH (genSingletons)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Servant.API (Capture, Get, JSON, (:>))
import Servant.Client (ClientM, client)
import Servant.Client.Core (RunClient)
import Servant.Dependent (DepClient (..), DepReqBody, DepServer (DepServer), HasDepClient (..), HasDepServer (..))
import Servant.Server (Application, Handler, serve)

data MediaType
  = Book
  | Movie
  deriving (Generic, FromJSON)

$(genSingletons [''MediaType])

instance FromJSON (Sigma MediaType MkBody) where
  parseJSON v =
    fmap
      ( \mediaType ->
          case mediaType of
            Book ->
              SBook :&: mediaType
            Movie ->
              SMovie :&: mediaType
      )
      (genericParseJSON defaultOptions v)

instance ToJSON (Sigma MediaType MkBody) where
  toJSON (_ :&: v) = genericToJSON defaultOptions v

data BookData = BookData
  { title :: Text,
    author :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

data MovieData = MovieData
  { title :: Text,
    director :: Text
  }
  deriving (Generic, ToJSON, FromJSON)

data MkBody :: MediaType ~> Type

type instance Apply MkBody _ = MediaType

data MkResponse :: MediaType ~> Type

type instance Apply MkResponse 'Book = Get '[JSON] BookData

type instance Apply MkResponse 'Movie = Get '[JSON] MovieData

type API = "lookup" :> Capture "identifier" Text :> DepReqBody '[JSON] MkBody MkResponse

lookupMedia :: Text -> DepServer MkBody MkResponse Handler
lookupMedia _identifier =
  DepServer
    ( \s _ ->
        case s of
          SBook ->
            -- In the real world, use the identifier to look up the book
            pure $
              BookData
                { title = "The Catcher in the Rye",
                  author = "J. D. Salinger"
                }
          SMovie ->
            -- In the real world, use the identifier to look up the movie
            pure $
              MovieData
                { title = "Interstellar",
                  director = "Christopher Nolan"
                }
    )

instance HasDepServer MkBody MkResponse where
  hasDepServer _ _ _ s =
    case s of
      SBook -> Dict
      SMovie -> Dict

instance (RunClient m) => HasDepClient MkBody MkResponse m where
  hasDepClient _ _ _ s =
    case s of
      SBook -> Dict
      SMovie -> Dict

app :: Application
app = serve (Proxy :: Proxy API) lookupMedia

createRequest :: Text -> DepClient MkBody MkResponse ClientM
createRequest = client (Proxy :: Proxy API)

requestBook :: Text -> ClientM BookData
requestBook identifier =
  let (DepClient f) = createRequest identifier
   in f SBook Book

requestMovie :: Text -> ClientM MovieData
requestMovie identifier =
  let (DepClient f) = createRequest identifier
   in f SMovie Movie

main :: IO ()
main = run 8081 app