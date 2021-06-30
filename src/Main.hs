{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module Main where

import Data.ByteString.Char8
import Data.ByteString.Lazy.Char8

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)
import Lucid

import Data.Text (Text)
import Data.IORef
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO, MonadIO)

import Data.Semigroup ((<>))
import Servant.API

-- -------------- Show notes -----------------
-- app :: SpockM () () () ()
-- app = return ()

-- type Server a = SpockM () () () a

-- app :: Server ()
-- app = return ()

-- Defining Routes
-- app :: Server ()
-- app = get root (text "Hello!")

-- Rendering HTML
-- app :: Server ()
-- app = get root (html "<h1>Hello!</h1>")

-- Lucid
-- app :: Server ()
-- app = get root $ lucid $ do
--     h1_ "Hello!"
--     p_ "How are you today?"

-- main :: IO ()
-- main = do
--     -- let app = return ()
--     cfg <- defaultSpockCfg () PCNoDatabase ()
--     runSpock 8080 (spock cfg app)

-- ----------- Keeping Notes with Server State -------------------

-- newtype ServerState = ServerState { notes :: IORef [Note] }
-- data Note = Note {
--     author      :: Text,
--     contents    :: Text
-- }

-- type Server a = SpockM () () ServerState a

-- app :: Server ()
-- app = get root $ do
--     notes' <- getState >>= (liftIO . readIORef . notes)
--     lucid $ do
--         h1_ "Notes"
--         ul_ $ forM_ notes' $ \note -> li_ $ do
--             toHtml (author note)
--             ": "
--             toHtml (contents note)


-- main :: IO ()
-- main = do
--     st <- ServerState <$> newIORef [
--         Note "Alice" "Must not forget to walk the dog."
--         , Note "Kaladin" "Are. You. Ready"
--         ]
--     cfg <- defaultSpockCfg () PCNoDatabase st
--     runSpock 8080 (spock cfg app)

-- ----------------------- Posting new notes with a form------------------

newtype ServerState = ServerState { notes :: IORef [Note] }
data Note = Note {
    author      :: Text,
    contents    :: Text
}

type Server a = SpockM () () ServerState a

param' :: forall p (m :: * -> *) ctx.
  (FromHttpApiData p, MonadIO m)
  => Text
  -> ActionCtxT ctx m p

app :: Server ()
app = do 
    get root $ do
        notes' <- getState >>= (liftIO . readIORef . notes)
        lucid $ do
            h1_ "Notes"
            ul_ $ forM_ notes' $ \note -> li_ $ do
                toHtml (author note)
                ": "
                toHtml (contents note)
    post root $ do
        author <- param' "author"
        contents <- param' "contents"
        notesRef <- notes <$> getState
        liftIO $ atomicModifyIORef' notesRef $ \notes ->
            (notes <> [Note author contents], ())
        redirect "/"


main :: IO ()
main = do
    st <- ServerState <$> newIORef [
        Note "Alice" "Must not forget to walk the dog."
        , Note "Kaladin" "Try and be happy"
        ]
    cfg <- defaultSpockCfg () PCNoDatabase st
    runSpock 8080 (spock cfg app)