module Domain.Repository.UrlRepository
    ( UrlRepository (..)
    ) where

import           Data.Map          (Map)

import           Domain.Entity.Url (Url, UrlId)

class Monad m => UrlRepository m where
    storeUrl :: Url -> m UrlId
    findUrl :: UrlId -> m (Maybe Url)
    getAllUrls :: m (Map UrlId Url)
