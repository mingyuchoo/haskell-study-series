module Application.UseCase.ShortenUrl
    ( ShortenUrlUseCase (..)
    , shortenUrl
    ) where

import           Domain.Entity.Url               (Url, UrlId)
import           Domain.Repository.UrlRepository (UrlRepository (..))

class Monad m => ShortenUrlUseCase m where
    shortenUrlUC :: Url -> m UrlId

shortenUrl :: UrlRepository m => Url -> m UrlId
shortenUrl = storeUrl
