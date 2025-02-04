{-# LANGUAGE FlexibleContexts #-}

module Util where

import Control.Monad.Except

guardE :: (MonadError String f, Show a) => a -> Bool -> f ()
guardE _ True = pure ()
guardE err False = throwError $ show err
