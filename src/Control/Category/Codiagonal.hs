module Control.Category.Codiagonal where

class Codiagonal c where
  codiagonal :: c (Either a a) a
