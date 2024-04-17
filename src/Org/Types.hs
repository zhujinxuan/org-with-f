{-# LANGUAGE QuantifiedConstraints #-}
module Org.Types where

import GHC.Generics ()
import Data.Functor.Classes (Eq1, eq1)

data DocumentF f a
  = DocHeadingF !(f (HeadingF f)) a
  | DocParagraphF !(f (ParagraphF f)) a
  | DocEmptyLineF !(f ()) a
  deriving stock (Typeable, Generic)

instance (Eq1 f, Eq a) => Eq (DocumentF f a) where
  (DocHeadingF x a) == (DocHeadingF y b) = eq1 x y && a == b
  (DocParagraphF x a) == (DocParagraphF y b) = eq1 x y && a == b
  (DocEmptyLineF x a) == (DocEmptyLineF y b) = eq1 x y && a == b
  _ == _ = False


data HeadingF f = HeadingF
  deriving stock (Eq, Show, Typeable, Generic)

data ParagraphF f = ParagraphF
  deriving stock (Eq, Show, Typeable, Generic)


data Drawer f = Drawer
  deriving stock (Eq, Show, Typeable, Generic)

newtype Token f = Token Text deriving stock (Eq, Show, Typeable, Generic)
