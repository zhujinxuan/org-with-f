{-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving #-}

module Org.Types where
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics

data DocumentF (f :: * -> * ) (a :: *)
  = DocEOFF (EOFF f)
  | DocHeadingF (f (HeadingF f)) a
  | DocParagraphF (f (ParagraphF f)) a
  | DocBlockF (f (BlockF f)) a
  deriving (Show, Eq, Typeable, Generic)

newtype HeadingF f =
  HeadingF {
  level:: f Int,
  text :: f Text
                            }
  deriving (Show, Eq, Typeable, Generic)

newtype ParagraphF f  = PF Text
  deriving (Show, Eq)


newtype BlockF f  = BlockF Text
  deriving (Show, Eq)


newtype Drawer f  = Drawer Text
  deriving (Show, Eq)

newtype EOFF f  = EOFF (f ())
  deriving (Show, Eq)
