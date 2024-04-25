{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Orgmode.Internal.Types (OrgDocumentF (..), HeadingF (..), ParagraphF (..), ListItemF (..), BlockName (..)) where

import GHC.Generics ()

data OrgDocumentF (f :: Type -> Type) a
  = OrgDocHeadingF (f (HeadingF f)) a
  | OrgDocParagraphF (f (ParagraphF f)) a
  | OrgDocListItemF (f (ListItemF f)) a
  | OrgDocEmptyLineF (f ()) a
  | OrgDocEOFF (f ())
  deriving stock (Typeable, Generic, Functor)

deriving stock instance (Eq a, forall x. (Eq x) => Eq (f x)) => Eq (OrgDocumentF f a)

deriving stock instance (Show a, forall x. (Show x) => Show (f x)) => Show (OrgDocumentF f a)

data ListItemF f = ListItemF
  deriving stock (Eq, Show, Typeable, Generic)

data HeadingF (f :: Type -> Type) = HeadingF
  { level :: f Int
  , keyword :: f (Maybe Text)
  , text :: f Text
  , tags :: [f Text]
  }
  deriving stock (Typeable, Generic)

deriving stock instance (forall x. (Eq x) => Eq (f x)) => Eq (HeadingF f)

deriving stock instance (forall x. (Show x) => Show (f x)) => Show (HeadingF f)

data ParagraphF f = ParagraphF
  deriving stock (Eq, Show, Typeable, Generic)

-- | Generate block name for json and other serializer
class BlockName b where
  blockName :: Proxy b -> Text

instance BlockName (OrgDocumentF f a) where
  blockName _ = "document"

instance BlockName (HeadingF f) where
  blockName _ = "heading"

instance BlockName (ParagraphF f) where
  blockName _ = "paragraph"

instance BlockName (ListItemF f) where
  blockName _ = "list-item"
