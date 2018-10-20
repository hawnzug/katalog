{-# Language OverloadedStrings #-}
module Katalog.Pretty where

import Katalog.Core
import Data.Either (either)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderLazy)
import Data.Text.Lazy (Text)

instance Pretty Predicate where
  pretty (Predicate name params) = pretty name <> parens (sep p)
    where
      p = punctuate comma $ pretty . either id id <$> params

instance Pretty Clause where
  pretty (Clause head []) = pretty head <+> dot
  pretty (Clause head body) = pretty head <+> ":-" <+> (align $ sep $ punctuate comma (pretty <$> body))

prettyRelation :: Relation -> Doc ann
prettyRelation tuples = vsep ((hsep . map pretty) <$> Set.toList tuples)

prettyDatabase :: Database -> Doc ann
prettyDatabase db = vsep rels
  where
    rels = concatMap f $ Map.assocs db
    f (name, rel) = [pretty name, indent 4 $ prettyRelation rel, emptyDoc]

render = renderLazy . layoutPretty defaultLayoutOptions

renderDatabase :: Database -> Text
renderDatabase = render . prettyDatabase

renderRelation :: Relation -> Text
renderRelation = render . prettyRelation

renderQuery :: [Predicate] -> Text
renderQuery ps = render $ sep $ punctuate comma (pretty <$> ps)
