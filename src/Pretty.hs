{-# Language OverloadedStrings #-}
module Pretty where

import Core
import Data.Either (either)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (putDoc)

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

putDatabase :: Database -> IO ()
putDatabase = putDoc . prettyDatabase

putRelation :: Relation -> IO ()
putRelation = putDoc . prettyRelation

putQuery :: [Predicate] -> IO ()
putQuery ps = putDoc $ sep $ punctuate comma (pretty <$> ps)