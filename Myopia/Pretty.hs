module Myopia.Pretty where

import           Text.PrettyPrint.Leijen

import           Myopia.AST

(<.>) :: Doc -> Doc -> Doc
(<.>) a b = a <> char ',' <$> b

instance Pretty Expr where
    pretty Z = char 'Z'
    pretty S = char 'S'
    pretty (I i k) = text "I[" <> int i <> char ',' <> integer k <> char ']'
    pretty (C h gs) = text "C" <> parens (group $ align $ pretty h <.> prettyList gs)
    pretty (P g h) = text "P" <> parens (group $ align $ pretty g <.> pretty h)
    pretty (M f) = text "M" <> parens (pretty f)
    pretty (FC f) = text f

    prettyList = sep . punctuate (text ", ") . map pretty

-- vim: set ts=4 sw=4 et
