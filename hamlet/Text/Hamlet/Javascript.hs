{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Hamlet.Javascript where

import Text.Hamlet.Parse
import Text.Shakespeare.Base (Deref(..), Ident(..), readUtf8File)
import qualified Data.Text.Lazy as TL
import Data.Monoid (mconcat)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

jhamlet :: QuasiQuoter
jhamlet = QuasiQuoter { quoteExp = jhamletFromString "render" "context" }

jhamletFromString :: String -> String -> String -> Q Exp
jhamletFromString functionName contextName s = case parseDoc settings s of
    Error s' -> error s'
    Ok d -> wrapFunction functionName contextName $ docsToExp contextName d
  where settings = debugHamletSettings -- TODO

jhamletFile :: FilePath -> String -> String -> Q Exp
jhamletFile fp functionName contextName = do
#ifdef GHC_7_4
    qAddDependentFile fp
#endif
    contents <- fmap TL.unpack $ qRunIO $ readUtf8File fp
    jhamletFromString functionName contextName contents

wrapFunction :: String -> String -> Q Exp -> Q Exp
wrapFunction functionName contextName js = [|"function " ++ functionName ++ "(" ++ contextName ++ ") {" ++ $js ++ "}"|]

docsToExp :: String -> [Doc] -> Q Exp
docsToExp contextName docs = do
  exps <- mapM (docToExp contextName) docs
  case exps of
    [] -> [|return ()|]
    [x] -> return x
    _ -> [|mconcat $(return $ ListE exps)|]

docToExp :: String -> Doc -> Q Exp
docToExp contextName (DocContent c) = contentToExp contextName c

contentToExp :: String -> Content -> Q Exp
contentToExp _ (ContentRaw s) = [|jsWriteLit s|]
contentToExp contextName (ContentVar d) = [|jsWrite $(derefToExp contextName d)|]

derefToExp :: String -> Deref -> Q Exp
derefToExp contextName (DerefIdent (Ident i)) = [|JsIdent $ contextName ++ "." ++ i|]
derefToExp _ (DerefIntegral i) = [|toJsLit i|]
derefToExp _ (DerefString s) = [|toJsLit s|]


data JsLit = JsString { unJsString :: String }
           | JsNum { unJsNum :: Float }
newtype JsIdent = JsIdent { unJsIdent :: String }

class Js j where
  renderJs :: j -> String

instance Js JsLit where
  renderJs (JsString s) = jsQuote $ escapearoo s
  renderJs (JsNum n) = show n

instance Js JsIdent where
  renderJs (JsIdent i) = i


-- TODO probably shouldn't write my own escaping function
escapearoo :: String -> String
escapearoo = foldr1 (.) replacers
  where
    replacers = map (uncurry replace) replacements
    replacements = [
        ('"', "\\\"")
      , ('\n', "\\n")
      ]

replace :: Eq a => a -> [a] -> [a] -> [a]
replace char replacement string = reverse $ go char (reverse replacement) string []
  where
    go _ _ [] accum = accum
    go c rep (i : is) accum | c == i    = go c rep is $ rep ++ accum
                            | otherwise = go c rep is $ i : accum

jsQuote :: String -> String
jsQuote = ('"' :) . (++ "\"")

class ToJsLit a where
  toJsLit :: a -> JsLit

  jsWriteLit :: a -> String
  jsWriteLit = jsWrite . toJsLit

instance ToJsLit [Char] where
  toJsLit = JsString

instance ToJsLit Integer where
  toJsLit = JsNum . fromIntegral

jsWrite :: Js j => j -> String
jsWrite = wrapDocWrite . renderJs
  where wrapDocWrite = ("document.write(" ++) . (++ ");")
