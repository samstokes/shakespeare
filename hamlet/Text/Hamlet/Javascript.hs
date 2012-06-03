{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Hamlet.Javascript where

import Text.Hamlet.Parse
import Text.Shakespeare.Base (Deref(..), Ident(..))
import Data.Monoid (mconcat)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

jhamlet :: QuasiQuoter
jhamlet = QuasiQuoter { quoteExp = jhamletFromString }

jhamletFromString :: String -> Q Exp
jhamletFromString s = case parseDoc settings s of
    Error s' -> error s'
    Ok d -> wrapFunction "render" $ docsToExp d
  where settings = debugHamletSettings -- TODO

wrapFunction :: String -> Q Exp -> Q Exp
wrapFunction functionName js = [|"function " ++ functionName ++ "() {" ++ $js ++ "}"|]

docsToExp :: [Doc] -> Q Exp
docsToExp docs = do
  exps <- mapM docToExp docs
  case exps of
    [] -> [|return ()|]
    [x] -> return x
    _ -> [|mconcat $(return $ ListE exps)|]

docToExp :: Doc -> Q Exp
docToExp (DocContent c) = contentToExp c

contentToExp :: Content -> Q Exp
contentToExp (ContentRaw s) = [|jsWriteVal s|]
contentToExp (ContentVar d) = derefToExp d

derefToExp :: Deref -> Q Exp
derefToExp (DerefIntegral i) = [|jsWriteVal i|]
derefToExp (DerefString s) = [|jsWriteVal s|]


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
replace c rep s = reverse $ go c (reverse rep) s []
  where
    go _ _ [] accum = accum
    go c rep (i : is) accum | c == i    = go c rep is $ rep ++ accum
                            | otherwise = go c rep is $ i : accum

jsQuote :: String -> String
jsQuote = ('"' :) . (++ "\"")

class ToJsVal a where
  toJsVal :: a -> String

  jsWriteVal :: a -> String
  jsWriteVal x = "document.write(" ++ toJsVal x ++ ");"

instance ToJsVal [Char] where
  toJsVal = jsQuote . escapearoo

instance ToJsVal Integer where
  toJsVal = show
