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
import Data.Monoid (mappend, mempty, mconcat)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax


data JHamletOpts = JHamletOpts {
    jhamletFunctionName :: String
  , jhamletContextName :: String
  }

defaultJHamletOpts :: JHamletOpts
defaultJHamletOpts = JHamletOpts "render" "context"


jhamlet :: QuasiQuoter
jhamlet = QuasiQuoter { quoteExp = jhamletFromString defaultJHamletOpts }

jhamletFromString :: JHamletOpts -> String -> Q Exp
jhamletFromString opts s = case parseDoc settings s of
    Error s' -> error s'
    Ok d -> wrapFunction opts $ docsToExp opts d
  where settings = debugHamletSettings -- TODO

jhamletFile :: FilePath -> JHamletOpts -> Q Exp
jhamletFile fp opts = do
#ifdef GHC_7_4
    qAddDependentFile fp
#endif
    contents <- fmap TL.unpack $ qRunIO $ readUtf8File fp
    jhamletFromString opts contents

wrapFunction :: JHamletOpts -> Q Exp -> Q Exp
wrapFunction opts js = [|"function " ++ functionName ++ "(" ++ contextName ++ ") {" ++ $js ++ "}"|]
  where functionName = jhamletFunctionName opts
        contextName = jhamletContextName opts

docsToExp :: JHamletOpts -> [Doc] -> Q Exp
docsToExp opts docs = do
  exps <- mapM (docToExp opts) docs
  case exps of
    [] -> [|return ()|]
    [x] -> return x
    _ -> [|mconcat $(return $ ListE exps)|]

docToExp :: JHamletOpts -> Doc -> Q Exp
docToExp opts (DocContent c) = contentToExp opts c
docToExp opts (DocCond conds final) = do
    conds' <- mapM go conds
    final' <- maybe [|mempty|] (docsToExp opts) final
    mc <- [|mconcat|]
    -- TODO actually evaluate and test the conditions!
    return $ mc `AppE` ListE (conds' ++ [final'])
  where
    go :: (Deref, [Doc]) -> Q Exp
    go (_, docs) = docsToExp opts docs -- TODO don't ignore derefs!
docToExp opts (DocMaybe _ _ inside mno) = do
    inside' <- docsToExp opts inside
    ninside' <- case mno of
      Nothing -> [|mempty|]
      Just no -> docsToExp opts no
    ma <- [|mappend|]
    return $ ma `AppE` inside' `AppE` ninside'
docToExp opts (DocForall _ _ inside) = [|mconcat $ replicate 5 $inside'|]
  where inside' = docsToExp opts inside

contentToExp :: JHamletOpts -> Content -> Q Exp
contentToExp _ (ContentRaw s) = [|jsWriteLit s|]
contentToExp opts (ContentVar d) = [|jsWrite $(derefToExp opts d)|]

derefToExp :: JHamletOpts -> Deref -> Q Exp
derefToExp opts (DerefIdent (Ident i)) = [|JsIdent $ contextName ++ "." ++ i|]
  where contextName = jhamletContextName opts
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
