{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Hamlet.Javascript where

import Data.List (intercalate)
import Text.Hamlet.Parse
import Text.Shakespeare.Base (Deref(..), Ident(..), readUtf8File)
import qualified Data.Text.Lazy as TL
import Data.Monoid (mappend, mempty, mconcat)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax


data JHamletOpts = JHamletOpts {
    jhamletFunctionName :: JsIdent
  , jhamletContextName :: JsIdent
  , jhamletHelpersName :: JsIdent
  }

defaultJHamletOpts :: JHamletOpts
defaultJHamletOpts = JHamletOpts (JsIdent "render") (JsIdent "context") (JsIdent "helpers")


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
wrapFunction opts js = [|"function " ++ functionName ++ "(" ++ intercalate ", " params ++ ") {" ++ $js ++ "}"|]
  where functionName = renderJs $ jhamletFunctionName opts
        params = map renderJs [jhamletContextName opts, jhamletHelpersName opts]

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
derefToExp opts (DerefIdent (Ident i)) = [|JsExpDot contextName (JsIdent i)|]
  where contextName = JsExpIdent $ jhamletContextName opts
derefToExp _ (DerefIntegral i) = [|JsExpLit $ toJsLit i|]
derefToExp _ (DerefString s) = [|JsExpLit $ toJsLit s|]


data JsLit = JsString { unJsString :: String }
           | JsNum { unJsNum :: Float }
newtype JsIdent = JsIdent { unJsIdent :: String }
data JsExp =
    JsExpLit JsLit
  | JsExpIdent JsIdent
  | JsExpDot { jsDotContext :: JsExp, jsDotName :: JsIdent }

class Js j where
  renderJs :: j -> String

instance Js JsLit where
  renderJs (JsString s) = jsQuote $ escapearoo s
  renderJs (JsNum n) = show n

instance Js JsIdent where
  renderJs (JsIdent i) = i

instance Js JsExp where
  renderJs (JsExpLit lit) = renderJs lit
  renderJs (JsExpIdent ident) = renderJs ident
  renderJs (JsExpDot context name) = renderJs context ++ "." ++ renderJs name


instance Lift JsIdent where
  lift (JsIdent i) = [|JsIdent i|]

instance Lift JsLit where
  lift (JsString s) = [|JsString s|]
  lift (JsNum n) = [|JsNum $ fromIntegral n'|]
    where n' = floor n :: Integer -- TODO don't truncate all numbers!

instance Lift JsExp where
  lift (JsExpLit lit) = [|JsExpLit lit|]
  lift (JsExpIdent ident) = [|JsExpIdent ident|]
  lift (JsExpDot context name) = [|JsExpDot context name|]


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
