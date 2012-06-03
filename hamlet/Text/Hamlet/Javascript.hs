{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Text.Hamlet.Javascript where

import Text.Hamlet.Parse
import Text.Shakespeare.Base
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
contentToExp (ContentRaw s) = [|"document.write(\"" ++ escapearoo s ++ "\");"|]
contentToExp (ContentVar d) = derefToJavascript d

derefToJavascript :: Deref -> Q Exp
derefToJavascript (DerefIntegral i) = [|"document.write(" ++ show i ++ ");"|]
derefToJavascript (DerefString s) = [|"document.write(\"" ++ escapearoo s ++ "\");"|]


-- TODO probably shouldn't write my own escaping function
-- TODO and it probably should use a typeclass
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
